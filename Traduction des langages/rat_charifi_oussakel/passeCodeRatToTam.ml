(* Module de la passe code to TAM *)
module PasseCodeRatToTam : Passe.Passe with type t1 = Ast.AstPlacement.programme  and type t2 = string =
struct

open Tds
open Exceptions
open Ast
open AstType
open Type
open AstPlacement
open Code
open Tam

type t1 = Ast.AstPlacement.programme
type t2 = string

(*Analyse de l'affectable 
   modif : boolean = false si c'est pas une expression modifiable||  true si une expression ne l'est pas
   a :  type affectable*)
let rec analyse_code_affectable_aux modif a =
  match a with 
  |AstType.Ident id -> begin
      match info_ast_to_info id with 
      | InfoVar(_,t,dep,reg) -> (if modif then 
        let taille = (getTaille t) in  (store taille dep reg ,t) 
        else
          let taille = (getTaille t) in  (load taille dep reg ,t) )
      | InfoConst(_,v) -> (loadl_int v,Int)
      | _ -> failwith"error"
      end
  |AstType.Valeur(v)-> let (n,t) = analyse_code_affectable_aux false v in 
      begin 
        match t with
        |Pointeur ta -> (let taille = (getTaille t) in
                        if modif then 
                          (n^storei taille,t)
                        else
                        (n ^ loadi taille,t) )
        |_ -> failwith "error"
      end
(*Pour extraire le code tam*)
 let analyse_code_affectable a modif= 
  let (n,_) = analyse_code_affectable_aux modif a in n 

(* analyse_code_expression : expression -> string *)
(* Produit le code correspondant à l'expression. L'execution de ce code 
   laissera en sommet de pile le résultat de l'évaluation de cette expression *)
let rec analyse_code_expression e =
  match e with
  (*Appel de fonction*)
  | AstType.AppelFonction (id, l) ->begin
                                        match info_ast_to_info id with
                                        | InfoFun(n,_,_) -> String.concat "" (List.map analyse_code_expression l) ^ call "SB" n
                                        | _ -> failwith "Error"
                                    end
  (* Booléen *)
  | AstType.Booleen b-> if b then loadl_int 1
                             else loadl_int 0
  (*Entier*)
  |AstType.Entier n -> loadl_int n
  (* Opération unaire représentée par l'opérateur et l'opérande *)
  | AstType.Unaire (u, e) -> analyse_code_expression e
                                  ^ (match u with
                                    | AstType.Numerateur -> pop 0 1
                                    | AstType.Denominateur -> pop 1 1)   
  (* Opération binaire représentée par l'opérateur, l'opérande gauche et l'opérande droite *)
  | AstType.Binaire (op,exp1,exp2) ->  analyse_code_expression exp1
                                                  ^ analyse_code_expression exp2
                                                  ^ (match op with
                                                    | AstType.MultInt -> subr "IMul"
                                                    | AstType.MultRat -> call "SB" "RMul"
                                                    | AstType.PlusInt -> subr "IAdd"
                                                    | AstType.PlusRat -> call "SB" "RAdd"
                                                    | AstType.EquBool -> subr "IEq"
                                                    | AstType.Inf -> subr "ILss"
                                                    | AstType.EquInt -> subr "IEq"
                                                    | AstType.Fraction -> call "SB" "norm"
                                                   )       
  (*Affectable*)                                                                    
  | AstType.Affectable aff ->let n = analyse_code_affectable aff false in n
  (*Le pointeur null*)
  | AstType.Null -> loadl_int (-1)
  (*Réserver une place mémoire pour un nouveau pointeur avec Malloc*)
  | AstType.New t ->let taille = getTaille t in 
                         loadi taille ^ subr "Malloc"
  (*utilisation de LOADA int[reg] pour enregistrer l'adresse du pointeur*)                       
  | AstType.Adresse a -> begin 
                              match info_ast_to_info a with 
                                  | InfoVar(_,_,dep,reg)-> loada dep reg
                                  | _ -> failwith"error"
                          end
   (*Création des étiquettes associées à chaque bloc de la conditionnelle et génération du code de chaque bloc*)                       
  |AstType.Ternaire (c,btrue,bfalse) ->
    (let cc = analyse_code_expression c in 
                                       let cTrue = analyse_code_expression btrue in 
                                       let cFalse = analyse_code_expression bfalse in 
                                       let bElse = getEtiquette() in 
                                       let bEnd = getEtiquette() in   
                                        cc ^ jumpif 0 bElse ^ cTrue 
                                        ^ jump bEnd ^ label bElse ^ cFalse 
                                        ^ label bEnd
                                       )  
(* analyse_code_instruction: instruction -> string *)
(* Produit le code correspondant à l'instruction. L'execution de ce code 
   laissera en sommet de pile le résultat de l'évaluation de cet instruction *)
and analyse_code_instruction i =
  match i with
  | AstPlacement.Declaration(info, e) -> 
    begin
      match info_ast_to_info info with
      | InfoVar (_,t,dep,reg) -> (let ce = analyse_code_expression e in
                             let taille = getTaille t in 
                             push taille ^ ce ^ store taille dep reg )
      | _ -> failwith "error"
      end
  | AstPlacement.Affectation(a,e)-> (
        let cE = analyse_code_expression e in 
        let cA = analyse_code_affectable a true in 
        cE^cA  )  
  |AstPlacement.Conditionnelle(c,bt,bf)  -> begin
    match bf with
    |[],0 ->(let cc = analyse_code_expression c in 
    let cTrue = analyse_code_bloc bt in 
    let bEnd = getEtiquette() in   
     cc ^ jumpif 0 bEnd ^ cTrue ^ label bEnd)
    |_,_ ->
     (let cc = analyse_code_expression c in 
                                       let cTrue = analyse_code_bloc bt in 
                                       let cFalse = analyse_code_bloc bf in 
                                       let bElse = getEtiquette() in 
                                       let bEnd = getEtiquette() in   
                                        cc ^ jumpif 0 bElse ^ cTrue 
                                        ^ jump bEnd ^ label bElse ^ cFalse 
                                        ^ label bEnd
                                       ) 
    end
  | AstPlacement.AffichageBool e -> (analyse_code_expression e) ^ subr "BOut"
  | AstPlacement.AffichageInt e -> (analyse_code_expression e) ^ subr "IOut"
  | AstPlacement.AffichageRat e -> (analyse_code_expression e) ^ call "SB" "ROut" 
  | AstPlacement.TantQue(c, b) ->  begin
                              let cc = analyse_code_expression c in 
                              let cb = analyse_code_bloc b in 
                              let debutTQ = getEtiquette() in 
                              let finTQ = getEtiquette() in 
                              label debutTQ ^cc^ (jumpif 0 finTQ) ^ cb ^ jump debutTQ ^ label finTQ 
                              end
   |AstPlacement.Retour(e,tret,tparam) ->(if ( tret==0) then failwith "error"
                                         else
                                          (analyse_code_expression e 
                                                        ^ return tret tparam ))
   | AstPlacement.Empty -> ""
   | AstPlacement.Loop (a,bloc) ->(let debutloop = getEtiquette() in
                                  let finloop = getEtiquette() in
                                  debutloop^(analyse_code_bloc bloc)^(jump debutloop)^finloop)
   | AstPlacement.Break a ->(match info_ast_to_info a with
                            |InfoLoop(_,_,finloop)-> jump finloop
                            |_ -> failwith "error")   
   |AstPlacement.Continue a ->(match info_ast_to_info a with
                              |InfoLoop(_,debutloop,_) -> jump debutloop
                              |_ -> failwith "error")  


  (* analyse_code_bloc : instruction list * int -> string
     application de analyse_instruction sur tout le bloc
     li : instruction list du bloc
     taille : int taille du bloc*)
   and analyse_code_bloc (li,taille) = push taille
                                      ^ String.concat "" (List.map (analyse_code_instruction) li)
                                      ^ pop 0 taille

  (* analyse_code_fonction : AstPlacement.fonction -> string *)
  let analyse_code_fonction (Fonction(info, _, (li,taille))) = 
  match info_ast_to_info info with 
  |InfoFun(n, t, list_param) -> ( 
   let anal_bloc = analyse_code_bloc (li,taille) in
    n^"\n"^anal_bloc  )         
  | _ -> failwith ""
  
  let analyser (Programme(fonctions, prog)) =
    let fonc =  String.concat "" (List.map analyse_code_fonction fonctions) in
    let main = label "main" ^ (analyse_code_bloc prog) ^ halt in
     getEntete()^fonc ^ main
end