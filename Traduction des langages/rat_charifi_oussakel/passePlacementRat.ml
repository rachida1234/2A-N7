
module PassePlacementRat : Passe.Passe with type t1 = Ast.AstType.programme and type t2 = Ast.AstPlacement.programme =
struct

  open Tds
  open Ast
  open AstPlacement
  open Type


  type t1 = Ast.AstType.programme
  type t2 = Ast.AstPlacement.programme

  (*retourne le type*)
  (*paramètre ia : info ast*) 
  let getType ia =  
    match info_ast_to_info ia with
    | InfoVar (_,t,_,_) -> t
    | _ -> failwith "err"

(* analyse_placement_instruction : instruction-> int -> int ->(AstPlacement.instruction,int)*)
let rec analyse_placement_instruction i reg depl =
        match i with
            
            |AstType.Declaration(info_ast, e) ->
                (* Placement en mémoire de la déclaration dans le registre reg *)
                modifier_adresse_variable depl reg info_ast;
                (* Renvoi la nouvelle déclaration avec la taille du type associé *)
                (AstPlacement.Declaration (info_ast, e), getTaille (getType info_ast))
            |AstType.Affectation (a, e) ->
                (* Renvoie la nouvelle affectation avec taille nulle (pas d'impact sur l'occupation du registre) *)
                (AstPlacement.Affectation (a, e), 0)
            |AstType.AffichageInt (e) ->
                (* Renvoie le nouvel AffichageInt avec une taille nulle *)
                (AstPlacement.AffichageInt (e),0)
            |AstType.AffichageBool (e) ->
                (* Renvoie le nouvel AffichageBool avec une taille nulle *)
                (AstPlacement.AffichageBool (e),0)
            |AstType.AffichageRat (e) ->
                (* Renvoie le nouvel AffichageRat avec une taille nulle *)
                (AstPlacement.AffichageRat (e),0)
            |AstType.Conditionnelle (e, bt, be) ->
                let nbt = analyse_placement_bloc bt depl reg in
                let nbe = analyse_placement_bloc be depl reg in
                    (* Renvoie la nouvelle Conditionnelle avec une taille nulle *)
                    (AstPlacement.Conditionnelle (e, nbt, nbe), 0)
            |AstType.TantQue (e, b) ->
                let nb = analyse_placement_bloc b depl reg in
                    (* Renvoie la nouvelle boucle avec une taille nulle *)
                    (AstPlacement.TantQue (e, nb), 0)
            |AstType.Empty ->
                    (* Renvoie du nouveau vide *)
                   ( AstPlacement.Empty, 0)
            |AstType.Retour (e, iast) -> 
                begin
                    match (info_ast_to_info iast) with
                        |InfoFun (_,t,tp) -> 
                            let size_return = getTaille t in
                            let size_param = List.fold_right (fun t acc -> acc + getTaille t) tp 0 in
                                AstPlacement.Retour (e, size_return, size_param), 0
                        (* Erreur si retour non appelé depuis une fonction *)
                        |_ -> failwith "error" 
                end
              |AstType.Loop (n, b) ->
                  let nb = analyse_placement_bloc b depl reg in
                     ( AstPlacement.Loop(n, nb), 0)
              |AstType.Break n ->
                  (AstPlacement.Break n, 0)
              |AstType.Continue n ->
                 ( AstPlacement.Continue n, 0)

(* analyse_placement_bloc :  list instructions-> int -> int ->(AstPlacement.bloc,int)
paramètre li : liste d'instruction à analyser
paramètre base : position par rapport au registre
paramètre reg : registre *)           
and analyse_placement_bloc li base reg =
 match li with
  | [] -> ([],0)
  | t::q -> 
    let (nt,nplac) = (analyse_placement_instruction t reg base)in
    let (nq,placq) =  (analyse_placement_bloc q (nplac + base) reg) in
     (nt::nq, nplac+placq)

  

(* analyse_placement_fonction : AstType.fonction -> AstPlacement.fonction *)
let analyse_placement_fonction(AstType.Fonction(info,l,bloc)) = 
let rec analyse_placement_param li depl =
  match li with
  | [] -> ()
  | t::q -> 
    match (info_ast_to_info t) with
    | InfoVar(_,typ,_,_) -> 
      modifier_adresse_info (depl-(getTaille typ)) "LB" t;
      (analyse_placement_param q (depl-(getTaille typ)))
    | _ -> failwith "Internal error"
in
analyse_placement_param (List.rev l) 0 ;
let nb = (analyse_placement_bloc bloc 3 "LB") in
AstPlacement.Fonction(info,l,nb)

(* AstType.programme -> AstPlacement.programme *)
let analyser (AstType.Programme (lf,bloc)) =
  let fonctions = List.map (analyse_placement_fonction) lf in
  let blocs = analyse_placement_bloc bloc 0 "SB"  in
  Programme (fonctions, blocs)
end