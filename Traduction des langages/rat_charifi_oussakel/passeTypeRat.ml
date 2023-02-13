(* Module de la passe de typage *)
module PasseTypeRat : Passe.Passe with type t1 = Ast.AstTds.programme and type t2 = Ast.AstType.programme =
struct

open Tds
open Exceptions
open Ast
open AstType
open Type

type t1 = Ast.AstTds.programme
type t2 = Ast.AstType.programme

(* analyse_type_affectable : AstTds.affectable -> AstType.affectable *)
(* Paramètre a : l'affectable à analyser *)
(* Vérifie la bonne utilisation des types et tranforme l'affectable
en un affectable de type AstType.affectable *)
(* Erreur si mauvaise utilisation des identifiants *)
let rec analyse_type_affectable a = 
    match a with
    | AstTds.Ident infoast -> 
    begin
      match (info_ast_to_info infoast) with
      | InfoConst _ -> (Ident(infoast), Int)
      | InfoVar(_,t,_,_) -> modifier_type_variable t infoast; (Ident(infoast), t)
      | _ -> failwith ("Internal error")
    end
    |AstTds.Valeur a1 -> (
      let na,t = analyse_type_affectable a1 in
      begin
          match t with
              |Pointeur nt ->  
                  (AstType.Valeur(na),nt)
              |_ -> 
                  raise (TypeInattendu (t,Pointeur Undefined))
      end)


(* analyse_type_expression : AstTds.expression -> AstType.expression *)
(* Paramètre e : l'expression à analyser *)
(* Vérifie la bonne utilisation des identifiants et tranforme l'expression
en une expression de type AstTds.expression *)
(* Erreur si mauvaise utilisation des identifiants *)
let rec analyse_type_expression e = match e with
  (* Appel de fonction représenté par le nom de la fonction et la liste des paramètres réels *)
  | AstTds.AppelFonction (id, l) ->(match info_ast_to_info id with
  |InfoFun(_,tR,ll)-> let lsplit  = List.split (List.map  analyse_type_expression l) in
                     let ln = fst lsplit in 
                     let lt = snd lsplit in
                     begin
                      match (est_compatible_list lt ll) with
                      |false -> raise (TypesParametresInattendus(lt,ll))
                      |true ->(AstType.AppelFonction(id,ln),tR)
                     end
  |_ -> failwith"Error")
  (* Booléen *)
  | AstTds.Booleen b-> (AstType.Booleen b,Bool)
  (* Opération unaire représentée par l'opérateur et l'opérande *)
  | AstTds.Unaire (u, e1) ->
    (   let (ne, te) = analyse_type_expression e1 in
    if (est_compatible te Rat) then
      let (tr, np) =
      begin
        match u with
        | AstSyntax.Numerateur -> (AstType.Numerateur, Int)
        | AstSyntax.Denominateur -> (AstType.Denominateur,Int)
      end
      in (AstType.Unaire(tr, ne), np)
    else raise (TypeInattendu(te, Rat)))
  (*Entier*)
  |AstTds.Entier n -> (AstType.Entier n, Int)
  (* Opération binaire représentée par l'opérateur, l'opérande gauche et l'opérande droite *)
  | AstTds.Binaire (operateur,exp1,exp2) ->( let (ne1,te1) =  analyse_type_expression exp1 in
                                            let (ne2,te2) =  analyse_type_expression exp2 in
                                            (*On vérifie si les deux opérandes sont de meme type*)
                                            if est_compatible te1 te2 then
                                              begin
                                                ( match operateur with
                                                |Plus -> (match te1, te2 with
                                                         |Int,Int ->(AstType.Binaire(PlusInt, ne1 , ne2), Int)
                                                         |Rat,Rat ->(AstType.Binaire(PlusRat, ne1 , ne2), Rat)
                                                         |_ -> raise (TypeBinaireInattendu(Plus,te1,te2)))
                                                |Mult -> (match te1, te2 with
                                                         |Int,Int ->(AstType.Binaire(MultInt, ne1 , ne2), Int)
                                                         |Rat,Rat ->(AstType.Binaire(MultRat, ne1 , ne2), Rat)
                                                         |_ -> raise( TypeBinaireInattendu(Mult,te1,te2)))  
                                                |Equ -> (match te1, te2 with
                                                         |Int,Int ->(AstType.Binaire(EquInt, ne1 , ne2), Bool)
                                                         |Bool,Bool ->(AstType.Binaire(EquBool, ne1 , ne2), Bool)
                                                         |_ -> raise (TypeBinaireInattendu(Equ,te1,te2)))
                                                |Fraction -> (match te1,te2 with
                                                          |Int,Int -> (AstType.Binaire(Fraction,ne1,ne2), Rat)
                                                          |_ -> raise (TypeBinaireInattendu(Fraction,te1,te2)))
                                                |Inf -> (if est_compatible te1 Int && est_compatible te2 Int
                                                         then (Binaire(Inf,ne1,ne2),Bool)  
                                                         else raise (TypeBinaireInattendu(Inf,te1,te2)) ))
                                                  
                                                end    
                                              (*si c'est pas le cas une exception est levée*)  
                                              else raise (TypeBinaireInattendu(operateur,te1,te2)))
  (*Affectable*)
  | AstTds.Affectable aff -> (let (naff,t) = ( analyse_type_affectable aff) in
                             (Affectable naff ,t))
  (*Pointeur Null*)
  | AstTds.Null -> (Null, Pointeur Undefined)
  (*New*)
  | AstTds.New t -> (New t, Pointeur t)
  (*Adresse*)
  | AstTds.Adresse n -> begin
                        match info_ast_to_info n with
                        |InfoVar(_,t,_,_)-> (Adresse n, Pointeur t)
                        |_ -> failwith"erreur"
                        end
  (*Opérateur ternaire*)
  |AstTds.Ternaire (e1,e2,e3) ->(
      (*analyser le type de chaque expression donnée en paramètre de l'opérateur*)
			let (ne1, te1) = analyse_type_expression e1 in
			let (ne2, te2) = analyse_type_expression e2 in
			let (ne3, te3) = analyse_type_expression e3 in
         (*Vérifier si la condition est bien de type booleane*)
					if (est_compatible te1 Bool) then
              (*On vérifie si les deux sorties sont de meme type*)
							if (est_compatible te2 te3) then
								AstType.Ternaire(ne1,ne2,ne3), te2
							else 
								raise (TypeTernaireInattendus(te2, te3))
					else 
							raise (ConditionTernaireInattendus(te1,Bool)))

(* analyse_type_instruction : AstTds.instruction -> AstType.instruction *)
(* Paramètre i : l'instruction à analyser *)
(* Vérifie la bonne utilisation des identifiants et tranforme l'instruction
en une instruction de type AstTds.expression *)
(* Erreur si les types ne sont pas compatibles *)
    let rec analyse_type_instruction i =
      match i with
      | AstTds.Declaration (t, n, e) -> (let (ne,te) = (analyse_type_expression e) in
                                      (*Après l'analyse de l'expression, on vérifie si les types sont compatibles*)
                                       if (est_compatible t te) then 
                                            let _ = modifier_type_variable t n in
                                            AstType.Declaration(n,ne)
                                      else (raise (TypeInattendu(te,t))))
      | AstTds.Affectation (a,e) -> (*on analyse l'affectable et l'expression à affecter*)
                                    (let (ne,te) = analyse_type_expression e in
                                    let (na,ta) = analyse_type_affectable a in
                                    (*On vérifie la compatiblité des types*)
                                    if est_compatible te ta then 
                                       Affectation(na,ne)
                                    else (raise (TypeInattendu(te,ta))))
      | AstTds.Affichage e -> (*analyse de l'expression pour récuperer le type de e*)
                              let (ne,te) = analyse_type_expression e in
                              (*appel de Affichage compatible avec le type de l'expression*) 
                              begin 
                                match te with
                                |Int -> AffichageInt ne
                                |Bool -> AffichageBool ne
                                |Rat -> AffichageRat ne
                                |Pointeur _ -> AffichageInt ne
                                |Undefined ->  raise( TypeInattendu(te,te))
                              end
      | AstTds.Conditionnelle (c,t,e) -> (let (nc,tc) = analyse_type_expression c in
                                         (*On vérifie que la condition est bien booleane*)
                                         if est_compatible Bool tc 
                                         then (let n1bloc = analyse_type_bloc t  in 
                                               let n2bloc = analyse_type_bloc e  in 
                                               AstType.Conditionnelle(nc,n1bloc,n2bloc))
                                         else raise( TypeInattendu(tc,Bool) ) )  
      | AstTds.TantQue (c,b) -> (let (nc,tc) = analyse_type_expression c in
                                (*On vérifie que la condition d'arret est bien booleane*)
                                if est_compatible Bool tc 
                                then (let nbloc = analyse_type_bloc b in 
                                      AstType.TantQue(nc,nbloc))
                                else raise (TypeInattendu(tc,Bool) )  )  
      | AstTds.Retour (e,n) -> (
        (*Analyse du type de l'expression de retour*)
        let (ne, te) = analyse_type_expression e in
        let (t_ret, _) = 
                        (match info_ast_to_info n with
                          | InfoFun(_, t_ret, lt_param) ->
                            if (est_compatible t_ret Undefined) then failwith "Type Undefined"
                            else (t_ret, lt_param)
                          | _ -> failwith "Pas un InfoFun") 
                        in
                        if (est_compatible te t_ret) then
                          AstType.Retour(ne, n)
                        else raise (TypeInattendu(te, t_ret)))
      |AstTds.Loop (n,b) ->
        (*analyse du bloc associé à la loop*)
        let nb = analyse_type_bloc b in
        AstType.Loop(n,nb)
      (*Continue d'une loop : pas de typage à vérifier*)
      |AstTds.Continue n -> AstType.Continue n
      (*Break d'une loop : pas de typage à vérifier*)
      |AstTds.Break n -> AstType.Break n
      (*Pour la constante*)
      |AstTds.Empty  -> AstType.Empty
  
    
    
    (* analyse_tds_bloc : tds -> info_ast option -> AstSyntax.bloc -> AstTds.bloc *)
    (* Paramètre tds : la table des symboles courante *)
    (* Paramètre oia : None si le bloc li est dans le programme principal,
                       Some ia où ia est l'information associée à la fonction dans laquelle est le bloc li sinon *)
    (* Paramètre li : liste d'instructions à analyser *)
    (* Vérifie la bonne utilisation des identifiants et tranforme le bloc en un bloc de type AstTds.bloc *)
    (* Erreur si mauvaise utilisation des identifiants *)
    and analyse_type_bloc li =(List.map analyse_type_instruction li)
    
    
    (* analyse_tds_fonction : tds -> AstSyntax.fonction -> AstTds.fonction *)
    (* Paramètre tds : la table des symboles courante *)
    (* Paramètre : la fonction à analyser *)
    (* Vérifie la bonne utilisation des identifiants et tranforme la fonction
    en une fonction de type AstTds.fonction *)
    (* Erreur si mauvaise utilisation des identifiants *)
    let analyse_type_fonction (AstTds.Fonction (r, iast, lp, b)) =
      (* Définition du type de chaque paramètre de la fonction (contenu dans la liste lp)
         à l'info lui correspondant *)
      List.iter (fun (x,y) -> modifier_type_variable x y) lp;
      (* Séparation de la liste des infos et de la liste des types à partir de lp *)
      let liste_type,liste_iast = List.split lp in
          (* Définition du type de retour et des types des paramètres dans l'info de la fonction *)
          modifier_type_fonction r liste_type iast;
      (* Transformation du bloc de la fonction *)
      let nb = analyse_type_bloc b in
          (* Renvoie un AstType.Fonction contenant l'info de la fonction, la liste des infos des paramètres et le
             nouveau bloc *)
          AstType.Fonction(iast, liste_iast, nb)
   
    
    (* analyser : AstSyntax.programme -> AstTds.programme *)
    (* Paramètre : le programme à analyser *)
    (* Vérifie la bonne utilisation des identifiants et tranforme le programme
    en un programme de type AstTds.programme *)
    (* Erreur si mauvaise utilisation des identifiants *)
    let analyser (AstTds.Programme (fonctions,prog)) =
      let nf = List.map (analyse_type_fonction) fonctions in
      let nb = analyse_type_bloc prog in
      Programme (nf,nb)
    end
