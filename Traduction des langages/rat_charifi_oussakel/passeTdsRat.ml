(* Module de la passe de gestion des identifiants *)
(* doit être conforme à l'interface Passe *)
module PasseTdsRat : Passe.Passe with type t1 = Ast.AstSyntax.programme and type t2 = Ast.AstTds.programme =
struct

open Tds
open Type
open Exceptions
open Ast
open AstTds

type t1 = Ast.AstSyntax.programme
type t2 = Ast.AstTds.programme

(* analyse_tds_affectable: tds -> AstSyntax.affectable -> AstTds.affectable *)
(* Paramètre tds : la table des symboles courante *)
(* Paramètre a : l'affectable à analyser *)
(* Paramètre modif : boolean indique si l'affectable est modifié *)
(* Vérifie la bonne utilisation des affectables et tranforme l'affectable
en une affectable de type AstTds.affectable *)
(* Erreur si mauvaise utilisation des identifiants *)
(* Erreur si l'identifiant n'est pas déclaré *)
let rec analyse_tds_affectable tds (a:AstSyntax.affectable) modif : affectable =
  match a with
    | AstSyntax.Ident n ->
    begin
      match chercherGlobalement tds n with
        | None -> raise (IdentifiantNonDeclare n)
        | Some info ->
        begin
          match info_ast_to_info info with
            |InfoFun _  -> raise (MauvaiseUtilisationIdentifiant n)
            |InfoLoop _ -> AstTds.Ident info
            |InfoVar _-> AstTds.Ident info
            |InfoConst _ -> if modif then raise (MauvaiseUtilisationIdentifiant n) else (AstTds.Ident info)
        end
      end
      | AstSyntax.Valeur(v) -> AstTds.Valeur(analyse_tds_affectable tds v modif)

(* analyse_tds_expression : tds -> AstSyntax.expression -> AstTds.expression *)
(* Paramètre tds : la table des symboles courante *)
(* Paramètre e : l'expression à analyser *)
(* Vérifie la bonne utilisation des identifiants et tranforme l'expression
en une expression de type AstTds.expression *)
(* Erreur si mauvaise utilisation des identifiants *)
let rec analyse_tds_expression tds e = match e with
  (* Appel de fonction représenté par le nom de la fonction et la liste des paramètres réels *)
  | AstSyntax.AppelFonction (id, l) ->( match (chercherGlobalement tds id) with
    |None -> raise (IdentifiantNonDeclare id)
    |Some a -> (match (info_ast_to_info a) with
              |InfoFun _ -> (let nl =  List.map (fun e -> analyse_tds_expression tds e) l in
                        (AstTds.AppelFonction( a, nl)))
              |_ -> raise (MauvaiseUtilisationIdentifiant id) ))
  (* Booléen *)
  | AstSyntax.Booleen b-> AstTds.Booleen b
  (* Opération unaire représentée par l'opérateur et l'opérande *)
  | AstSyntax.Unaire (u, e1) -> (AstTds.Unaire(u, (analyse_tds_expression tds e1)))
  (* Opération binaire représentée par l'opérateur, l'opérande gauche et l'opérande droite *)
  | AstSyntax.Binaire (operateur,exp1,exp2) -> 
      (let nexp1 = analyse_tds_expression tds exp1 in
      let nexp2 = analyse_tds_expression tds exp2 in
      AstTds.Binaire(operateur,nexp1,nexp2))
  (*Affectable*)    
  | AstSyntax.Affectable aff -> AstTds.Affectable (analyse_tds_affectable tds aff false)
  (*Null*)
  | AstSyntax.Null -> AstTds.Null 
  (*New*)
  | AstSyntax.New t -> AstTds.New t
  (*Adresse*)
  | AstSyntax.Adresse n ->(begin 
    (*On cherche l'identifiant dans la tds*)
    match chercherGlobalement tds n with 
      (*On lève une exception si l'identifiant n'est pas déclaré*)
    | None -> raise(IdentifiantNonDeclare n)
    | Some info -> begin 
      match info_ast_to_info info with 
        | InfoVar _ -> AstTds.Adresse info
        | _ -> raise(MauvaiseUtilisationIdentifiant n)
      end 
    end )
  (*Opérateur ternaire*)
  |AstSyntax.Ternaire (e1,e2,e3) ->
    (*on analyse chaque expression donnée en paramètre de notre opérateur*)
     (let ne1 = analyse_tds_expression tds e1 in 
     let ne2 = analyse_tds_expression tds e2 in 
     let ne3 = analyse_tds_expression tds e3 in
      AstTds.Ternaire (ne1,ne2,ne3))
  (*Entier*)
  |AstSyntax.Entier n -> AstTds.Entier n




(* analyse_tds_instruction : tds -> info_ast option -> AstSyntax.instruction -> AstTds.instruction *)
(* Paramètre tds : la table des symboles courante *)
(* Paramètre oia : None si l'instruction i est dans le bloc principal,
                   Some ia où ia est l'information associée à la fonction dans laquelle est l'instruction i sinon *)
(* Paramètre i : l'instruction à analyser *)
(* Vérifie la bonne utilisation des identifiants et tranforme l'instruction
en une instruction de type AstTds.instruction *)
(* Erreur si mauvaise utilisation des identifiants *)
let rec analyse_tds_instruction tds oia i =
  match i with
  | AstSyntax.Declaration (t, n, e) ->
      begin
        match chercherLocalement tds n with
        | None ->
            (* L'identifiant n'est pas trouvé dans la tds locale,
            il n'a donc pas été déclaré dans le bloc courant *)
            (* Vérification de la bonne utilisation des identifiants dans l'expression *)
            (* et obtention de l'expression transformée *)
            let ne = analyse_tds_expression tds e in
            (* Création de l'information associée à l'identfiant *)
            let info = InfoVar (n,Undefined, 0, "") in
            (* Création du pointeur sur l'information *)
            let ia = info_to_info_ast info in
            (* Ajout de l'information (pointeur) dans la tds *)
            ajouter tds n ia;
            (* Renvoie de la nouvelle déclaration où le nom a été remplacé par l'information
            et l'expression remplacée par l'expression issue de l'analyse *)
            AstTds.Declaration (t, ia, ne)
          | Some a-> match info_ast_to_info a with
              (* L'identifiant est trouvé dans la tds locale,
               mais il refère à l'identifiant d'une boucle*)
                   |InfoLoop _ -> let ne = analyse_tds_expression tds e in
                   let info = InfoVar (n,Undefined, 0, "") in
                   let ia = info_to_info_ast info in
                  ajouter tds n ia; AstTds.Declaration (t, ia, ne)
            (* L'identifiant est trouvé dans la tds locale,
            il a donc déjà été déclaré dans le bloc courant *)
            |_-> raise (DoubleDeclaration n)
      end
  | AstSyntax.Affectation (n,e) -> AstTds.Affectation (analyse_tds_affectable tds n true, analyse_tds_expression tds e)
      
  | AstSyntax.Constante (n,v) ->
      begin
        match chercherLocalement tds n with
        | None ->
          (* L'identifiant n'est pas trouvé dans la tds locale,
             il n'a donc pas été déclaré dans le bloc courant *)
          (* Ajout dans la tds de la constante *)
          ajouter tds n (info_to_info_ast (InfoConst (n,v)));
          (* Suppression du noeud de déclaration des constantes devenu inutile *)
          AstTds.Empty
        | Some _ ->
          (* L'identifiant est trouvé dans la tds locale,
          il a donc déjà été déclaré dans le bloc courant *)
          raise (DoubleDeclaration n)
      end
  | AstSyntax.Affichage e ->
      (* Vérification de la bonne utilisation des identifiants dans l'expression *)
      (* et obtention de l'expression transformée *)
      let ne = analyse_tds_expression tds e in
      (* Renvoie du nouvel affichage où l'expression remplacée par l'expression issue de l'analyse *)
      AstTds.Affichage (ne)
  | AstSyntax.Conditionnelle (c,t,e) ->
      (* Analyse de la condition *)
      let nc = analyse_tds_expression tds c in
      (* Analyse du bloc then *)
      let tast = analyse_tds_bloc tds oia t in
      (* Analyse du bloc else *)
      let east = analyse_tds_bloc tds oia e in
      (* Renvoie la nouvelle structure de la conditionnelle *)
      AstTds.Conditionnelle (nc, tast, east)
  | AstSyntax.TantQue (c,b) ->
      (* Analyse de la condition *)
      let nc = analyse_tds_expression tds c in
      (* Analyse du bloc *)
      let bast = analyse_tds_bloc tds oia b in
      (* Renvoie la nouvelle structure de la boucle *)
      AstTds.TantQue (nc, bast)
  | AstSyntax.Retour (e) ->
      begin
      (* On récupère l'information associée à la fonction à laquelle le return est associée *)
      match oia with
        (* Il n'y a pas d'information -> l'instruction est dans le bloc principal : erreur *)
      | None -> raise RetourDansMain
        (* Il y a une information -> l'instruction est dans une fonction *)
      | Some ia ->
        (* Analyse de l'expression *)
        let ne = analyse_tds_expression tds e in
        AstTds.Retour (ne,ia)
      end
  |AstSyntax.Loop (n,b) ->
      (*Construction de la loop associée à l'identifiant n*)
      let is = InfoLoop(n,"","") in
      (*Ajout de la loop dans la tds*)
      (ajouter tds n (info_to_info_ast is));
      (*on cherche localement l'info associé à n*)
      let na = chercherLocalement tds n in 
      (*analyse du bloc de la loop*)
      let nb = analyse_tds_bloc tds na b in
      AstTds.Loop ((info_to_info_ast is),nb)

  |AstSyntax.Continue n ->
    (*On cherche la loop associée à l'identifiant donnée à l'instruction*)
    let info = chercherGlobalement tds n in
    begin
        match info with
            (*si pas de loop trouvée, on lève l'exception que la boucle n'est pas connue*)
            |None ->
                raise (Loopunknown n)
            (*Le cas où l'identifiant est trouvé, on vérifie si c'est bien l'identifiant d'une boucle*)    
            |Some a ->
                begin
                    match (info_ast_to_info a) with
                        |InfoLoop _ ->
                            AstTds.Continue a
                        | _ ->
                            raise (WrongContinue n)
                end
    end
    |AstSyntax.Break n ->
      (*On cherche la loop associée à l'identifiant donnée à l'instruction*)
      let info = chercherGlobalement tds n in
      begin
          match info with
              (*si pas de loop trouvée, on lève l'exception que la boucle n'est pas connue*)
              |None ->
                  raise (Loopunknown n)
              |Some a ->
                  begin
                     (*Le cas où l'identifiant est trouvé, on vérifie si c'est bien l'identifiant d'une boucle*)   
                      match (info_ast_to_info a) with
                          |InfoLoop _ ->
                              AstTds.Break a
                          | _ ->
                              raise (WrongBreak n)
                  end
      end


(* analyse_tds_bloc : tds -> info_ast option -> AstSyntax.bloc -> AstTds.bloc *)
(* Paramètre tds : la table des symboles courante *)
(* Paramètre oia : None si le bloc li est dans le programme principal,
                   Some ia où ia est l'information associée à la fonction dans laquelle est le bloc li sinon *)
(* Paramètre li : liste d'instructions à analyser *)
(* Vérifie la bonne utilisation des identifiants et tranforme le bloc en un bloc de type AstTds.bloc *)
(* Erreur si mauvaise utilisation des identifiants *)
and analyse_tds_bloc tds oia li =
  (* Entrée dans un nouveau bloc, donc création d'une nouvelle tds locale
  pointant sur la table du bloc parent *)
  let tdsbloc = creerTDSFille tds in
  (* Analyse des instructions du bloc avec la tds du nouveau bloc.
     Cette tds est modifiée par effet de bord *)
   let nli = List.map (analyse_tds_instruction tdsbloc oia) li in
   (* afficher_locale tdsbloc ; *) (* décommenter pour afficher la table locale *)
   nli


(* analyse_tds_fonction : tds -> AstSyntax.fonction -> AstTds.fonction *)
(* Paramètre tds : la table des symboles courante *)
(* Paramètre : la fonction à analyser *)
(* Vérifie la bonne utilisation des identifiants et tranforme la fonction
en une fonction de type AstTds.fonction *)
(* Erreur si mauvaise utilisation des identifiants *)
let analyse_tds_fonction maintds (AstSyntax.Fonction(t,n,lp,li))  =
match chercherLocalement maintds n with
| None -> let info = InfoFun(n,Undefined,[]) in
  let info_fun = (info_to_info_ast info) in
    ajouter maintds n info_fun;
    let tdsfille = creerTDSFille maintds in
      let m = List.map (fun (x,y) -> 
        let infovar = InfoVar(y,Undefined,0,"") in 
        begin
          match chercherLocalement tdsfille y with
          | None -> 
          let info = info_to_info_ast infovar in
          ajouter tdsfille y info;
          (x,info)
          |Some _ -> raise (DoubleDeclaration y)
          end) lp in
        Ast.AstTds.Fonction (t,info_fun,m,(analyse_tds_bloc tdsfille (Some info_fun) li))
| Some _-> raise (DoubleDeclaration n)

(* analyser : AstSyntax.programme -> AstTds.programme *)
(* Paramètre : le programme à analyser *)
(* Vérifie la bonne utilisation des identifiants et tranforme le programme
en un programme de type AstTds.programme *)
(* Erreur si mauvaise utilisation des identifiants *)
let analyser (AstSyntax.Programme (fonctions,prog)) =
  let tds = creerTDSMere () in
  let nf = List.map (analyse_tds_fonction tds) fonctions in
  let nb = analyse_tds_bloc tds None prog in
  AstTds.Programme (nf,nb)

end