open Ast
(* the type `environment` is defined in Ast *)


let new_lexical env = 
  let h = Hashtbl.create 16 in
  h :: env

let set_local env s v =
  match env with
  | [] -> ()
  | e1 :: _ -> Hashtbl.replace e1 s v

let rec update env s v =
  match env with
  | [] -> ()
  | e1 :: erest ->
     match Hashtbl.find_opt e1 s with
     | None -> update erest s v
     | Some _ -> Hashtbl.replace e1 s v

let rec get_root (env : environment) =
  match env with
  | [] -> raise (Invalid_argument "Empty environment passed to env_root!")
  | e :: [] -> e
  | _ :: t -> get_root t

let set_global env s v =
  Hashtbl.replace (get_root env) s v

let copy env =
  List.map Hashtbl.copy env


