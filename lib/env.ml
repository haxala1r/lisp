open Ast
(* the type `environment` is defined in Ast *)

let default_env: environment = [Hashtbl.create 1024];;

let copy (env : environment) : environment =
  List.map Hashtbl.copy env

let make_env () = copy default_env

let new_lexical (env : environment) : environment = 
  let h = Hashtbl.create 16 in
  h :: env

let set_local (env : environment) (s : string) (v : lisp_val) : unit =
  match env with
  | [] -> ()
  | e1 :: _ -> Hashtbl.replace e1 s v

let rec update (env : environment) s v =
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

let set_global (env : environment) s v =
  Hashtbl.replace (get_root env) s v

let add_builtin s f =
  set_global default_env s (LBuiltinFunction (s, f))
let add_special s f =
  set_global default_env s (LBuiltinSpecial (s, f))
