
let ( let* ) = Result.bind
let traverse = Compiler.Util.traverse

type runtime_value =
  | Int of int
  | Double of float
  | String of string
  | Nil
  | Cons of runtime_value * runtime_value
  | Symbol of string
(* The rest can't appear as literal values, and are constructed in other ways *)
  | Closure of Compiler.Scope_analysis.expression * (runtime_value ref list)

let rec interpret_literal = function
  | Compiler.Core_ast.Int x -> Ok (Int x)
  | Double x -> Ok (Double x)
  | String s -> Ok (String s)
  | Cons (a, b) ->
     let* a = interpret_literal a in
     let* b = interpret_literal b in
     Ok (Cons (a, b))
  | Nil -> Ok (Nil)
    

let rec interpret_one expr env globals =
  match expr with
  | Compiler.Scope_analysis.Literal l -> interpret_literal l
  | Local i ->
     (match (List.nth_opt env i) with
      | None -> Error "Error while accessing local variable!"
      | Some x -> Ok !x)
  | Global i ->
     Ok (Array.get globals i)
  | Apply (f, e) ->
     let* f = interpret_one f env globals in
     let* e = interpret_one e env globals in
     (match f with
      | Closure (body, inner_env) ->
         let f_env = (ref e) :: inner_env in
         interpret_one body f_env globals
      | _ -> Error "Cannot apply an argument to non-closure value!")
  | Lambda body ->
     Ok (Closure (body, env))
  | If (test, then_e, else_e) ->
     let* test = interpret_one test env globals in
     (match test with
      | Nil -> interpret_one else_e env globals
      | _ -> interpret_one then_e env globals)
  | SetLocal (i, e) ->
     (match (List.nth_opt env i) with
      | None -> Error "Error while setting local variable!"
      | Some r ->
         let* e = interpret_one e env globals in
         r := e; Ok e)
  | SetGlobal (i, e) ->
     let* e = interpret_one e env globals in
     Array.set globals i e; Ok e
  | Begin [] -> Ok Nil
  | Begin [e] -> interpret_one e env globals
  | Begin (e :: rest) ->
     let* e = interpret_one e env globals in
     ignore e; interpret_one (Begin rest) env globals

let interpret program global_syms =
  let count = Compiler.Scope_analysis.SymbolTable.cardinal global_syms in
  let globals : runtime_value array = Array.make count Nil in
  
  Ok (interpret_one (Begin program) [] globals)



let interpret_src src =
  let* (program, globals) = Compiler.Scope_analysis.of_src src in
  interpret program globals

