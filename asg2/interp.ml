(* $Id: interp.ml,v 1.6 2019-01-24 19:14:14-08 - - $ *)

open Absyn

exception Unimplemented of string
let unimpl reason = raise (Unimplemented reason)

let want_dump = ref false

(* throw in a expression and evaluate it *)
let rec eval_expr (expr : Absyn.expr) : float = match expr with
  | Number number -> number
  | Unary (oper, expr) -> 
      (Hashtbl.find Tables.unary_fn_table oper) (eval_expr expr)
  | Binary(oper,expr1,expr2)->
      (Hashtbl.find Tables.binary_fn_table oper)
          (eval_expr expr1) (eval_expr expr2)
  | Memref memref -> match memref with
    | Arrayref(name, size) ->  
        Array.get (Hashtbl.find Tables.array_table name) 
            (int_of_float (eval_expr size-.1.))
                          (* print_string("get value of the array") *)
                          (* return 0 if out of bound *)
    | Variable name -> Hashtbl.find Tables.variable_table name

(* Evaluate boolean expressions for the IF statements 
   Only expressions in the form of Binary would be evaluated*)
let rec eval_bool (expr : Absyn.expr) : bool = match expr with
    | Number number -> false
    | Unary (oper, expr) -> false
    | Binary (oper, expr1, expr2) -> 
             (Hashtbl.find Tables.boolean_fn_table oper) 
             (eval_expr expr1) (eval_expr expr2)
    | Memref memref -> match memref with
        | Arrayref(name, size) -> false
        | Variable name -> false

(* given Print funtion *)
let interp_print (print_list : Absyn.printable list) =
    let print_item item =
        (print_string " ";
         match item with
         | String string -> 
           let regex = Str.regexp "\"\\(.*\\)\""
           in print_string (Str.replace_first regex "\\1" string)
         | Printexpr expr ->
           print_float (eval_expr expr);
           (* print_string "end evaluate expression" *))
    in (List.iter print_item print_list; print_newline ())

(* Fetch user input in a list, and put them in the var_table *)
let interp_input (memref_list : Absyn.memref list) =
    let input_number memref =
        match memref with
        (* pattern matching for memref *)
        | Arrayref (name, size) -> ()
        | Variable name ->
           try  let number = Etc.read_number ()
                in (
                  (* print_string "Got input \n"; print_newline ();  *)
                    Hashtbl.add Tables.variable_table name number)
                (* if eof is encountered, change its value to 1 *)
           with End_of_file -> 
                (
                  (* print_string "End_of_file"; print_newline (); *)
                 Hashtbl.add Tables.variable_table "eof" 1.)
        
    in List.iter input_number memref_list
    (* apply "input_number" to the memref_list(variable list)  *)

(* If the expression holds true, return the label
   Else, return None to proceed the program *)
let interp_if (expr : Absyn.expr) (label : Absyn.label) 
              : Absyn.program option =
  (* print_string "---Inside If--- \n";
  Printf.printf "Current label: %s\n" label;
  Printf.printf "The result of the expression is:
   %B \n" (eval_bool expr); *)
  (* Printf.printf "Inside If with label %s\n" label; *)
  if (eval_bool expr) 
     then Some(Hashtbl.find Tables.label_table label)
  else None

(* store variable/array into hashtable by pattern matching *)
let interp_let (memref : Absyn.memref) (expr : Absyn.expr) =
    match memref with
    | Variable name -> 
            (* Printf.printf "Inside Let with variable %s\n" name; *)
            Hashtbl.add Tables.variable_table name (eval_expr expr)
            (* print_string("added to variable table") *)
    | Arrayref (name, size) ->
          (* Printf.printf "Inside Let with array %s\n" name ;
          Printf.printf "Changing index %i to value %f\n" 
          (int_of_float (eval_expr size)) (eval_expr expr); *)
         Array.set (Hashtbl.find Tables.array_table name) 
         (int_of_float (eval_expr size-.1.)) (eval_expr expr)(* ; *)
         (* Printf.printf "DONE setting value\n" *)
         (* print_string("set the element to array table") *)

(* return the GOTO label from the hashtable *)
let interp_goto (label : Absyn.label) =
    (* Printf.printf "Inside Goto with label %s\n" label; *)
    Some (Hashtbl.find Tables.label_table label)
    (*implement interp goto*) 

(* define an array; only two cases for Number and Variable *)
let interp_dim (ident : Absyn.ident) (expr : Absyn.expr) = 
    (*implement interp dim *)
    (* print_string(ident); *)
    match expr with
    (* if the expression is a number, just add Ex. a[3] *)
    | Number size-> Hashtbl.add Tables.array_table ident 
        (Array.make (int_of_float size) 0.)
    (* just in sake of the pattern match and no warning *)
    | Unary (oper, expr) -> ()
    | Binary (oper, expr1, expr2) -> ()
    | Memref memref -> match memref with
        | Arrayref(name, size) -> ()
    (* if  variable, fetch its value from the table; Ex. a[i] *)
        | Variable name -> Hashtbl.add Tables.array_table ident 
            (Array.make (int_of_float 
              (Hashtbl.find Tables.variable_table name)) 0.)
             


let interp_stmt (stmt : Absyn.stmt): Absyn.program option = 
  match stmt with
    | Dim (ident, expr) -> interp_dim ident expr; None
    | Let (memref, expr) -> interp_let memref expr; None
    | Goto label -> interp_goto label
    | If (expr, label) -> interp_if expr label
    | Print print_list -> interp_print print_list; None
    | Input memref_list -> interp_input memref_list; None

let rec interpret (program : Absyn.program) = match program with
    | [] -> ()
    | firstline::otherlines -> match firstline with
      | _, _, None -> interpret otherlines
      | _, _, Some stmt -> let next_line = interp_stmt stmt
                           in match next_line with
                           | None -> interpret otherlines
                           | Some line -> interpret line 

let interpret_program program =
    (Tables.init_label_table program; 
     if !want_dump then Tables.dump_label_table ();
     if !want_dump then Dumper.dump_program program;
     interpret program)

