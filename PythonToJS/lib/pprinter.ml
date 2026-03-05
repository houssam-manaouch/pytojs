open PPrint
open Lang

(* Global constant *)
let indent_level = 4

let doc_of_var v = string v
let doc_of_var_list vs = parens (separate_map comma string vs)

let doc_of_value (v : value) : document =
  match v with
  | IntV n -> string (string_of_int n)
  | BoolV b -> string (if b then "true" else "false")
  | StringV s -> dquotes (string s)
  | NoneV -> string "null"

let doc_of_binop (op : binop) : document =
  match op with
  | BArith BAadd -> string "+"
  | BArith BAsub -> string "-"
  | BArith BAmul -> string "*"
  | BArith BAdiv -> string "/"
  | BCompar BCeq -> string "==="
  | BCompar BCne -> string "!=="
  | BCompar BClt -> string "<"
  | BCompar BCle -> string "<="
  | BCompar BCgt -> string ">"
  | BCompar BCge -> string ">="
  | BBool BBand -> string "&&"
  | BBool BBor  -> string "||"

let rec doc_of_expr (e : expr) : document =
  match e with
  | Const v -> doc_of_value v
  | VarE x -> string x
  | BinOp (op, e1, e2) ->
      parens (doc_of_expr e1 ^^ space ^^ doc_of_binop op ^^ space ^^ doc_of_expr e2)
  | CallE (fn, args) ->
      (* Special cases for builtins used in the project *)
      begin match fn, args with
      | "input", [Const (StringV s)] ->
          (* prompt("...") *)
          string "prompt" ^^ parens (dquotes (string s))
      | "int", [a] ->
          string "parseInt" ^^ parens (doc_of_expr a)
      | "str", [a] ->
          string "String" ^^ parens (doc_of_expr a)
      | _ ->
          string fn ^^ parens (separate_map (comma ^^ space) doc_of_expr args)
      end


let doc_of_local_vardecl (Vardecl(vn,_t)) =
  string "let" ^^ space ^^ string vn ^^ semi

let doc_of_global_vardecl (Vardecl(vn,_t)) =
  string "var" ^^ space ^^ string vn ^^ semi

let rec doc_of_stmt (s : stmt) : document =
  match s with
  | Block ss ->
      braces (
        nest indent_level (
          hardline ^^ separate_map hardline doc_of_stmt ss
        ) ^^ hardline
      )
  | Assign (x, e) ->
      string x ^^ space ^^ equals ^^ space ^^ doc_of_expr e ^^ semi
  | Return e ->
      string "return" ^^ space ^^ doc_of_expr e ^^ semi
  | Cond (c, s1, s2) ->
      string "if" ^^ space ^^ parens (doc_of_expr c) ^^ space ^^
      doc_of_stmt s1 ^^ space ^^
      string "else" ^^ space ^^ doc_of_stmt s2
  | While (c, body) ->
      string "while" ^^ space ^^ parens (doc_of_expr c) ^^ space ^^ doc_of_stmt body
  | CallS (fn, args) ->
      (* Special case: print(...) -> console.log(...) *)
      begin match fn with
      | "print" ->
          string "console.log" ^^ parens (separate_map (comma ^^ space) doc_of_expr args) ^^ semi
      | _ ->
          string fn ^^ parens (separate_map (comma ^^ space) doc_of_expr args) ^^ semi
      end

let doc_of_fundefn (Fundefn (Fundecl(fn, params, _rt), vds, body)) =
  let param_names = List.map name_of_vardecl params in
  let locals_doc =
    match vds with
    | [] -> empty
    | _  -> separate_map hardline doc_of_local_vardecl vds ^^ hardline
  in
  separate space [string "function"; string fn; doc_of_var_list param_names]
  ^^ space ^^
  braces (
    nest indent_level (
      hardline ^^ locals_doc ^^ doc_of_stmt body
    ) ^^ hardline
  )

let doc_of_prog (Prog(fdfs, vds, s)) =
  separate_map (hardline ^^ hardline) doc_of_fundefn fdfs
  ^^ hardline ^^ hardline
  ^^ separate_map hardline doc_of_global_vardecl vds
  ^^ hardline
  ^^ doc_of_stmt s
  ^^ hardline

let print_prog prg =
  ToChannel.pretty 0.5 80 stdout (doc_of_prog prg);
  flush stdout
