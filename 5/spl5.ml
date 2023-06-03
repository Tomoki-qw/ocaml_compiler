module Lexer = struct
  type token = CID of string | VID of string | NUM of string| TO | IS | QUIT | OPEN | EOF | ONE of char
  module P = Printf
  exception End_of_system
  let _ISTREAM = ref stdin
  let ch = ref []
  let read () = match !ch with [] -> input_char !_ISTREAM
                              | h::rest -> (ch := rest; h)
  let unread c = ch := c::!ch
  let lookahead () = try let c = read () in unread c; c with End_of_file -> '$'
    let rec integer i=
      let c = lookahead () in
      if (c >= '0' && c <= '9') then
      integer (i^(Char.escaped (read ())))
      else i
      and identifier id =
    let  c = lookahead () in
      if ((c >= 'a' && c <= 'z') || (c >= 'A' && c <= 'Z') ||
        (c >= '0' && c <= '9') || c == '_') then
          identifier (id^(Char.escaped (read ())))
    else id
    and native_token () =
    let c = lookahead () in
      if  (c >= 'a' && c <= 'z') then
      let id = identifier "" in
      match id with
      |"is"->IS
      |"quit"->QUIT
      |"open"->OPEN
      |"eof"->EOF
      |_ ->CID (id) 
      else if (c >= 'A' && c <= 'Z') then VID (identifier "") 
      else if (c >= '0' && c <= '9') then NUM (integer "")
      else if (c = ':') then let _ = read () in let b = lookahead ()
      in if (b = '-') then let _ = read () in TO
      else ONE (':')
      else ONE (read ())
      and gettoken () = try
      let token = native_token () in
      match token with
      ONE ' ' -> gettoken ()
      | ONE '\t' -> gettoken ()
      | ONE '\n' -> gettoken ()
      | _ -> token
      with End_of_file -> EOF
      let print_token tk=
        match tk with
        |(CID i)->P.printf "CID(%s)" i
        |(VID i)->P.printf "VID(%s)" i
        |(NUM i)->P.printf "NUM(%S)" i
        |(TO)
        ->P.printf ":-"
        |(IS)
        ->P.printf "IS"
        |(QUIT) ->P.printf "QUIT"
        |(OPEN) ->P.printf "OPEN"
        |(EOF) ->P.printf "EOF"
        |(ONE c)->P.printf "ONE(%c)" c
      end;;

      module Evaluator =
      struct
      (*抽象構文木の型宣言*)
      type ast = Atom of string | Var of string | App of string * ast list
      (*抽象構文木の印字関数*)
      module P = Printf
      exception Finish
      let rec print_ast_list lst1 lst2 = 
        match (lst1,lst2) with ([],[]) -> 
                               (flush stdout;
                               let a = ref ' ' in
                                while !a != ';' && !a != '.' do
                                a := input_char stdin
                                done;
                              if !a='.' then raise Finish else ())
                              |(x1::rest1,x2::rest2) -> if x1 = x2 then (print_ast_list rest1 rest2)
                                                        else (match (x1,x2) with 
                                                             |((Atom t),(Var s)) -> (P.printf "%s" s;print_string"=";P.printf "%s" t;P.printf "\n";print_ast_list rest1 rest2)
                                                             |(_,_) -> ())
                              |(_,_) -> ()
      let rec print_ast ast lst = match (ast,lst) with
      (App(s1, hd1::tl1),[App(s2, hd2::tl2)]) -> if hd1 = hd2 then (print_ast_list tl1 tl2)
                                               else (match (hd1,hd2) with
                                               |((Atom t),(Var s)) -> (P.printf "%s" s;print_string"=";P.printf "%s" t;P.printf "\n";print_ast_list tl1 tl2)
                                               |(_,_) -> ())
      |(App(s1, hd1::tl1),_) -> ()
      |(_,_) -> ()
      (*sub*)
      let sub name term =
        let rec mapVar ast = match ast with
      (Atom x) -> Atom(x)
      | (Var n) -> if n=name then term else Var n
      | (App(n, terms)) -> App(n, List.map mapVar terms)
      in mapVar ;;
      (*mgu*)
      let mgu (a,b) =
         let rec ut (one, another, unifier) = match (one, another) with
          ([], []) -> (true, unifier)
      | (term::t1, Var(name)::t2) -> let r = fun x -> sub name term (unifier x) in ut(List.map r t1,List.map r t2, r)
      | (Var(name)::t1, term::t2) -> let r = fun x -> sub name term (unifier x) in ut(List.map r t1,List.map r t2, r) 
      | (Atom(n)::t1, Atom(m)::t2) ->
             if n=m then ut(t1,t2,unifier) else (false, unifier)
      | (App(n1,xt1)::t1, App(n2,xt2)::t2) ->
      if n1=n2 && List.length xt1 = List.length xt2 then ut(xt1@t1, xt2@t2, unifier)
             else (false, unifier)
      | (_,_) -> (false, unifier);
      in ut ([a],[b], (fun x -> x))
      (*succeed*)
      (*let succeed query que = (test que ; true)*)
      let succeed query que = (print_ast query que ; true)
      (*rename*)
      let rename ver term =
        let rec mapVar ast = match ast with
      (Atom x) -> Atom(x)
      | (Var n) -> Var(n^"#"^ver)
      | (App(n, terms)) -> App(n, List.map mapVar terms)
        in mapVar term
      
      (*solve*)
      exception Compiler_error
      let rec out_solve (program, question, result, depth) = let aaa = question in
      let rec solve (program, question, result, depth) = match question with [] -> succeed result aaa 
      | goal::goals ->
      let onestep _ clause =
      match List.map (rename (string_of_int depth)) clause with [] -> raise Compiler_error
      | head::conds ->
      let (unifiable, unifier) = mgu(head,goal) in
      if unifiable then
      solve (program, List.map unifier (conds@goals),
          unifier result, depth+1)
          else true
          in List.fold_left onestep true program
          in solve (program, question, result, depth)
      (*eval*)
             let eval (program, question) = out_solve (program, [question], question, 1) 
      
      end;;
      module Parser = struct
        module L = Lexer
        module E = Evaluator
        let tok = ref (L.ONE ' ')
        let token_hozon = ref ()
        let getToken () = L.gettoken ()
        let advance () = (tok := getToken () (*; L.print_token (!tok)*))
        exception Syntax_error
        let error () = raise Syntax_error
        let prog = ref [[E.Var ""]]
        let check t = match !tok with
                        L.CID _ -> if (t = (L.CID "")) then () else error ()
                        |L.VID _-> if (t = (L.VID "")) then () else error ()
                        |L.NUM _-> if (t = (L.NUM "")) then () else error ()
                        |L.TO -> if (t = (L.TO)) then () else error ()
                        |tk -> if (t = tk) then () else error ()
        let eat t = (check t; advance ())
        let rec clauses () = match !tok with
                              L.EOF ->[]
                              | _ -> let cl = clause () and cls = clauses () in cl :: cls
        and clause () = match !tok with
                          L.ONE '(' -> let t = [term()] in eat(L.ONE '.') ; t
                          | _ ->let p = predicate() in let too = to_opt()
                                in (eat(L.ONE '.') ; p :: too)
        and to_opt () = match !tok with
                          L.TO->eat(L.TO) ; let t = terms() in t
                          | _ -> []
        and command () = match !tok with
                          L.QUIT -> exit 0
                          |L.OPEN -> (eat (L.OPEN);
                             match !tok with
                               L.CID s -> (eat (L.CID "") ; check (L.ONE '.') ;
                                          L._ISTREAM := open_in (s^".pl") ; advance () ;
                                          prog := clauses() ; close_in (!L._ISTREAM))
                              | _ -> error ())
                          | _ ->let t=term() in
                                (check(L.ONE '.' ) ; let _ =E.eval(!prog, t) in ())
        and term () = match !tok with
                       L.ONE '(' ->eat(L.ONE '(') ; let t=term() in eat(L.ONE ')') ; t
                       | _ ->let p = predicate() in p
      and terms () = let t = [term () ] in let et = eterms () in t @ et
      and eterms () = match !tok with
                        L.ONE ',' ->eat(L.ONE ',') ; let t=[term()] in let et=eterms() in t @ et
                        | _ ->[]
      and predicate () = match !tok with 
                          L.CID a -> eat (L.CID "") ; eat (L.ONE '(') ; let ar = args () in eat (L.ONE ')') ; E.App(a,ar)
                          | _ -> error ()
      and args () = let e = [expr ()] in let ea = eargs () in e @ ea
      and eargs () = match !tok with
                      L.ONE ',' ->eat (L.ONE ','); let e = [expr()] in let ea = eargs() in e @ ea
                      | _ ->[]
      and expr () = match !tok with
                      L.ONE '(' -> eat (L.ONE '(') ;let e = expr () in eat (L.ONE ')') ; e
                      |L.ONE '[' -> eat (L.ONE '[') ; let l = list () in eat (L.ONE ']') ;l
                      |L.CID s -> eat (L.CID "") ; let t = tail_opt s in t
                      |L.VID s -> eat (L.VID "") ; (E.Var s)
                      |L.NUM n -> eat (L.NUM "") ; (E.Atom n)
                      |_ -> error ()
      and tail_opt m = match !tok with 
                        L.ONE '(' -> eat (L.ONE '(') ; let ar = args () in eat (L.ONE ')') ; E.App(m,ar)
                        | _ -> E.Atom m
      and list () = match !tok with
                      L.ONE ']' -> E.Atom "Nil"
                      | _ ->E.App ("cons",[expr() ; list_opt()])
      and list_opt () = match !tok with
                          L.ONE '|' -> (eat (L.ONE '|') ; let i = id () in i)
                          |L.ONE ',' -> (eat (L.ONE ',') ; let l = list () in l)
                          | _ -> E.Atom "Nil"
      and id () = match !tok with
                    L.CID a -> eat (L.CID "") ; (E.Atom a)
                    |L.VID a -> eat (L.VID "") ; (E.Var a)
                    |L.NUM a -> eat (L.NUM "") ; (E.Atom a)
                    | _ -> error ()
      end
      let rec run () =
        print_string "?- ";
        while true do
        flush stdout ; Lexer._ISTREAM := stdin;
        Parser.advance () ; Parser.command () ; print_string "\n?- "
        done
      let _ = run()
      ;;