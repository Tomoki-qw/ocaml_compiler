module Parser = struct
  module L = Lexer
  module E = Evaluator
  let tok = ref (L.ONE ' ')
  let getToken () = L.gettoken ()
  let advance () = (tok := getToken () ; L.print_token (!tok))
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
                        | _ -> let a1 = clause () and a2 = clauses () in a1 :: a2
  and clause () = match !tok with
                    L.ONE '(' -> let a1 = [term()] in eat(L.ONE '.') ; a1
                    | _ ->let a1 = predicate() in let a2 = to_opt()
                          in (eat(L.ONE '.') ; a1 :: a2)
  and to_opt () = match !tok with
                    L.TO->eat(L.TO) ; let a1 = terms() in a1
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
                 L.ONE '(' ->eat(L.ONE '(') ; let t2=term() in eat(L.ONE ')') ; t2
                 | _ ->let p2 = predicate() in p2
and terms () = let t3 = [term () ] in let et1 = eterms () in t3 @ et1
and eterms () = match !tok with
                  L.ONE ',' ->eat(L.ONE ',') ; let t4=[term()] in let et2=eterms() in t4 @ et2
                  | _ ->[]
and predicate () = match !tok with 
                    L.CID a1 -> eat (L.CID "") ; eat (L.ONE '(') ; let a3 = args () in eat (L.ONE ')') ; E.App(a1,a3)
                    | _ -> error ()
and args () = let e = [expr ()] in let ea = eargs () in e @ ea
and eargs () = match !tok with
                L.ONE ',' ->eat (L.ONE ','); let e1=[expr()] in let ea1=eargs() in e1 @ ea1
                | _ ->[]
and expr () = match !tok with
                L.ONE '(' -> eat (L.ONE '(') ;let a2 = expr () in eat (L.ONE ')') ; a2
                |L.ONE '[' -> eat (L.ONE '[') ; let a2 = list () in eat (L.ONE ']') ;a2
                |L.CID s -> eat (L.CID "") ; let a2 = tail_opt s in a2
                |L.VID s -> eat (L.VID "") ; (E.Var s)
                |L.NUM n -> eat (L.NUM "") ; (E.Atom n)
                |_ -> error ()
and tail_opt m = match !tok with 
                  L.ONE '(' -> eat (L.ONE '(') ; let a2 = args () in eat (L.ONE ')') ; E.App(m,a2)
                  | _ -> E.Atom m
and list () = match !tok with
                L.ONE ']' -> E.Atom "Nil"
                | _ ->E.App ("cons",[expr() ; list_opt()])
and list_opt () = match !tok with
                    L.ONE '|' -> (eat (L.ONE '|') ; let a2 = id () in a2)
                    |L.ONE ',' -> (eat (L.ONE ',') ; let a2 = list () in a2)
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