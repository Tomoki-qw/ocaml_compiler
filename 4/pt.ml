module Parser = struct
  module L = Lexer
  let tok = ref (L.ONE ' ')
  let getToken () = L.gettoken ()
  let advance () = (tok := getToken () ; L.print_token (!tok))
  exception Syntax_error1
  exception Syntax_error2
  exception Syntax_error3
  exception Syntax_error4
  exception Syntax_error5
  exception Syntax_error6
  exception Syntax_error7
  exception Syntax_error8
  exception Syntax_error9
  let error1 () = raise Syntax_error1
  let error2 () = raise Syntax_error2
  let error3 () = raise Syntax_error3
  let error4 () = raise Syntax_error4
  let error5 () = raise Syntax_error5
  let error6 () = raise Syntax_error6
  let error7 () = raise Syntax_error7
  let error8 () = raise Syntax_error8
  let error9 () = raise Syntax_error9
  let check t = match !tok with
                  L.CID _ -> if (t = (L.CID "")) then () else error1 ()
                  |L.VID _-> if (t = (L.VID "")) then () else error2 ()
                  |L.NUM _-> if (t = (L.NUM "")) then () else error3 ()
                  |L.TO -> if (t = (L.TO)) then () else (L.print_token (!tok) ; L.print_token t ; error4 ())
                  |tk -> if (t = tk) then () else error5 ()
  let eat t = (check t; advance ())
  let rec clauses () = match !tok with
                        L.EOF ->()
                        | _ -> (clause (); clauses ())
  and clause () = match !tok with
                    L.ONE '(' -> (term () ; eat (L.ONE '.'))
                    | _ ->(predicate () ; to_opt (); eat (L.ONE '.'))
  and to_opt () = match !tok with
                    L.TO->(eat (L.TO) ; terms ())
                    | _ -> ()
  and command () = match !tok with
                    L.QUIT -> exit 0
                    |L.OPEN -> (eat (L.OPEN);
                       match !tok with
                         L.CID s -> (eat (L.CID "") ; check (L.ONE '.') ; L._ISTREAM := open_in (s^".pl") ; advance () ; clauses () ; close_in (!L._ISTREAM))
                        | _ -> error6 ())
                        | _ -> clauses()
  and term () = match !tok with
                  L.ONE '(' ->(eat(L.ONE '(') ; term() ; eat(L.ONE ')'))
                  | _ -> predicate ()
and terms () = term () ; eterms ()
and eterms () = match !tok with
                  L.ONE ',' ->(eat(L.ONE ',') ; term(); eterms())
                  | _ ->()
and predicate () = match !tok with 
                    L.CID _ -> eat (L.CID "") ; eat (L.ONE '(') ; args () ; eat (L.ONE ')')
                    | _ -> error7 ()
and args () = expr () ; eargs ()
and eargs () = match !tok with
                L.ONE ',' -> (eat (L.ONE ',') ; expr () ; eargs ())
                | _ ->()
and expr () = match !tok with
                L.ONE '(' -> (eat (L.ONE '(') ; expr () ; eat (L.ONE ')'))
                |L.ONE '[' -> (eat (L.ONE '[') ; list () ; eat (L.ONE ']'))
                |L.CID s -> eat (L.CID "") ; tail_opt ()
                |L.VID s -> eat (L.VID "")
                |L.NUM n -> eat (L.NUM "")
                |_ -> error8 ()
and tail_opt () = match !tok with 
                    L.ONE '(' -> (eat (L.ONE '(') ; args () ; eat (L.ONE ')'))
                    | _ -> ()
and list () = match !tok with
                L.ONE ']' -> ()
                | _ -> (expr () ; list_opt ())
and list_opt () = match !tok with
                    L.ONE '|' -> (eat (L.ONE '|') ; id())
                    |L.ONE ',' -> (eat (L.ONE ',') ; list ())
                    | _ -> ()
and id () = match !tok with
              L.CID _ -> eat (L.CID "")
              |L.VID _ -> eat (L.VID "")
              |L.NUM _ -> eat (L.NUM "")
              | _ -> error9 ()
end
let rec run () =
  print_string "?- ";
  while true do
  flush stdout ; Lexer._ISTREAM := stdin;
  Parser.advance () ; Parser.command () ; print_string "\n?- "
  done
  ;;