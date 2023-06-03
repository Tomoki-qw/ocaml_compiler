module Parser = struct
  module L = Lexer
  let tok = ref (L.ONE ' ')
  let getToken () = L.gettoken ()
  let advance () = (tok := getToken () ; L.print_token (!tok))
  exception Syntax_error
  let error () = raise Syntax_error
  let check t = match !tok with
                  L.CID _ -> if (t = (L.CID "")) then () else error ()
                  |L.VID _-> if (t = (L.VID "")) then () else error ()
                  |L.NUM _-> if (t = (L.NUM "")) then () else error ()
                  |L.TO -> if (t = (L.TO)) then () else error ()
                  |tk -> if (t = tk) then () else error ()
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
                        | _ -> error ())
                        | _ -> (term () ; check (L.ONE '.'))
  and term () = match !tok with
                  L.ONE '(' ->(eat(L.ONE '(') ; term() ; eat(L.ONE ')'))
                  | _ -> predicate ()
and terms () = term () ; eterms ()
and eterms () = match !tok with
                  L.ONE ',' ->(eat(L.ONE ',') ; term(); eterms())
                  | _ ->()
and predicate () = match !tok with 
                    L.CID _ -> eat (L.CID "") ; eat (L.ONE '(') ; args () ; eat (L.ONE ')')
                    | _ -> error ()
and args () = arithmexp () ; eargs ()
and eargs () = match !tok with
                L.ONE ',' -> (eat (L.ONE ',') ; arithmexp () ; eargs ())
                | _ ->()
and tail_opt () = match !tok with 
                    L.ONE '(' -> (eat (L.ONE '(') ; args () ; eat (L.ONE ')'))
                    | _ -> ()
and list () = match !tok with
                L.ONE ']' -> ()
                | _ -> (arithmexp () ; list_opt ())
and list_opt () = match !tok with
                    L.ONE '|' -> (eat (L.ONE '|') ; id())
                    |L.ONE ',' -> (eat (L.ONE ',') ; list ())
                    | _ -> ()
and id () = match !tok with
              L.CID _ -> eat (L.CID "")
              |L.VID _ -> eat (L.VID "")
              |L.NUM _ -> eat (L.NUM "")
              | _ -> error ()
and arithmexp () = arithmterm () ; earithmexp ()
and earithmexp ()= match !tok with
                    L.ONE '+' -> eat (L.ONE '+') ; arithmterm () ; earithmexp ()
                    |L.ONE '-' -> eat (L.ONE '-') ; arithmterm () ; earithmexp ()
                    | _ -> ()
and arithmterm () = arithmfactor () ; earithmterm ()
and earithmterm () = match !tok with
                      L.ONE '*' -> eat (L.ONE '*') ; arithmfactor () ; earithmterm ()
                      |L.ONE '/' -> eat (L.ONE '/') ; arithmfactor () ; earithmterm ()
                      | _ -> ()
and arithmfactor () = match !tok with
                        L.ONE '(' -> eat (L.ONE '(') ; arithmexp () ; eat (L.ONE ')')
                        |L.ONE '-' -> eat (L.ONE '-') ; arithmexp ()
                        |L.ONE '[' -> eat (L.ONE '[') ; list () ; eat (L.ONE ']')
                        |L.CID s -> eat (L.CID "") ; tail_opt ()
                        |L.VID s -> eat (L.VID "")
                        |L.NUM n -> eat (L.NUM "")
                        |_ -> error ()
end
let rec run () =
  print_string "?- ";
  while true do
  flush stdout ; Lexer._ISTREAM := stdin;
  Parser.advance () ; Parser.command () ; print_string "\n?- "
  done
  ;;