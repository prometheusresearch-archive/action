let () =
  let parse s =
    let filebuf = Lexing.from_string s in
    try
      Js.log2 "RESULT" (Core.UntypedQuery.show (Parser.query Lexer.read filebuf))
    with
    | Lexer.Error msg ->
      Js.log msg
    | Parser.Error ->
      Js.log2 "At offset %d: syntax error.\n%!" (Lexing.lexeme_start filebuf)
  in

  parse "/";
  parse "/ ";
  parse " /";
  parse "\n/";
  parse "/\n";
  parse "/individual";
  parse "individual";
  parse "individual.site";
  parse "individual.site.title";
  parse "individual{}";
  parse "individual { site }";
  parse "individual { site, site.title, individualName: name }";
  parse "individual()";
  parse "individual.site()";
  parse "individual { site() }";
  parse "individual { s: site() }";
  parse "individual(x: true)";
  parse "individual(x: false)";
  parse "individual(x: 42)";
  parse "individual(x: 42.5)";
  parse "individual(x: \"hello\")";
  parse "individual(x: 1, y :2)";
  parse "individual.screen:pick(x: 1, y :2)";
