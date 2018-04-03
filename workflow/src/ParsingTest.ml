let () =
  let parse s =
    let filebuf = Lexing.from_string s in
    try
      let res = Parser.start Lexer.read filebuf in
      match res with
      | Core.ParseResult.Query q ->
        Js.log2 "QUERY" (Core.Query.show q)
      | Core.ParseResult.Workflow w ->
        Js.log2 "WORKFLOW" (Core.UntypedWorkflow.show w)
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
  parse "individual { site, }";
  parse "individual { site: site, }";
  parse "individual { site: site, name, }";
  parse "individual { site, site.title, individualName: name }";
  parse "individual()";
  parse "individual.site()";
  parse "individual { site() }";
  parse "individual { s: site() }";
  parse "individual(x: true)";
  parse "individual(x: true,)";
  parse "individual(x: false)";
  parse "individual(x: 42)";
  parse "individual(x: 42.5)";
  parse "individual(x: \"hello\")";
  parse "individual(x: 1, y :2)";
  parse "individual(x: 1, y: 2,)";
  parse "individual:pick(x: 1, y :2)";
  parse "render(individual:pick)";
  parse "render(individual:pick) { render(here:view) }";
  parse "render(individual:pick) { render(here:view), }";
  parse {|

    render(individual:pick) {
      render(here:view),
      render(site:view)
    }

  |}
