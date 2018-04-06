open Jest
open Expect
open Expect.Operators

let () =

  let expectParseOk s =
    test s begin fun () ->
      let filebuf = Lexing.from_string s in
      try
        let res = Parser.start Lexer.read filebuf in
        match res with
        | Core.ParseResult.Query _ ->
          pass
        | Core.ParseResult.Workflow _ ->
          pass
      with
      | Lexer.Error msg ->
        fail msg
      | Parser.Error ->
        fail "Syntax error"
    end in

  describe "Parsing" begin fun () ->
    expectParseOk "/";
    expectParseOk "/ ";
    expectParseOk " /";
    expectParseOk "\n/";
    expectParseOk "/\n";
    expectParseOk "/individual";
    expectParseOk "individual"; expectParseOk "individual.site"; expectParseOk "individual.site.title";
    expectParseOk "individual{}";
    expectParseOk "individual { site }";
    expectParseOk "individual { site, }";
    expectParseOk "individual { site: site, }";
    expectParseOk "individual { site: site, name, }";
    expectParseOk "individual { site, site.title, individualName: name }";
    expectParseOk "individual { s: site:view() }";
    expectParseOk "individual:pick(x: true)";
    expectParseOk "individual:pick(x: true,)";
    expectParseOk "individual:pick(x: false)";
    expectParseOk "individual:pick(x: 42)";
    expectParseOk "individual:pick(x: 42.5)";
    expectParseOk "individual:pick(x: \"hello\")";
    expectParseOk "individual:pick(x: 1, y :2)";
    expectParseOk "individual:pick(x: 1, y: 2,)";

    expectParseOk ":meta";
    expectParseOk "individual:meta";
    expectParseOk "individual.nation:meta";

    expectParseOk "render(individual:pick)";
    expectParseOk "render(individual:pick) { render(here:view) }";
    expectParseOk "render(individual:pick) { render(here:view), }";
    expectParseOk {|

      render(individual:pick) {
        render(here:view),
        render(site:view)
      }

    |};
    expectParseOk "1 < 2";
    expectParseOk "null";

  end;
