open! Jest
open! Expect
open! Expect.Operators

module Result = Common.Result

module Q = Query.Untyped.Syntax

let univ = JsApi.univ
let db = JsApi.db

let runToResult v = match Run.toResult v with
  | Result.Ok v -> Result.Ok v
  | Result.Error (`DatabaseError err) -> Result.Error err
  | Result.Error (`RunWorkflowError err) -> Result.Error err
  | Result.Error (`WorkflowTypeError err) -> Result.Error err
  | Result.Error (`QueryTypeError err) -> Result.Error err

let runResult result = match result with
  | Result.Ok () -> ()
  | Result.Error err ->
    Js.log2 "ERROR:" err;
    exit 1

let expectOk = function
  | Result.Ok _ -> pass
  | Result.Error err -> fail err

let expectQueryOk query =
  let result =
    let open Run.Syntax in
    let%bind query = QueryTyper.typeQuery ~univ query in
    let%bind _result = JSONDatabase.query ~db query in
    return ()
  in
  expectOk (runToResult result)

let unwrapAssertionResult = function
  | Result.Ok assertion -> assertion
  | Result.Error err -> fail err

let runQueryAndExpect q v =
  unwrapAssertionResult (runToResult (
    let open Run.Syntax in
    let%bind q = Core.QueryTyper.typeQuery ~univ q in
    let%bind r = JSONDatabase.query ~db q in
    return (expect(r) |> toEqual(v))
  ))

let typeWorkflow w =
  runToResult (Workflow.Typer.typeWorkflow ~univ w)

let () =

  describe "navigation" begin fun () ->

    test "/region" begin fun () ->
      expectQueryOk Q.(
        void
        |> nav "region"
      );
    end;

    test "region" begin fun () ->
      expectQueryOk Q.(
        here
        |> nav "region"
      );
    end;

    test "region.nation" begin fun () ->
      expectQueryOk Q.(
        here
        |> nav "region"
        |> nav "nation"
      );
    end;

    test "region.name" begin fun () ->
      expectQueryOk Q.(
        here
        |> nav "region"
        |> nav "name"
      );
    end
  end;

  describe "select" begin fun () ->
    test "{ region.name }" begin fun () ->
      expectQueryOk Q.(
        here
        |> select [
          field (here |> nav "region" |> nav "name");
        ]
      );
    end;

    test "region { name }" begin fun () ->
      expectQueryOk Q.(
        here
        |> nav "region"
        |> select [
          field (here |> nav "name");
        ]
      );
    end;

    test "region { regionName: name }" begin fun () ->
      expectQueryOk Q.(
        here
        |> nav "region"
        |> select [
          field ~alias:"regionName" (here |> nav "name");
        ]
      );
    end;
  end;

  describe "define" begin fun () ->
    test "let name = name in region { regionName: $name }" begin fun () ->
      expectQueryOk Q.(
        here
        |> nav "region"
        |> select [
          field ~alias:"regionName" (name "name")
        ]
        |> where [
          define "name" (here |> nav "name");
        ]
      );
    end;

    test "let name = name in region { regionName: $name, nationName: let name = nation.name in $name}" begin fun () ->
      expectQueryOk Q.(
        here
        |> nav "region"
        |> select [
          field ~alias:"regionName" (name "name");
          field ~alias:"nationName" (
            name "name"
            |> where [
              define "name" (here |> nav "nation" |> nav "name");
            ]
          );
        ]
        |> where [
          define "name" (here |> nav "name");
        ]
      );
    end;
  end;

  describe "screens" begin fun () ->
    test "region:pick" begin fun () ->
      expectQueryOk Q.(
        here
        |> nav "region"
        |> screen "pick"
      );
    end;

    test "region:pick.value" begin fun () ->
      expectQueryOk Q.(
        here
        |> nav "region"
        |> screen "pick"
        |> nav "value"
      );
    end;

    test "region:pick(id: 'ASIA').value" begin fun () ->
      expectQueryOk Q.(
        here
        |> nav "region"
        |> screen ~args:[arg "id" (string "ASIA")] "pick"
        |> nav "value"
      );
    end;

    test "region:pick(id: 'ASIA').value:view" begin fun () ->
      expectQueryOk Q.(
        here
        |> nav "region"
        |> screen ~args:[arg "id" (string "ASIA")] "pick"
        |> nav "value"
        |> screen "view"
      );
    end;
  end;

  describe "meta" begin fun () ->
    test "region:meta" begin fun () ->
      expectQueryOk Q.(
        here
        |> nav "region"
        |> meta
      );
    end;
    test "region:meta.type" begin fun () ->
      expectQueryOk Q.(
        here
        |> nav "region"
        |> meta
        |> nav "type"
      );
    end;
    test "region:meta.type.card" begin fun () ->
      expectQueryOk Q.(
        here
        |> nav "region"
        |> meta
        |> nav "type"
        |> nav "card"
      );
    end;
    test "region:meta.registry" begin fun () ->
      expectQueryOk Q.(
        here
        |> nav "region"
        |> meta
        |> nav "registry"
      );
    end;
  end;

  describe "< / lessThan" begin fun () ->
    test "1 < 2 -> true" begin fun () ->
      runQueryAndExpect Q.(
        lessThan (number 1.) (number 2.)
      ) (Value.bool true);
    end;

    test "2 < 1 -> false" begin fun () ->
      runQueryAndExpect Q.(
        lessThan (number 2.) (number 1.)
      ) (Value.bool false);
    end;

    test "1 < null -> null" begin fun () ->
      runQueryAndExpect Q.(
        lessThan (number 1.) null
      ) Value.null;
    end;

    test "null < 1 -> null" begin fun () ->
      runQueryAndExpect Q.(
        lessThan null (number 1.)
      ) Value.null;
    end;

    test "null < null -> null" begin fun () ->
      runQueryAndExpect Q.(
        lessThan null null
      ) Value.null;
    end;


  end;
