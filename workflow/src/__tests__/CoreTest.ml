open Jest
open Expect
open Expect.Operators

open Core

module Q = Query.Syntax

let univ = JsApi.univ
let db = JsApi.db

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
    let open Result.Syntax in
    let%bind query = QueryTyper.typeQuery ~univ query in
    let (_scope, (_card, _typ)), _syn = query in
    let%bind _result = JSONDatabase.execute ~db query in
    return ()
  in
  expectOk result

let typeWorkflow w =
  runResult (Result.ignore (WorkflowTyper.typeWorkflow ~univ w))

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
    test "region:meta.card" begin fun () ->
      expectQueryOk Q.(
        here
        |> nav "region"
        |> meta
        |> nav "card"
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
  end;
