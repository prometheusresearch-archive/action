open! Jest
open! Expect
open! Expect.Operators

module Result = Common.Result

module Q = Query.Untyped.Syntax
module M = Query.Mutation.Syntax
module QueryTyper = QueryTyper.Make(JSONDatabase.Universe)

let univ = Config.univ
let db = Config.db

let errorMessage = function
  | `DatabaseError err
  | `RunWorkflowError err
  | `WorkflowTypeError err
  | `QueryTypeError err -> err

let runToResult v =
  let formatContext ctx =
    ctx
    |> List.rev
    |> List.map (fun err -> let err = errorMessage err in {j|  $err|j})
    |> String.concat "\n"
  in
  let formatError kind msg ctx =
    let ctx = formatContext ctx in
    let msg = {j|$kind: $msg\nContext:\n$ctx|j} in
    Result.Error msg
  in
  match Run.toResultWithContext v with
  | Result.Ok v -> Result.Ok v
  | Result.Error (`DatabaseError msg, ctx) -> formatError "DatabaseError" msg ctx
  | Result.Error (`RunWorkflowError msg, ctx) -> formatError "RunWorkflowError" msg ctx
  | Result.Error (`WorkflowTypeError msg, ctx) -> formatError "WorkflowTypeError" msg ctx
  | Result.Error (`QueryTypeError msg, ctx) -> formatError "QueryTypeError" msg ctx

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
    let%bind q = QueryTyper.typeQuery ~univ q in
    let%bind r = JSONDatabase.query ~db q in
    return (expect(r) |> toEqual(v))
  ))

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

    test "region:pick()(id: 'ASIA')" begin fun () ->
      expectQueryOk Q.(
        here
        |> nav "region"
        |> screen "pick"
        |> growArgs [arg "id" (string "ASIA")]
      );
    end;

    test "region:pick()(id: 'ASIA').value" begin fun () ->
      expectQueryOk Q.(
        here
        |> nav "region"
        |> screen "pick"
        |> growArgs [arg "id" (string "ASIA")]
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

    test "void:grow(here.region:pick(id: 'AFRICA')):grow(here.value:view())" begin fun () ->
      expectQueryOk Q.(
        void
        |> grow (
          here
          |> nav "region"
          |> screen ~args:[arg "id" (string "ASIA")] "pick"
        )
        |> grow (
          here
          |> nav "value"
          |> screen "view"
        )
      );
    end;

    test "void:grow(here.region:pick(id: 'AFRICA')):grow(here.value:view()).data" begin fun () ->
      expectQueryOk Q.(
        void
        |> grow (
          here
          |> nav "region"
          |> screen ~args:[arg "id" (string "ASIA")] "pick"
        )
        |> grow (
          here
          |> nav "value"
          |> screen "view"
        )
        |> nav "data"
      );
    end;

    test "void:grow(here.region:pick(id: 'AFRICA'):grow(here.value:view()))" begin fun () ->
      expectQueryOk Q.(
        void
        |> grow (
          here
          |> nav "region"
          |> screen ~args:[arg "id" (string "ASIA")] "pick"
          |> grow (
            here
            |> nav "value"
            |> screen "view"
          )
        )
      );
    end;

    test "void:grow(here.region:pick(id: 'AFRICA'):grow(here.value:view())).data" begin fun () ->
      expectQueryOk Q.(
        void
        |> grow (
          here
          |> nav "region"
          |> screen ~args:[arg "id" (string "ASIA")] "pick"
          |> grow (
            here
            |> nav "value"
            |> screen "view"
          )
        )
        |> nav "data"
      );
    end;

    test "here.region:first:form(spec: :update {name: $value.name})" begin fun () ->
      expectQueryOk Q.(
        let spec =
          here
          |> update [
            "name", M.update (name "value" |> nav "name");
          ]
        in
        void
        |> nav "region"
        |> first
        |> screen ~args:[arg "spec" spec] "form"
      );
    end;

    test "here.region:first:form(spec: :update {name: $value.name.nested})" begin fun () ->
      expectQueryOk Q.(
        let spec =
          here
          |> update [
            "name", M.update (name "value" |> nav "name" |> nav "nested");
          ]
        in
        void
        |> nav "region"
        |> first
        |> screen ~args:[arg "spec" spec] "form"
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

    test "1 < 1 -> false" begin fun () ->
      runQueryAndExpect Q.(
        lessThan (number 1.) (number 1.)
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

  describe "> / greaterThan" begin fun () ->
    test "1 > 2 -> false" begin fun () ->
      runQueryAndExpect Q.(
        greaterThan (number 1.) (number 2.)
      ) (Value.bool false);
    end;

    test "2 > 1 -> true" begin fun () ->
      runQueryAndExpect Q.(
        greaterThan (number 2.) (number 1.)
      ) (Value.bool true);
    end;

    test "1 > 1 -> false" begin fun () ->
      runQueryAndExpect Q.(
        greaterThan (number 1.) (number 1.)
      ) (Value.bool false);
    end;

    test "1 > null -> null" begin fun () ->
      runQueryAndExpect Q.(
        greaterThan (number 1.) null
      ) Value.null;
    end;

    test "null > 1 -> null" begin fun () ->
      runQueryAndExpect Q.(
        greaterThan null (number 1.)
      ) Value.null;
    end;

    test "null > null -> null" begin fun () ->
      runQueryAndExpect Q.(
        greaterThan null null
      ) Value.null;
    end;
  end;

  describe "<= / lessOrEqThan" begin fun () ->
    test "1 <= 2 -> true" begin fun () ->
      runQueryAndExpect Q.(
        lessOrEqThan (number 1.) (number 2.)
      ) (Value.bool true);
    end;

    test "2 <= 1 -> false" begin fun () ->
      runQueryAndExpect Q.(
        lessOrEqThan (number 2.) (number 1.)
      ) (Value.bool false);
    end;

    test "1 <= 1 -> true" begin fun () ->
      runQueryAndExpect Q.(
        lessOrEqThan (number 1.) (number 1.)
      ) (Value.bool true);
    end;

    test "1 <= null -> null" begin fun () ->
      runQueryAndExpect Q.(
        lessOrEqThan (number 1.) null
      ) Value.null;
    end;

    test "null <= 1 -> null" begin fun () ->
      runQueryAndExpect Q.(
        lessOrEqThan null (number 1.)
      ) Value.null;
    end;

    test "null <= null -> null" begin fun () ->
      runQueryAndExpect Q.(
        lessOrEqThan null null
      ) Value.null;
    end;
  end;

  describe ">= / greaterOrEq" begin fun () ->
    test "1 >= 2 -> false" begin fun () ->
      runQueryAndExpect Q.(
        greaterOrEqThan (number 1.) (number 2.)
      ) (Value.bool false);
    end;

    test "2 >= 1 -> true" begin fun () ->
      runQueryAndExpect Q.(
        greaterOrEqThan (number 2.) (number 1.)
      ) (Value.bool true);
    end;

    test "1 >= 1 -> true" begin fun () ->
      runQueryAndExpect Q.(
        greaterOrEqThan (number 1.) (number 1.)
      ) (Value.bool true);
    end;

    test "1 >= null -> null" begin fun () ->
      runQueryAndExpect Q.(
        greaterOrEqThan (number 1.) null
      ) Value.null;
    end;

    test "null >= 1 -> null" begin fun () ->
      runQueryAndExpect Q.(
        greaterOrEqThan null (number 1.)
      ) Value.null;
    end;

    test "null >= null -> null" begin fun () ->
      runQueryAndExpect Q.(
        greaterOrEqThan null null
      ) Value.null;
    end;
  end;

  describe "= / eq" begin fun () ->
    test "1 = 2 -> false" begin fun () ->
      runQueryAndExpect Q.(
        eq (number 1.) (number 2.)
      ) (Value.bool false);
    end;

    test "1 = 1 -> true" begin fun () ->
      runQueryAndExpect Q.(
        eq (number 1.) (number 1.)
      ) (Value.bool true);
    end;

    test "1 = null -> null" begin fun () ->
      runQueryAndExpect Q.(
        eq (number 1.) null
      ) (Value.bool false);
    end;

    test "null = 1 -> null" begin fun () ->
      runQueryAndExpect Q.(
        eq null (number 1.)
      ) (Value.bool false);
    end;

    test "null = null -> null" begin fun () ->
      runQueryAndExpect Q.(
        eq null null
      ) (Value.bool true);
    end;
  end;

  describe "!= / notEq" begin fun () ->
    test "1 != 2 -> true" begin fun () ->
      runQueryAndExpect Q.(
        notEq (number 1.) (number 2.)
      ) (Value.bool true);
    end;

    test "1 != 1 -> false" begin fun () ->
      runQueryAndExpect Q.(
        notEq (number 1.) (number 1.)
      ) (Value.bool false);
    end;

    test "1 != null -> true" begin fun () ->
      runQueryAndExpect Q.(
        notEq (number 1.) null
      ) (Value.bool true);
    end;

    test "null != 1 -> true" begin fun () ->
      runQueryAndExpect Q.(
        notEq null (number 1.)
      ) (Value.bool true);
    end;

    test "null != null -> false" begin fun () ->
      runQueryAndExpect Q.(
        notEq null null
      ) (Value.bool false);
    end;
  end;
