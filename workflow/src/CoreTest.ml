open Core

module Q = UntypedQuery.Syntax

let univ = JsApi.univ
let db = JsApi.db

let runResult result = match result with
  | Result.Ok () -> ()
  | Result.Error err ->
    Js.log2 "ERROR:" err;
    exit 1

let runQuery query =
  Js.log "--- RUNNING QUERY ---";
  let result =
    Js.log2 "QUERY:" (UntypedQuery.show query);
    let open Result.Syntax in
    Js.log "TYPING...";
    let%bind query = QueryTyper.typeQuery ~univ query in
    let (_, (_, typ)), _ = query in
    Js.log2 "TYPE:" (Type.show typ);
    Js.log "RUNNING...";
    let%bind result = JSONDatabase.execute ~db query in
    Js.log2 "RESULT:" result;
    return ()
  in
  runResult result;
  Js.log "--- DONE ---"

let typeWorkflow w =
  Js.log "TYPING WORKFLOW...";
  runResult (Result.ignore (WorkflowTyper.typeWorkflow ~univ w))

let () =

  (* Navigation *)

  runQuery Q.(
    void
    |> nav "region"
  );
  runQuery Q.(
    here
    |> nav "region"
  );
  runQuery Q.(
    here
    |> nav "region"
    |> nav "nation"
  );
  runQuery Q.(
    here
    |> nav "region"
    |> nav "name"
  );

  (* Select *)

  runQuery Q.(
    here
    |> select [
      field (here |> nav "region" |> nav "name");
    ]
  );
  runQuery Q.(
    here
    |> nav "region"
    |> select [
      field (here |> nav "name");
    ]
  );
  runQuery Q.(
    here
    |> nav "region"
    |> select [
      field ~alias:"regionName" (here |> nav "name");
    ]
  );

  (* Define *)
  runQuery Q.(
    here
    |> nav "region"
    |> select [
      field ~alias:"regionName" (name "name")
    ]
    |> where [
      define "name" (here |> nav "name");
    ]
  );
  runQuery Q.(
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
