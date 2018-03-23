open Workflow

let univ = JsApi.univ
let db = JsApi.db

(**
  * {
  *   individuals: individual
  *   individualsNames: individual.name
  *   sites: individual.site
  *   siteTitles: individual.site.title
  * }
  *)
let getSomeData = UntypedQuery.Syntax.(
  void
  |> select [
    field ~alias:"individuals" (here |> nav "individual");
    field ~alias:"individualNames" (here |> nav "individual" |> nav "name");
    field ~alias:"sites" (here |> nav "individual" |> nav "site");
    field ~alias:"siteTitles" (here |> nav "individual" |> nav "site" |> nav "title");
  ]
)

(*
  * individual.site.pick
  *)
let renderListOfSites = UntypedQuery.Syntax.(
  void |> nav "individual" |> nav "site" |> screen "pick"
)

(*
  * individual.site.first.view
  *)
let renderFirstSite = UntypedQuery.Syntax.(
  void |> nav "individual" |> nav "site" |> first |> screen "view"
)

(*
  * individual.pick.value(id: "someid").site.view
  *)
let renderSiteByIndividual = UntypedQuery.Syntax.(
  void
  |> nav "individual"
  |> screen ~args:[Arg.number "id" 1.] "pick"
  |> nav "value"
  |> nav "site"
  |> screen "view"
)

(*
  * individual.pick.value(id: "someid").site.view
  *)
let getSiteTitleByIndividualViaView = UntypedQuery.Syntax.(
  void
  |> nav "individual"
  |> screen ~args:[Arg.number "id" 1.] "pick"
  |> nav "value"
  |> nav "site"
  |> screen "view"
  |> nav "value"
  |> nav "title"
)

(*
  * individual.pick.value(id: "someid").site.view
  *)
let getSiteTitleByIndividualViaViewViaBind2 = UntypedQuery.Syntax.(
  void
  |> chain (
    here
    |> nav "individual"
    |> screen ~args:[Arg.number "id" 1.] "pick"
    |> chain (
      here
      |> nav "value"
      |> chain (
        here
        |> nav "site"
        |> screen "view"
      )
    )
  )
)

(*
  * individual.pick.value(id: "someid").site.view
  *)
let getSiteTitleByIndividualViaViewViaBind = UntypedQuery.Syntax.(
  void
  |> nav "individual"
  |> screen ~args:[Arg.number "id" 1.] "pick"
  |> chain (
    here
    |> nav "value"
    |> nav "site"
    |> screen "view"
    |> nav "value"
    |> chain (
      here
      |> nav "title"
    )
  )
)

(*
  * individual.pick.value(id: "someid").site.view
  *)
let getSelectedIndividual = UntypedQuery.Syntax.(
  void
  |> nav "individual"
  |> screen ~args:[Arg.number "id" 1.] "pick"
  |> nav "value"
)

let pickAndViewIndividualWorkflow =
  let open UntypedWorkflow.Syntax in
  let open UntypedQuery.Syntax in

  let pickIndividual = render (here |> nav "individual" |> screen "pick") in

  let view = render (here |> screen "view") in

  let viewSite = render (here |> nav "site" |> screen "view") in

  pickIndividual |> andThen [ view; viewSite; ]

let runResult result = match result with
  | Result.Ok () -> ()
  | Result.Error err -> Js.log2 "ERROR:" err

let runQuery db query =
  Js.log "--- RUNNING QUERY ---";
  let result =
    Js.log2 "QUERY:" (UntypedQuery.show query);
    let open Result.Syntax in
    Js.log "TYPING...";
    let%bind query = QueryTyper.typeQuery ~univ query in
    let (_, typ), _ = query in
    Js.log2 "TYPE:" (Type.show typ);
    Js.log "RUNNING...";
    let%bind result = JSONDatabase.execute db query in
    Js.log2 "RESULT:" result;
    return ()
  in
  runResult result;
  Js.log "--- DONE ---"

let typeWorkflow w =
  Js.log "TYPING WORKFLOW...";
  runResult (Result.ignore (WorkflowTyper.typeWorkflow ~univ w))

let () =
  Js.log db;
  runQuery db getSomeData;
  runQuery db renderListOfSites;
  runQuery db renderFirstSite;
  runQuery db renderSiteByIndividual;
  runQuery db getSelectedIndividual;
  runQuery db getSiteTitleByIndividualViaView;
  runQuery db getSiteTitleByIndividualViaViewViaBind;
  runQuery db getSiteTitleByIndividualViaViewViaBind2;

  (**
    * Value & title of the pick screen with no item selected.
    *)
  runQuery db UntypedQuery.Syntax.(
    void
    |> nav "individual"
    |> screen "pick"
    |> select [
      field ~alias:"value" (here |> nav "value");
      field ~alias:"title" (here |> nav "title");
    ]
  );

  (**
    * Value of the pick screen with the selected item.
    *)
  runQuery db UntypedQuery.Syntax.(
    void
    |> nav "individual"
    |> screen ~args:[Arg.number "id" 2.] "pick"
    |> select [
      field ~alias:"value" (here |> nav "value");
      field ~alias:"title" (here |> nav "title");
    ]
  );

  typeWorkflow pickAndViewIndividualWorkflow;
