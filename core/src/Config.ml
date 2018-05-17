module Q = Query.Untyped.Syntax

let pickScreen =

  Screen.Syntax.(screen
    ~inputCard:(Some Query.Card.Many)
    ~args:[
      arg "id" ~default:Q.null (one number);
      arg "title" ~default:(Q.string "Pick") (one string);
      arg "fields" ~default:(Q.here) (one string);
    ]
    Q.(
      let parent = name "parent" in
      let title = name "title" in
      let fields = name "fields" in
      let id = name "id" in
      let base = void |> select [
          field ~alias:"data" parent;
          field ~alias:"value" (parent |> locate id);
        ]
      in
      void
      |> select [
        field ~alias:"data" parent;
        field ~alias:"dataForUI" (parent |> grow fields);
        field ~alias:"value" (base |> nav "value");
        field ~alias:"title" (base |> grow title);
      ]
    )
  )

let viewScreen =
  Screen.Syntax.(screen
    ~inputCard:(Some Query.Card.One)
    ~args:[
      arg "title" ~default:(Q.string "View") (one string);
    ]
    Q.(
      let base = void |> select [
        field ~alias:"value" (name "parent");
      ] in
      void
      |> select [
        field ~alias:"data" (name "parent");
        field ~alias:"value" (name "parent");
        field ~alias:"title" (base |> grow (name "title"));
      ]
    )
  )

let formScreen =
  Screen.Syntax.(screen
    ~inputCard:None
    ~args:[
      arg "title" ~default:(Q.string "Form") (one string);
      arg "spec" (one string);
      arg "value" ~default:(Q.null) (one string);
    ]
    Q.(
      let title = name "title" in
      let parent = name "parent" in
      let base = void |> select [
        field ~alias:"value" parent;
      ] in
      let mutation =
        parent
        |> grow (name "spec")
      in
      void
      |> select [
        field ~alias:"mutation" mutation;
        field ~alias:"data" parent;
        field ~alias:"value" parent;
        field ~alias:"title" (base |> grow title);
      ]
    )
  )

let univ =
  JSONDatabase.Config.(
    let customer = fun _ -> [
      hasOne "id" string;
      hasOne "name" string;
      hasOne "comment" string;
      hasOne "phone" string;
      hasOne "acctbal" string;
    ] in

    let nation = fun _ -> [
      hasOne "id" string;
      hasOne "name" string;
      hasOne "comment" string;
      hasMany "customer" (entity "customer");
      hasLink ~via:("region", "id") "region" (entity "region");
    ] in

    let region = fun _ -> [
      hasOne "id" string;
      hasOne "name" string;
      hasOne "comment" string;
      hasManyBackLink ~via:("nation", "region") "nation" (entity "nation");
    ] in

    init

    |> defineEntity "region" region
    |> defineEntity "nation" nation
    |> defineEntity "customer" customer

    |> defineScreen "pick" pickScreen
    |> defineScreen "view" viewScreen
    |> defineScreen "form" formScreen

    |> finish
  )

external data : Js.Json.t = "data" [@@bs.module "./data.js"]

let db = JSONDatabase.ofJson ~univ data

