open! Jest
open! Expect
open! Expect.Operators

module Q = Core.Query.Syntax

let univ =
  let rec nation = lazy Core.Type.Syntax.(
    entity "nation" (fun _ -> [
      hasOne "id" string;
      hasOne "name" string;
      hasOne "region" (Lazy.force region);
    ])
  )
  and region = lazy Core.Type.Syntax.(
    entity "region" (fun _ -> [
      hasOne "id" string;
      hasOne "name" string;
      hasMany "nation" (Lazy.force nation);
    ])
  )
  in
  Core.Universe.(
    empty
    |> hasMany "region" (Lazy.force region)
    |> hasMany "nation" (Lazy.force nation)
  )

let db = JSONDatabase.ofStringExn ~univ {|
  {
    "region": {
      "AMERICA": {
        "id": "AMERICA",
        "name": "America",
        "nation": [
          {"$ref": {"entity": "nation", "id": "US"}}
        ]
      },
      "ASIA": {
        "id": "ASIA",
        "name": "Asia",
        "nation": [
          {"$ref": {"entity": "nation", "id": "RUSSIA"}},
          {"$ref": {"entity": "nation", "id": "CHINA"}}
        ]
      }
    },

    "nation": {
      "US": {
        "id": "US",
        "name": "United States of America",
        "region": {"$ref": {"entity": "region", "id": "AMERICA"}}
      },
      "CHINA": {
        "id": "CHINA",
        "name": "China",
        "region": {"$ref": {"entity": "region", "id": "ASIA"}}
      },
      "RUSSIA": {
        "id": "RUSSIA",
        "name": "Russia",
        "region": {"$ref": {"entity": "region", "id": "ASIA"}}
      }
    }
  }
|}

let unwrapAssertionResult = function
  | Core.Result.Ok assertion -> assertion
  | Core.Result.Error err -> fail err

let runQueryAndExpect q v =
  unwrapAssertionResult (
    let open Core.Result.Syntax in
    let%bind q = Core.QueryTyper.typeQuery ~univ q in
    let%bind r = JSONDatabase.execute ~db q in
    return (expect(r) |> toEqual(v))
  )

let valueOfStringExn s = s |> Js.Json.parseExn |> Core.Value.ofJson

let () =

  describe "JSONDatabase" begin fun () ->

    test "/" begin fun () ->
      let q = Q.(void) in
      runQueryAndExpect q (JSONDatabase.root db)
    end;

    test "/region" begin fun () ->
      let q = Q.(void |> nav "region") in
      runQueryAndExpect q (valueOfStringExn {|
        [
          {
            "id": "AMERICA",
            "name": "America"
          },
          {
            "id": "ASIA",
            "name": "Asia"
          }
        ]
      |})
    end;

    test "/region.name" begin fun () ->
      let q = Q.(void |> nav "region" |> nav "name") in
      runQueryAndExpect q (valueOfStringExn {|
        [
          "America",
          "Asia"
        ]
      |})
    end;

    test "{ regions: region }" begin fun () ->
      let q = Q.(
        here
        |> select [
          field ~alias:"regions" (here |> nav "region");
        ]
      ) in
      runQueryAndExpect q (valueOfStringExn {|
        {
          "regions": [
            {
              "id": "AMERICA",
              "name": "America"
            },
            {
              "id": "ASIA",
              "name": "Asia"
            }
          ]
        }
      |})
    end;

    test "{ regionNames: region.name }" begin fun () ->
      let q = Q.(
        here
        |> select [
          field ~alias:"regionNames" (here |> nav "region" |> nav "name");
        ]
      ) in
      runQueryAndExpect q (valueOfStringExn {|
        {
          "regionNames": [
            "America",
            "Asia"
          ]
        }
      |})
    end;

    test "/region[\"ASIA\"]" begin fun () ->
      let q = Q.(void |> nav "region" |> locate (string "ASIA")) in
      runQueryAndExpect q (valueOfStringExn {|
        {
          "id": "ASIA",
          "name": "Asia"
        }
      |})
    end;

    test "/region[\"ASIA\"].nation" begin fun () ->
      let q = Q.(
        void
        |> nav "region"
        |> locate (string "ASIA")
        |> nav "nation"
      ) in
      runQueryAndExpect q (valueOfStringExn {|
        [
          {"id": "RUSSIA", "name": "Russia"},
          {"id": "CHINA", "name": "China"}
        ]
      |})
    end;

    test "/region[\"ASIA\"].nation.name" begin fun () ->
      let q = Q.(
        void
        |> nav "region"
        |> locate (string "ASIA")
        |> nav "nation"
        |> nav "name"
      ) in
      runQueryAndExpect q (valueOfStringExn {|
        [
          "Russia",
          "China"
        ]
      |})
    end;

    test "{ asia: region[\"ASIA\"] }" begin fun () ->
      let q = Q.(
        here
        |> select [
          field ~alias:"asia" (
            here
            |> nav "region"
            |> locate (string "ASIA")
          )
        ]
      ) in
      runQueryAndExpect q (valueOfStringExn {|
        {
          "asia": {
            "id": "ASIA",
            "name": "Asia"
          }
        }
      |})
    end;

    test "{ asia: region[\"ASIA\"].name }" begin fun () ->
      let q = Q.(
        here
        |> select [
          field ~alias:"asia" (
            here
            |> nav "region"
            |> locate (string "ASIA")
            |> nav "name"
          )
        ]
      ) in
      runQueryAndExpect q (valueOfStringExn {|
        {
          "asia": "Asia"
        }
      |})
    end;

    test "{ asiaNations: region[\"ASIA\"].nation }" begin fun () ->
      let q = Q.(
        here
        |> select [
          field ~alias:"asiaNations" (
            here
            |> nav "region"
            |> locate (string "ASIA")
            |> nav "nation"
          )
        ]
      ) in
      runQueryAndExpect q (valueOfStringExn {|
        {
          "asiaNations": [
            {"id": "RUSSIA", "name": "Russia"},
            {"id": "CHINA", "name": "China"}
          ]
        }
      |})
    end;

    test "{ asiaNationNames: region[\"ASIA\"].nation.name }" begin fun () ->
      let q = Q.(
        here
        |> select [
          field ~alias:"asiaNationNames" (
            here
            |> nav "region"
            |> locate (string "ASIA")
            |> nav "nation"
            |> nav "name"
          )
        ]
      ) in
      runQueryAndExpect q (valueOfStringExn {|
        {
          "asiaNationNames": [
            "Russia",
            "China"
          ]
        }
      |})
    end;

    test "{ data: region[\"ASIA\"] { nations: nation } }" begin fun () ->
      let q = Q.(
        here
        |> select [
          field ~alias:"data" (
            here
            |> nav "region"
            |> locate (string "ASIA")
            |> select [
              field ~alias:"nations" (
                here
                |> nav "nation"
              )
            ]
          )
        ]
      ) in
      runQueryAndExpect q (valueOfStringExn {|
        {
          "data": {
            "nations": [
              {"id": "RUSSIA", "name": "Russia"},
              {"id": "CHINA", "name": "China"}
            ]
          }
        }
      |})
    end;

    test "{ data: region[\"ASIA\"] { nationNames: nation.name } }" begin fun () ->
      let q = Q.(
        here
        |> select [
          field ~alias:"data" (
            here
            |> nav "region"
            |> locate (string "ASIA")
            |> select [
              field ~alias:"nationNames" (
                here
                |> nav "nation"
                |> nav "name"
              )
            ]
          )
        ]
      ) in
      runQueryAndExpect q (valueOfStringExn {|
        {
          "data": {
            "nationNames": [
              "Russia",
              "China"
            ]
          }
        }
      |})
    end;

    test "region.nation.region" begin fun () ->
      let q = Q.(
        void
        |> nav "region"
        |> nav "nation"
        |> nav "region"
      ) in
      runQueryAndExpect q (valueOfStringExn {|
        [
          {
            "id": "AMERICA",
            "name": "America"
          },
          {
            "id": "ASIA",
            "name": "Asia"
          },
          {
            "id": "ASIA",
            "name": "Asia"
          }
        ]
      |})
    end;

  end;
