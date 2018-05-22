open! Jest
open! Expect

module W = Workflow.Make(struct
  type t = string
  let empty = "empty"
  let append a b = a ^ " -> " ^ b
  let show v = v
end)

module S = W.Syntax
module P = W.Pos

let testRun ?(only=false) name f =
  let test = if only then Only.test else test in
  test name begin fun () ->
    match Run.toResult (f ()) with
    | Common.Result.Ok assertion -> assertion
    | Common.Result.Error (`WorkflowError err) -> fail err
  end

let () =
  describe "Worflow.next" begin fun () ->

    let collectNextValues pos =
      let open Run.Syntax in
      let%bind next = P.next pos in
      let next = next |> List.map (fun pos -> P.value pos) |> Array.of_list in
      return next
    in

    let goto ~label pos =
      let open Run.Syntax in
      let%bind next = P.next pos in
      match List.find (fun pos -> (P.value pos) == label) next with
      | pos -> return pos
      | exception Not_found -> W.workflowError {j|no such position: $label|j}
    in

    testRun "a" begin fun () ->
      let open Run.Syntax in
      let workflow = S.(
        empty
        |> define "main" (value "a")
      ) in
      let%bind pos = P.run ~label:"main" workflow in
      let%bind next = collectNextValues pos in
      return (expect next |> toEqual [|"empty -> a"|])
    end;

    testRun "[a]" begin fun () ->
      let open Run.Syntax in
      let workflow = S.(
        empty
        |> define "main" (seq [value "a"])
      ) in
      let%bind pos = P.run ~label:"main" workflow in
      let%bind next = collectNextValues pos in
      return (expect next |> toEqual [|"empty -> a"|])
    end;

    describe "[a; b]" begin fun () ->
      let workflow = S.(
        empty
        |> define "main" (seq [value "a"; value "b"])
      ) in

      testRun "# ..." begin fun () ->
        let open Run.Syntax in
        let%bind pos = P.run ~label:"main" workflow in
        let%bind next = collectNextValues pos in
        return (expect next |> toEqual [|"empty -> a"|])
      end;

      testRun "a # ..." begin fun () ->
        let open Run.Syntax in
        let%bind pos = P.run ~label:"main" workflow in
        let%bind pos = goto ~label:"empty -> a" pos in
        let%bind next = collectNextValues pos in
        return (expect next |> toEqual [|"empty -> a -> b"|])
      end;

      testRun "a -> b # ..." begin fun () ->
        let open Run.Syntax in
        let%bind pos = P.run ~label:"main" workflow in
        let%bind pos = goto ~label:"empty -> a" pos in
        let%bind pos = goto ~label:"empty -> a -> b" pos in
        let%bind next = collectNextValues pos in
        return (expect next |> toEqual [||])
      end;
    end;

    describe "[a; [b; c]; d]" begin fun () ->
      let workflow = S.(
        empty
        |> define "main" (seq [
          value "a";
          seq [value "b"; value "c"];
          value "d"
        ])
      ) in

      testRun "# ..." begin fun () ->
        let open Run.Syntax in
        let%bind pos = P.run ~label:"main" workflow in
        let%bind next = collectNextValues pos in
        return (expect next |> toEqual [|"empty -> a"|])
      end;

      testRun "a # ..." begin fun () ->
        let open Run.Syntax in
        let%bind pos = P.run ~label:"main" workflow in
        let%bind pos = goto ~label:"empty -> a" pos in
        let%bind next = collectNextValues pos in
        return (expect next |> toEqual [|"empty -> a -> b"|])
      end;

      testRun "a -> b # ..." begin fun () ->
        let open Run.Syntax in
        let%bind pos = P.run ~label:"main" workflow in
        let%bind pos = goto ~label:"empty -> a" pos in
        let%bind pos = goto ~label:"empty -> a -> b" pos in
        let%bind next = collectNextValues pos in
        return (expect next |> toEqual [|"empty -> a -> b -> c"|])
      end;

      testRun "a -> b -> c # ..." begin fun () ->
        let open Run.Syntax in
        let%bind pos = P.run ~label:"main" workflow in
        let%bind pos = goto ~label:"empty -> a" pos in
        let%bind pos = goto ~label:"empty -> a -> b" pos in
        let%bind pos = goto ~label:"empty -> a -> b -> c" pos in
        let%bind next = collectNextValues pos in
        return (expect next |> toEqual [|"empty -> a -> b -> c -> d"|])
      end;

      testRun "a -> b -> c -> d # ..." begin fun () ->
        let open Run.Syntax in
        let%bind pos = P.run ~label:"main" workflow in
        let%bind pos = goto ~label:"empty -> a" pos in
        let%bind pos = goto ~label:"empty -> a -> b" pos in
        let%bind pos = goto ~label:"empty -> a -> b -> c" pos in
        let%bind pos = goto ~label:"empty -> a -> b -> c -> d" pos in
        let%bind next = collectNextValues pos in
        return (expect next |> toEqual [||])
      end;

    end;

    describe "[[a1 | a2]; b]" begin fun () ->

      let workflow = S.(
        empty
        |> define "main" (seq [par [value "a1"; value "a2"]; value "b"])
      ) in

      testRun "# ..." begin fun () ->
        let open Run.Syntax in
        let%bind pos = P.run ~label:"main" workflow in
        let%bind next = collectNextValues pos in
        return (expect next |> toEqual [|"empty -> a1"; "empty -> a2"|])
      end;

      testRun "a1 # ..." begin fun () ->
        let open Run.Syntax in
        let%bind pos = P.run ~label:"main" workflow in
        let%bind pos = goto ~label:"empty -> a1" pos in
        let%bind next = collectNextValues pos in
        return (expect next |> toEqual [|"empty -> a1 -> b"|])
      end;

      testRun "a2 # ..." begin fun () ->
        let open Run.Syntax in
        let%bind pos = P.run ~label:"main" workflow in
        let%bind pos = goto ~label:"empty -> a2" pos in
        let%bind next = collectNextValues pos in
        return (expect next |> toEqual [|"empty -> a2 -> b"|])
      end;

      testRun "a2 -> b # ..." begin fun () ->
        let open Run.Syntax in
        let%bind pos = P.run ~label:"main" workflow in
        let%bind pos = goto ~label:"empty -> a2" pos in
        let%bind pos = goto ~label:"empty -> a2 -> b" pos in
        let%bind next = collectNextValues pos in
        return (expect next |> toEqual [||])
      end;

    end;

    describe "[[a1 | [a2 | a3]]; b]" begin fun () ->

      let workflow = S.(
        empty
        |> define "main" (seq [
          par [value "a1"; par [value "a2"; value "a3"]];
          value "b"
        ])
      ) in

      testRun "# ..." begin fun () ->
        let open Run.Syntax in
        let%bind pos = P.run ~label:"main" workflow in
        let%bind next = collectNextValues pos in
        return (expect next |> toEqual [|"empty -> a1"; "empty -> a2"; "empty -> a3"|])
      end;

      testRun "a1 # ..." begin fun () ->
        let open Run.Syntax in
        let%bind pos = P.run ~label:"main" workflow in
        let%bind pos = goto ~label:"empty -> a1" pos in
        let%bind next = collectNextValues pos in
        return (expect next |> toEqual [|"empty -> a1 -> b"|])
      end;

      testRun "a2 # ..." begin fun () ->
        let open Run.Syntax in
        let%bind pos = P.run ~label:"main" workflow in
        let%bind pos = goto ~label:"empty -> a2" pos in
        let%bind next = collectNextValues pos in
        return (expect next |> toEqual [|"empty -> a2 -> b"|])
      end;

      testRun "a3 # ..." begin fun () ->
        let open Run.Syntax in
        let%bind pos = P.run ~label:"main" workflow in
        let%bind pos = goto ~label:"empty -> a3" pos in
        let%bind next = collectNextValues pos in
        return (expect next |> toEqual [|"empty -> a3 -> b"|])
      end;

      testRun "a2 -> b # ..." begin fun () ->
        let open Run.Syntax in
        let%bind pos = P.run ~label:"main" workflow in
        let%bind pos = goto ~label:"empty -> a2" pos in
        let%bind pos = goto ~label:"empty -> a2 -> b" pos in
        let%bind next = collectNextValues pos in
        return (expect next |> toEqual [||])
      end;

    end;

    describe "main: &then ^ then: a" begin fun () ->

      let workflow = S.(
        empty
        |> define "main" (seq [
          label "then"
        ])
        |> define "then" (seq [
          value "a"
        ])
      ) in

      testRun "# ..." begin fun () ->
        let open Run.Syntax in
        let%bind pos = P.run ~label:"main" workflow in
        let%bind next = collectNextValues pos in
        return (expect next |> toEqual [|"empty -> a"|])
      end;

      testRun "a # ..." begin fun () ->
        let open Run.Syntax in
        let%bind pos = P.run ~label:"main" workflow in
        let%bind pos = goto ~label:"empty -> a" pos in
        let%bind next = collectNextValues pos in
        return (expect next |> toEqual [||])
      end;
    end;

    describe "main: &then | b ^ then: a" begin fun () ->

      let workflow = S.(
        empty
        |> define "main" (seq [
          par [
            label "then";
            value "b";
          ]
        ])
        |> define "then" (seq [
          value "a"
        ])
      ) in

      testRun "# ..." begin fun () ->
        let open Run.Syntax in
        let%bind pos = P.run ~label:"main" workflow in
        let%bind next = collectNextValues pos in
        return (expect next |> toEqual [|"empty -> a"; "empty -> b"|])
      end;

      testRun "a # ..." begin fun () ->
        let open Run.Syntax in
        let%bind pos = P.run ~label:"main" workflow in
        let%bind pos = goto ~label:"empty -> a" pos in
        let%bind next = collectNextValues pos in
        return (expect next |> toEqual [||])
      end;

      testRun "b # ..." begin fun () ->
        let open Run.Syntax in
        let%bind pos = P.run ~label:"main" workflow in
        let%bind pos = goto ~label:"empty -> b" pos in
        let%bind next = collectNextValues pos in
        return (expect next |> toEqual [||])
      end;
    end;

    describe "main: &then | b ^ then: a | &main" begin fun () ->

      let workflow = S.(
        empty
        |> define "main" (seq [
          par [
            label "then";
            value "b";
          ]
        ])
        |> define "then" (seq [
          par [
            value "a";
            label "main";
          ]
        ])
      ) in

      testRun "# ..." begin fun () ->
        let open Run.Syntax in
        let%bind pos = P.run ~label:"main" workflow in
        let%bind next = collectNextValues pos in
        (* TODO: I think "emtpy -> b" shouldn't be duplicated here *)
        return (expect next |> toEqual [|"empty -> a"; "empty -> b"; "empty -> b"|])
      end;

      testRun "a # ..." begin fun () ->
        let open Run.Syntax in
        let%bind pos = P.run ~label:"main" workflow in
        let%bind pos = goto ~label:"empty -> a" pos in
        let%bind next = collectNextValues pos in
        return (expect next |> toEqual [||])
      end;

      testRun "b # ..." begin fun () ->
        let open Run.Syntax in
        let%bind pos = P.run ~label:"main" workflow in
        let%bind pos = goto ~label:"empty -> b" pos in
        let%bind next = collectNextValues pos in
        return (expect next |> toEqual [||])
      end;
    end;

    describe "main: [a; &then] ^ then: [b; &main]" begin fun () ->

      let workflow = S.(
        empty
        |> define "main" (seq [
          seq [
            value "a";
            label "then";
          ]
        ])
        |> define "then" (seq [
          seq [
            value "b";
            label "main";
          ]
        ])
      ) in

      testRun "# ..." begin fun () ->
        let open Run.Syntax in
        let%bind pos = P.run ~label:"main" workflow in
        let%bind next = collectNextValues pos in
        return (expect next |> toEqual [|"empty -> a";|])
      end;

      testRun "a # ..." begin fun () ->
        let open Run.Syntax in
        let%bind pos = P.run ~label:"main" workflow in
        let%bind pos = goto ~label:"empty -> a" pos in
        let%bind next = collectNextValues pos in
        return (expect next |> toEqual [|"empty -> a -> b";|])
      end;

      testRun "a -> b # ..." begin fun () ->
        let open Run.Syntax in
        let%bind pos = P.run ~label:"main" workflow in
        let%bind pos = goto ~label:"empty -> a" pos in
        let%bind pos = goto ~label:"empty -> a -> b" pos in
        let%bind next = collectNextValues pos in
        return (expect next |> toEqual [|"empty -> a -> b -> a";|])
      end;

      testRun "a -> b -> a # ..." begin fun () ->
        let open Run.Syntax in
        let%bind pos = P.run ~label:"main" workflow in
        let%bind pos = goto ~label:"empty -> a" pos in
        let%bind pos = goto ~label:"empty -> a -> b" pos in
        let%bind pos = goto ~label:"empty -> a -> b -> a" pos in
        let%bind next = collectNextValues pos in
        return (expect next |> toEqual [|"empty -> a -> b -> a -> b";|])
      end;
    end;

    describe "main: [a; &then; d] ^ then: [b; c]" begin fun () ->

      let workflow = S.(
        empty
        |> define "main" (seq [
          seq [
            value "a";
            label "then";
            value "d";
          ]
        ])
        |> define "then" (seq [
          seq [
            value "b";
            value "c";
          ]
        ])
      ) in

      testRun "# ..." begin fun () ->
        let open Run.Syntax in
        let%bind pos = P.run ~label:"main" workflow in
        let%bind next = collectNextValues pos in
        return (expect next |> toEqual [|"empty -> a";|])
      end;

      testRun "# a ..." begin fun () ->
        let open Run.Syntax in
        let%bind pos = P.run ~label:"main" workflow in
        let%bind pos = goto ~label:"empty -> a" pos in
        let%bind next = collectNextValues pos in
        return (expect next |> toEqual [|"empty -> a -> b";|])
      end;

      testRun "# a -> b ..." begin fun () ->
        let open Run.Syntax in
        let%bind pos = P.run ~label:"main" workflow in
        let%bind pos = goto ~label:"empty -> a" pos in
        let%bind pos = goto ~label:"empty -> a -> b" pos in
        let%bind next = collectNextValues pos in
        return (expect next |> toEqual [|"empty -> a -> b -> c";|])
      end;

      testRun "# a -> b -> c ..." begin fun () ->
        let open Run.Syntax in
        let%bind pos = P.run ~label:"main" workflow in
        let%bind pos = goto ~label:"empty -> a" pos in
        let%bind pos = goto ~label:"empty -> a -> b" pos in
        let%bind pos = goto ~label:"empty -> a -> b -> c" pos in
        let%bind next = collectNextValues pos in
        return (expect next |> toEqual [|"empty -> a -> b -> c -> d";|])
      end;

      testRun "# a -> b -> c -> d ..." begin fun () ->
        let open Run.Syntax in
        let%bind pos = P.run ~label:"main" workflow in
        let%bind pos = goto ~label:"empty -> a" pos in
        let%bind pos = goto ~label:"empty -> a -> b" pos in
        let%bind pos = goto ~label:"empty -> a -> b -> c" pos in
        let%bind pos = goto ~label:"empty -> a -> b -> c -> d" pos in
        let%bind next = collectNextValues pos in
        return (expect next |> toEqual [||])
      end;

    end;

    describe "main: [a | &then | d] ^ then: [b | c]" begin fun () ->

      let workflow = S.(
        empty
        |> define "main" (seq [
          par [
            value "a";
            label "then";
            value "d";
          ]
        ])
        |> define "then" (seq [
          par [
            value "b";
            value "c";
          ]
        ])
      ) in

      testRun "# ..." begin fun () ->
        let open Run.Syntax in
        let%bind pos = P.run ~label:"main" workflow in
        let%bind next = collectNextValues pos in
        return (expect next |> toEqual [|
          "empty -> a";
          "empty -> b";
          "empty -> c";
          "empty -> d";
        |])
      end;

    end;

    describe "main: [a | &then | d] ^ then: [b ; c]" begin fun () ->

      let workflow = S.(
        empty
        |> define "main" (seq [
          par [
            value "a";
            label "then";
            value "d";
          ]
        ])
        |> define "then" (seq [
          seq [
            value "b";
            value "c";
          ]
        ])
      ) in

      testRun "# ..." begin fun () ->
        let open Run.Syntax in
        let%bind pos = P.run ~label:"main" workflow in
        let%bind next = collectNextValues pos in
        return (expect next |> toEqual [|
          "empty -> a";
          "empty -> b";
          "empty -> d";
        |])
      end;

      testRun "# a ..." begin fun () ->
        let open Run.Syntax in
        let%bind pos = P.run ~label:"main" workflow in
        let%bind pos = goto ~label:"empty -> a" pos in
        let%bind next = collectNextValues pos in
        return (expect next |> toEqual [||])
      end;

      testRun "# b ..." begin fun () ->
        let open Run.Syntax in
        let%bind pos = P.run ~label:"main" workflow in
        let%bind pos = goto ~label:"empty -> b" pos in
        let%bind next = collectNextValues pos in
        return (expect next |> toEqual [|"empty -> b -> c"|])
      end;

      testRun "# b -> c ..." begin fun () ->
        let open Run.Syntax in
        let%bind pos = P.run ~label:"main" workflow in
        let%bind pos = goto ~label:"empty -> b" pos in
        let%bind pos = goto ~label:"empty -> b -> c" pos in
        let%bind next = collectNextValues pos in
        return (expect next |> toEqual [||])
      end;

      testRun "# d ..." begin fun () ->
        let open Run.Syntax in
        let%bind pos = P.run ~label:"main" workflow in
        let%bind pos = goto ~label:"empty -> d" pos in
        let%bind next = collectNextValues pos in
        return (expect next |> toEqual [||])
      end;

    end;

  end;
