(**
 * Tests for QueryWorkflow
 *)

open! Jest
open! Expect
open! Expect.Operators

module Q = Query.Untyped.Syntax
module W = QueryWorkflow.Syntax

let expectRuns comp =
  match Run.toResult comp with
  | Js.Result.Ok () -> pass
  | Js.Result.Error (`WorkflowError err) -> fail ("WorkflowError: " ^ err)
  | Js.Result.Error (`DatabaseError err) -> fail err
  | Js.Result.Error (`QueryTypeError err) -> fail err

let () =

  describe "QueryWorkflowTest" begin fun () ->

    let simpleWorkflow =
      let pickRegion =
        Q.(void |> nav "region" |> screen "pick")
      in
      let view =
        Q.(here |> nav "value" |> screen "view")
      in
      W.(
        empty
        |> define "main" (seq [value pickRegion; value view])
      ) in

    test "simple: initial state" begin fun () ->
      expectRuns (
        let open Run.Syntax in
        let%bind _init = QueryWorkflow.run ~db:(Config.db) simpleWorkflow in
        return ()
      )
    end;

    test "simple: next state is empty if not args given" begin fun () ->
      expectRuns (
        let open Run.Syntax in
        let%bind init = QueryWorkflow.run ~db:(Config.db) simpleWorkflow in
        let%bind [] = QueryWorkflow.next init in
        return ()
      )
    end;

    test "simple: next state is defined if args are given" begin fun () ->
      expectRuns (
        let open Run.Syntax in
        let%bind init = QueryWorkflow.run ~db:(Config.db) simpleWorkflow in
        let initWithArgs =
          let args = [Q.(arg "id" (string "ASIA"))] in
          QueryWorkflow.replaceArgs args init
        in
        let%bind [state] = QueryWorkflow.next initWithArgs in
        return ()
      )
    end;

    test "simple: no available states after view" begin fun () ->
      expectRuns (
        let open Run.Syntax in
        let%bind init = QueryWorkflow.run ~db:(Config.db) simpleWorkflow in
        let initWithArgs =
          let args = [Q.(arg "id" (string "ASIA"))] in
          QueryWorkflow.replaceArgs args init
        in
        let%bind [state] = QueryWorkflow.next initWithArgs in
        let%bind [] = QueryWorkflow.next state in
        return ()
      )
    end;

  end
