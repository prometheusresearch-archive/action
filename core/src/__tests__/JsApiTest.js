// @flow

const JsApi = require('../JsApi.bs');

declare var describe: (string, Function) => void;
declare var test: (string, Function) => void;

describe("JsApi", function() {

  function getInitState(workflow) {
    return JsApi.run(JsApi.db, workflow);
  }

  describe("simplest workflow", function() {
    const workflow = JsApi.parseWorkflow(`
      main =
        render region:pick
    `);

    test("init state", function() {
      let state = getInitState(workflow);
      let ui = JsApi.ui(state);
      expect(ui.name).toBe("pick");
    });

    test("running queries: .title", function() {
      let state = getInitState(workflow);
      let data = JsApi.query("title", state);
      expect(data).toBe("Pick");
    });

    test("running queries: .data:count", function() {
      let state = getInitState(workflow);
      let data = JsApi.query("data:count", state);
      expect(data).toBe(5);
    });

    test("pick when no item was selected: next.length is 0", function() {
      let state = getInitState(workflow);
      let next = JsApi.next(state);
      expect(next.length).toBe(0);
    });
  });

  describe("pick-view workflow", function() {
    const workflow = JsApi.parseWorkflow(`
      main =
        render region:pick;
        render value:view;
    `);

    test("init state", function() {
      let state = getInitState(workflow);
      let ui = JsApi.ui(state);
      expect(ui.name).toBe("pick");
    });

    test("running queries: .title", function() {
      let state = getInitState(workflow);
      let data = JsApi.query("title", state);
      expect(data).toBe("Pick");
    });

    test("running queries: .data:count", function() {
      let state = getInitState(workflow);
      let data = JsApi.query("data:count", state);
      expect(data).toBe(5);
    });

    test("pick when no item was selected: next.length is 0", function() {
      let state = getInitState(workflow);
      let next = JsApi.next(state);
      expect(next.length).toBe(0);
    });

  });

});
