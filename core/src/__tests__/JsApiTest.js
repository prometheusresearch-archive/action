const JsApi = require('../JsApi.bs');

describe("JsApi", function() {

  function getInitState(workflow) {
    return JsApi.run(JsApi.db, workflow);
  }

  describe("simplestWorkflow", function() {
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

    test("running queries: .data:coutn", function() {
      let state = getInitState(workflow);
      let data = JsApi.query("data:count", state);
      expect(data).toBe(5);
    });
  });

});
