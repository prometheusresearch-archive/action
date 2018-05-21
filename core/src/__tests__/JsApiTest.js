// @flow

const JsApi = require('../JsApi.bs');

declare var describe: (string, Function) => void;
declare var test: (string, Function) => void;

describe('JsApi', function() {
  describe('simplest workflow', function() {
    let workflow = JsApi.parseWorkflow(`
      main =
        render region:pick
    `);

    describe('init state', function() {
      let getInitState = () => {
        return JsApi.run(JsApi.db, workflow);
      };

      test('init state', function() {
        let state = getInitState();
        let ui = JsApi.ui(state);
        expect(ui.name).toBe('pick');
      });

      test('running queries: .title', function() {
        let state = getInitState();
        let data = JsApi.query('title', state);
        expect(data).toBe('Pick');
      });

      test('running queries: .data:count', function() {
        let state = getInitState();
        let data = JsApi.query('data:count', state);
        expect(data).toBe(5);
      });

      test('running queries: .value', function() {
        let state = getInitState();
        let data = JsApi.query('value', state);
        expect(data).toBe(null);
      });

      test('pick when no item was selected: next.length is 0', function() {
        let state = getInitState();
        let next = JsApi.next(state);
        expect(next.length).toBe(0);
      });

      test('around.length is 1', function() {
        let state = getInitState();
        let next = JsApi.around(state);
        expect(next.length).toBe(1);
      });
    });

    describe('an item was selected', function() {
      let getInitState = () => {
        let state;
        state = JsApi.run(JsApi.db, workflow);
        state = JsApi.replaceArgs({id: 'ASIA'}, state);
        return state;
      };

      test('value now points to a selected item', function() {
        let state = getInitState();
        let data = JsApi.query('value', state);
        expect(data).not.toBe(null);
        expect((data: any).id).toBe('ASIA');
      });

      test('still no next items (per workflow, as designed)', function() {
        let state = getInitState();
        let next = JsApi.next(state);
        expect(next.length).toBe(0);
      });
    });
  });

  describe('pick-view workflow', function() {
    const workflow = JsApi.parseWorkflow(`
      main =
        render region:pick;
        render value:view;
    `);

    describe('init state', function() {
      let getInitState = () => {
        return JsApi.run(JsApi.db, workflow);
      };

      test('init state', function() {
        let state = getInitState();
        let ui = JsApi.ui(state);
        expect(ui.name).toBe('pick');
      });

      test('running queries: .title', function() {
        let state = getInitState();
        let data = JsApi.query('title', state);
        expect(data).toBe('Pick');
      });

      test('running queries: .data:count', function() {
        let state = getInitState();
        let data = JsApi.query('data:count', state);
        expect(data).toBe(5);
      });

      test('pick when no item was selected: next.length is 0', function() {
        let state = getInitState();
        let next = JsApi.next(state);
        expect(next.length).toBe(0);
      });
    });

    describe('an item was selected', function() {
      let getInitState = () => {
        let state;
        state = JsApi.run(JsApi.db, workflow);
        state = JsApi.replaceArgs({id: 'ASIA'}, state);
        return state;
      };

      test('now next should contain the next one', function() {
        let state = getInitState();
        let next = JsApi.next(state);
        expect(next.length).toBe(1);
        expect(JsApi.ui(next[0]).name).toBe('view');
      });
    });

    describe('on a view', function() {
      let getInitState = () => {
        let state;
        state = JsApi.run(JsApi.db, workflow);
        state = JsApi.replaceArgs({id: 'ASIA'}, state);
        state = JsApi.next(state)[0];
        return state;
      };

      test('query value', function() {
        let state = getInitState();
        let data = JsApi.query('value', state);
        expect(data).not.toBe(null);
        expect((data: any).id).toBe('ASIA');
      });
    });
  });

  describe('workflow with multiple alternatives at the root', function() {
    let workflow = JsApi.parseWorkflow(`
      main =
        | render region:pick
        | render nation:pick
    `);

    describe('init state', function() {
      let getInitState = () => {
        return JsApi.run(JsApi.db, workflow);
      };

      test('init state', function() {
        let state = getInitState();
        let ui = JsApi.ui(state);
        expect(ui.name).toBe('pick');
      });

      test('around.length is 2', function() {
        let state = getInitState();
        let next = JsApi.around(state);
        expect(next.length).toBe(2);
      });
    });
  });
});
