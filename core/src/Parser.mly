%token VOID
%token PICK
%token VIEW
%token EDIT
%token BAR_CHART
%token UPDATE
%token CREATE
%token COUNT
%token META
%token FIRST
%token RENDER
%token NULL
%token DOT
%token COMMA
%token COLON
%token LEFT_BRACE
%token RIGHT_BRACE
%token LEFT_PAREN
%token RIGHT_PAREN
%token LEFT_BRACKET
%token RIGHT_BRACKET
%token <string> NAME
%token <string> ID
%token <string> STRING
%token <float> NUMBER
%token <bool> BOOL
%token LT
%token EOF

%{

  module S = Query.Untyped.Syntax
  module W = Workflow.Untyped.Syntax
  module StringMap = Belt.Map.String

  type nav = {
    name : string;
    args : Query.Untyped.Syntax.Arg.arg list;
  }

%}

%start start
%type <ParserResult.t> start

%%

start:
  | p = program EOF { p }

program:
  | q = query { ParserResult.Query q }
  | w = workflow { ParserResult.Workflow w }

workflow:
  | RENDER; LEFT_PAREN; q = query; RIGHT_PAREN { W.render q }
  | w = workflow; LEFT_BRACE; RIGHT_BRACE { w }
  | w = workflow; LEFT_BRACE; ws = workflowList; RIGHT_BRACE { W.andThen ws w }

workflowList:
  | w = workflow { [w] }
  | w = workflow; COMMA { [w] }
  | w = workflow; COMMA; ws = workflowList { w::ws }

query:
  | VOID { S.void }
  | name = ID { S.nav name S.here }
  | COLON; nav = screen { S.screen ~args:nav.args nav.name S.here }
  | COLON; mut = mutation { mut S.here }
  | VOID; name = ID { S.nav name S.void }
  | VOID; COLON; s = screen { S.screen ~args:s.args s.name S.void }
  | VOID; COLON; mut = mutation { mut S.void }
  | COLON; COUNT { S.count S.here }
  | parent = query; COLON; COUNT { S.count parent }
  | COLON; META { S.meta S.here }
  | parent = query; COLON; META { S.meta parent }
  | COLON; FIRST { S.first S.here }
  | parent = query; COLON; FIRST { S.first parent }
  | parent = query; DOT; name = ID { S.nav name parent }
  | parent = query; COLON; s = screen { S.screen ~args:s.args s.name parent }
  | parent = query; COLON; mut = mutation { mut parent }
  | parent = query; LEFT_BRACE; RIGHT_BRACE { S.select [] parent }
  | parent = query; LEFT_BRACE; s = selectFieldList; RIGHT_BRACE { S.select s parent }
  | parent = query; LEFT_BRACKET; id = query RIGHT_BRACKET { S.locate id parent }
  | LEFT_BRACE; RIGHT_BRACE { S.select [] S.here }
  | LEFT_BRACE; s = selectFieldList; RIGHT_BRACE { S.select s S.here }
  | v = STRING { S.string v }
  | v = NUMBER { S.number v }
  | v = BOOL { S.bool v }
  | NULL { S.null }
  | name = NAME { S.name name }
  | left = query; LT; right = query { S.lessThan left right }

nav:
  | name = ID { {name; args = []} }
  | name = ID; LEFT_PAREN; RIGHT_PAREN { {name; args = StringMap.empty} }
  | name = ID; LEFT_PAREN; args = argList; RIGHT_PAREN { {name; args = Some args} }

mutation:
  | UPDATE; LEFT_BRACE; RIGHT_BRACE { (fun query -> S.update [] query)  }
  | UPDATE; LEFT_BRACE; ops = opList; RIGHT_BRACE { (fun query -> S.update ops query) }
  | CREATE; LEFT_BRACE; RIGHT_BRACE { (fun query -> S.create [] query)  }
  | CREATE; LEFT_BRACE; ops = opList; RIGHT_BRACE { (fun query -> S.create ops query) }

opList:
  | op = op { [op] }
  | op = op; COMMA { [op] }
  | op = op; COMMA; ops = opList { op::ops }

op:
  | name = ID; COLON; q = query { name, S.opUpdate q }
  | name = ID; COLON; q = query { name, S.opUpdate q }
  | name = ID; COLON; UPDATE; LEFT_BRACE; ops = opList; RIGHT_BRACE { name, S.opUpdateEntity ops }
  | name = ID; COLON; CREATE; LEFT_BRACE; ops = opList; RIGHT_BRACE { name, S.opCreateEntity ops }

screen:
  | PICK { {name = "pick"; args = [] } }
  | PICK; LEFT_PAREN; RIGHT_PAREN { {name = "pick"; args = [] } }
  | PICK; LEFT_PAREN; args = argList; RIGHT_PAREN { {name = "pick"; args} }
  | VIEW { {name = "view"; args = [] } }
  | VIEW; LEFT_PAREN; RIGHT_PAREN { {name = "view"; args = [] } }
  | VIEW; LEFT_PAREN; args = argList; RIGHT_PAREN { {name = "view"; args} }
  | EDIT { {name = "edit"; args = [] } }
  | EDIT; LEFT_PAREN; RIGHT_PAREN { {name = "edit"; args = [] } }
  | EDIT; LEFT_PAREN; args = argList; RIGHT_PAREN { {name = "edit"; args} }
  | BAR_CHART { {name = "barChart"; args = [] } }
  | BAR_CHART; LEFT_PAREN; RIGHT_PAREN { {name = "barChart"; args = [] } }
  | BAR_CHART; LEFT_PAREN; args = argList; RIGHT_PAREN { {name = "barChart"; args} }

arg:
  | name = ID; COLON; q = query { S.arg name q }

argList:
  | a = arg { [a] }
  | a = arg; COMMA { [a] }
  | a = arg; COMMA; as_ = argList { a::as_ }

selectField:
  | q = query { S.field q }
  | alias = ID; COLON; q = query { S.field ~alias q }

selectFieldList:
  | f = selectField { [f] }
  | f = selectField; COMMA { [f] }
  | f = selectField; COMMA; fs = selectFieldList { f::fs }

%%
