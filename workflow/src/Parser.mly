%token VOID
%token PICK
%token VIEW
%token COUNT
%token RENDER
%token NULL
%token DOT
%token COMMA
%token COLON
%token LEFT_BRACE
%token RIGHT_BRACE
%token LEFT_PAREN
%token RIGHT_PAREN
%token <string> ID
%token <string> STRING
%token <float> NUMBER
%token <bool> BOOL
%token EOF

%{

  module S = Core.UntypedQuery.Syntax
  module W = Core.UntypedWorkflow.Syntax
  module StringMap = Belt.Map.String

  type nav = {
    name : string;
    args : Core.UntypedQuery.Syntax.Arg.t list;
  }

%}

%start start
%type <Core.ParseResult.t> start

%%

start:
  | p = program EOF { p }

program:
  | q = query { Core.ParseResult.Query q }
  | w = workflow { Core.ParseResult.Workflow w }

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
  | VOID; name = ID { S.nav name S.void }
  | VOID; COLON; s = screen { S.screen ~args:s.args s.name S.void }
  | COLON; COUNT { S.count S.void }
  | parent = query; COLON; COUNT { S.count parent }
  | parent = query; DOT; name = ID { S.nav name parent }
  | parent = query; COLON; s = screen { S.screen ~args:s.args s.name parent }
  | parent = query; LEFT_BRACE; RIGHT_BRACE { S.select [] parent }
  | parent = query; LEFT_BRACE; s = selectFieldList; RIGHT_BRACE { S.select s parent }
  | LEFT_BRACE; RIGHT_BRACE { S.select [] S.void }
  | LEFT_BRACE; s = selectFieldList; RIGHT_BRACE { S.select s S.void }

nav:
  | name = ID { {name; args = []} }
  | name = ID; LEFT_PAREN; RIGHT_PAREN { {name; args = StringMap.empty} }
  | name = ID; LEFT_PAREN; args = argList; RIGHT_PAREN { {name; args = Some args} }

screen:
  | PICK { {name = "pick"; args = [] } }
  | VIEW { {name = "view"; args = [] } }
  | PICK; LEFT_PAREN; RIGHT_PAREN { {name = "pick"; args = [] } }
  | VIEW; LEFT_PAREN; RIGHT_PAREN { {name = "view"; args = [] } }
  | PICK; LEFT_PAREN; args = argList; RIGHT_PAREN { {name = "pick"; args} }
  | VIEW; LEFT_PAREN; args = argList; RIGHT_PAREN { {name = "view"; args} }

arg:
  | name = ID; COLON; value = STRING { S.arg name (S.string value) }
  | name = ID; COLON; value = NUMBER { S.arg name (S.number value) }
  | name = ID; COLON; value = BOOL { S.arg name (S.bool value) }

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
