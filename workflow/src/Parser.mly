%token VOID
%token SCREEN
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

%start query
%type <Core.UntypedQuery.t> query

%{

  module S = Core.UntypedQuery.Syntax
  module A = Core.Arg

  type nav = { name : string; args : Core.Arg.t list option }

%}

%%

query:
  | s = program EOF { s }

program:
  | sl = term { sl }

term:
  | VOID { S.void }
  | nav = nav { S.nav ?args:nav.args nav.name S.here }
  | SCREEN; COLON; nav = nav { S.screen ?args:nav.args nav.name S.here }
  | VOID; nav = nav { S.nav ?args:nav.args nav.name S.void }
  | VOID; SCREEN; COLON; nav = nav { S.screen ?args:nav.args nav.name S.void }
  | parent = term; DOT; nav = nav { S.nav ?args:nav.args nav.name parent }
  | parent = term; DOT; SCREEN; COLON; nav = nav { S.screen ?args:nav.args nav.name parent }
  | parent = term; LEFT_BRACE; RIGHT_BRACE { S.select [] parent }
  | parent = term; LEFT_BRACE; s = selectFieldList; RIGHT_BRACE { S.select s parent }

nav:
  | name = ID { {name; args = None} }
  | name = ID; LEFT_PAREN; RIGHT_PAREN { {name; args = Some []} }
  | name = ID; LEFT_PAREN; args = argList; RIGHT_PAREN { {name; args = Some args} }

arg:
  | name = ID; COLON; value = STRING { A.string name value }
  | name = ID; COLON; value = NUMBER { A.number name value }
  | name = ID; COLON; value = BOOL { A.bool name value }

argList:
  | a = arg { [a] }
  | a = arg; COMMA; as_ = argList { a::as_ }

selectField:
  | t = term { S.field t }
  | alias = ID; COLON; t = term { S.field ~alias t }

selectFieldList:
  | f = selectField { [f] }
  | f = selectField; COMMA; fs = selectFieldList { f::fs }

%%
