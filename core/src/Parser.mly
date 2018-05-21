%token VOID
%token PICK
%token VIEW
%token FORM
%token BAR_CHART
%token UPDATE
%token CREATE
%token RENDER
%token NULL
%token GOTO
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
%token EQ
%token NEQ
%token LT
%token LTE
%token GT
%token GTE
%token AND
%token OR
%token EOF
%token ARROW_RIGHT
%token ALT
%token SEMI

%right ALT
%right SEMI
%right ARROW_RIGHT
%right OR
%right AND
%left LT LTE GT GTE EQ NEQ
%right DOT COLON

%{

  module S = Query.Untyped.Syntax
  module WS = QueryWorkflow.Syntax
  module M = Query.Mutation.Syntax
  module StringMap = Belt.Map.String

  type nav = {
    name : string;
    args : Query.Untyped.Syntax.Arg.arg list;
  }

  let mkGlobalCombinator0 here = function
    | "meta" -> Some (S.meta here)
    | "count" -> Some (S.count here)
    | "first" -> Some (S.first here)
    | _ -> None

  let mkGlobalCombinator1 here arg = function
    | "grow" -> Some (S.grow arg here)
    | "filter" -> Some (S.filter arg here)
    | _ -> None

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
  | name = ID; EQ; node = workflowNode { WS.define name node WS.empty }
  | name = ID; EQ; node = workflowNode; wc = workflow { WS.define name node wc }

workflowNode:
  | LEFT_PAREN; node = workflowNode; RIGHT_PAREN { node }
  | RENDER; q = query; { WS.value q }
  | GOTO; label = ID { WS.label label }
  | q = query; ARROW_RIGHT; node = workflowNode { WS.navigateAnd q node }
  | ALT; left = workflowNode; ALT; right = workflowNode { WS.or_ left right } %prec ALT
  | left = workflowNode; ALT; right = workflowNode { WS.or_ left right } %prec ALT
  | left = workflowNode; SEMI; right = workflowNode { WS.and_ left right } %prec SEMI
  | node = workflowNode; SEMI; { node }

query:
  | VOID { S.void }
  | name = ID { S.nav name S.here }
  | query = queryCombinator1 { query }
  | query = queryCombinator0 { query }
  | COLON; nav = screen { S.screen ~args:nav.args nav.name S.here }
  | COLON; mut = mutation { mut S.here }
  | VOID; name = ID { S.nav name S.void }
  | VOID; COLON; s = screen { S.screen ~args:s.args s.name S.void }
  | VOID; COLON; mut = mutation { mut S.void }
  | parent = query; DOT; name = ID { S.nav name parent }
  | parent = query; COLON; s = screen { S.screen ~args:s.args s.name parent }
  | parent = query; COLON; mut = mutation { mut parent }
  | LEFT_BRACE; RIGHT_BRACE { S.select [] S.here }
  | LEFT_BRACE; s = selectFieldList; RIGHT_BRACE { S.select s S.here }
  | parent = query; LEFT_BRACE; RIGHT_BRACE { S.select [] parent }
  | parent = query; LEFT_BRACE; s = selectFieldList; RIGHT_BRACE { S.select s parent }
  | parent = query; LEFT_BRACKET; id = query RIGHT_BRACKET { S.locate id parent }
  | v = STRING { S.string v }
  | v = NUMBER { S.number v }
  | v = BOOL { S.bool v }
  | NULL { S.null }
  | name = NAME { S.name name }
  | LEFT_PAREN; q = query; RIGHT_PAREN { q }
  | left = query; OR; right = query { S.or_ left right } %prec OR
  | left = query; AND; right = query { S.and_ left right } %prec AND
  | left = query; LT; right = query { S.lessThan left right } %prec LT
  | left = query; GT; right = query { S.greaterThan left right } %prec GT
  | left = query; LTE; right = query { S.lessOrEqThan left right } %prec LTE
  | left = query; GTE; right = query { S.greaterOrEqThan left right } %prec GTE
  | left = query; EQ; right = query { S.eq left right } %prec EQ
  | left = query; NEQ; right = query { S.notEq left right } %prec NEQ

queryCombinator0:
  | parent = query; COLON; name = ID; LEFT_PAREN; RIGHT_PAREN {
      match mkGlobalCombinator0 parent name with
      | Some q -> q
      | None -> $syntaxerror
    }
  | parent = query; COLON; name = ID {
      match mkGlobalCombinator0 parent name with
      | Some q -> q
      | None -> $syntaxerror
    }
  | COLON; name = ID; LEFT_PAREN; RIGHT_PAREN {
      match mkGlobalCombinator0 S.here name with
      | Some q -> q
      | None -> $syntaxerror
    }
  | COLON; name = ID {
      match mkGlobalCombinator0 S.here name with
      | Some q -> q
      | None -> $syntaxerror
    }

queryCombinator1:
  | parent = query; COLON; name = ID; LEFT_PAREN; arg1 = query; RIGHT_PAREN {
      match mkGlobalCombinator1 parent arg1 name with
      | Some q -> q
      | None -> $syntaxerror
    }
  | COLON; name = ID; LEFT_PAREN; arg1 = query; RIGHT_PAREN {
      match mkGlobalCombinator1 S.here arg1 name with
      | Some q -> q
      | None -> $syntaxerror
    }

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
  | name = ID; COLON; q = query { name, M.update q }
  | name = ID; COLON; UPDATE; LEFT_BRACE; ops = opList; RIGHT_BRACE { name, M.updateEntity ops }
  | name = ID; COLON; CREATE; LEFT_BRACE; ops = opList; RIGHT_BRACE { name, M.updateEntity ops }

screen:
  | PICK { {name = "pick"; args = [] } }
  | PICK; LEFT_PAREN; RIGHT_PAREN { {name = "pick"; args = [] } }
  | PICK; LEFT_PAREN; args = argList; RIGHT_PAREN { {name = "pick"; args} }
  | VIEW { {name = "view"; args = [] } }
  | VIEW; LEFT_PAREN; RIGHT_PAREN { {name = "view"; args = [] } }
  | VIEW; LEFT_PAREN; args = argList; RIGHT_PAREN { {name = "view"; args} }
  | FORM { {name = "form"; args = [] } }
  | FORM; LEFT_PAREN; RIGHT_PAREN { {name = "form"; args = [] } }
  | FORM; LEFT_PAREN; args = argList; RIGHT_PAREN { {name = "form"; args} }
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
