type t = {
  args : Query.Type.arg Common.StringMap.t;
  inputCard : Query.Card.t;
  grow : Query.Untyped.t;
}

module Syntax = struct

  include Query.Type.Syntax.Value
  include Query.Type.Syntax.Card
  include Query.Type.Syntax.Arg

  let screen ?(args=[]) ~inputCard grow =
    let args = Query.Type.Syntax.Arg.ArgSyntax.toMap args in {
      args;
      inputCard;
      grow;
    }

  let has ?(args=[]) ~resolve fieldName fieldCtyp =
    let args = Query.Type.Syntax.Arg.ArgSyntax.toMap args in
    let field = {
      Query.Type.
      fieldName;
      fieldArgs = args;
      fieldCtyp;
    } in field, resolve

end
