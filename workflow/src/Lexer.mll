{
  open Parser

  exception Error of string

  let get = Lexing.lexeme

  let next_line lexbuf =
    let pos = lexbuf.Lexing.lex_curr_p in
    lexbuf.Lexing.lex_curr_p <- {
      pos with
      pos_bol = lexbuf.lex_curr_pos;
      pos_lnum = pos.pos_lnum + 1;
    }
}

let digit = ['0'-'9']
let frac = '.' digit*
let exp = ['e' 'E'] ['-' '+']? digit+
let float = digit* frac? exp?
let white = [' ' '\t']+
let newline = '\r' | '\n' | "\r\n"
let id = ['a'-'z' 'A'-'Z' '_'] ['a'-'z' 'A'-'Z' '0'-'9' '_']*
let name = ['$'] ['a'-'z' 'A'-'Z' '0'-'9' '_']*

rule read = parse
  | white      { read lexbuf }
  | newline    { next_line lexbuf; read lexbuf }
  | '/'        { VOID }
  | "pick"     { PICK }
  | "view"     { VIEW }
  | "barChart" { BAR_CHART }
  | "count"    { COUNT }
  | "first"    { FIRST }
  | "meta"     { META }
  | "render"   { RENDER }
  | "true"     { BOOL true }
  | "false"    { BOOL false }
  | '{'        { LEFT_BRACE }
  | '}'        { RIGHT_BRACE }
  | '('        { LEFT_PAREN }
  | ')'        { RIGHT_PAREN }
  | ':'        { COLON }
  | '.'        { DOT }
  | ','        { COMMA }
  | '<'        { LT }
  | id         { ID (Lexing.lexeme lexbuf) }
  | name       { NAME (Lexing.lexeme lexbuf) }
  | float      { NUMBER (float_of_string (Lexing.lexeme lexbuf)) }
  | '"'        { read_string (Buffer.create 17) lexbuf }
  | _          { read lexbuf }
  | eof        { EOF }

and read_string buf =
  parse
  | '"'       { STRING (Buffer.contents buf) }
  | '\\' '/'  { Buffer.add_char buf '/'; read_string buf lexbuf }
  | '\\' '\\' { Buffer.add_char buf '\\'; read_string buf lexbuf }
  | '\\' 'b'  { Buffer.add_char buf '\b'; read_string buf lexbuf }
  | '\\' 'f'  { Buffer.add_char buf '\012'; read_string buf lexbuf }
  | '\\' 'n'  { Buffer.add_char buf '\n'; read_string buf lexbuf }
  | '\\' 'r'  { Buffer.add_char buf '\r'; read_string buf lexbuf }
  | '\\' 't'  { Buffer.add_char buf '\t'; read_string buf lexbuf }
  | [^ '"' '\\']+
    { Buffer.add_string buf (Lexing.lexeme lexbuf);
      read_string buf lexbuf
    }
  | _ { raise (Error ("Illegal string character: " ^ Lexing.lexeme lexbuf)) }
  | eof { raise (Error ("String is not terminated")) }
