type details =
  | ParseExpected of string
  | TypeError of string
  | UnexpectedCharacter of char
  | UnterminatedStrLit
  [@@deriving show]

let pprint_details = function
  | ParseExpected what -> "expected " ^ what
  | TypeError msg -> "type error: " ^ msg
  | UnexpectedCharacter c -> "unexpected character " ^ String.make 1 c
  | UnterminatedStrLit -> "unterminated string literal"

type t = { line: int option; lexeme: string option; details: details }
  [@@deriving show]

let pprint { line; lexeme; details } =
  let start = match line with
    | Some l -> "[line " ^ string_of_int l ^ "] Error"
    | None -> "[end of input] Error"
  in
  let where = match lexeme with
    | Some l -> " at \"" ^ l ^ "\""
    | None -> ""
  in
  start ^ where ^ ": " ^ pprint_details details
