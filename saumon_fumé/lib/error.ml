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

type error = { lexeme: string option; details: details }

let pprint_error { lexeme; details } =
  let where = match lexeme with
    | Some l -> " at \"" ^ l ^ "\""
    | None -> ""
  in
  "Error" ^ where ^ ": " ^ pprint_details details

type t = error Located.t

let pprint { Located.item; loc } =
  Located.pprint_location loc ^ " " ^ pprint_error item
