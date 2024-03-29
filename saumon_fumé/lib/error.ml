type details =
  | AlreadyDefined of string
  | CircularDefinition of string
  | InvalidLValue of string
  | InvalidReturn
  | ParseExpected of string
  | TooManyArgs
  | TypeError of string
  | UndefinedVariable of string
  | UnexpectedCharacter of char
  | UnterminatedStrLit
  [@@deriving show]

let pprint_details = function
  | AlreadyDefined name -> "variable " ^ name ^ " already defined in scope"
  | CircularDefinition name -> "variable " ^ name ^ " used in own initializer"
  | InvalidLValue what -> "invalid assignment target: " ^ what
  | InvalidReturn -> "return outside function or method body"
  | ParseExpected what -> "expected " ^ what
  | TooManyArgs -> "more than 255 arguments or parameters"
  | TypeError msg -> "type error: " ^ msg
  | UndefinedVariable name -> "undefined variable " ^ name
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
