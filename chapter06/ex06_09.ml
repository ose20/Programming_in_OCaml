(* 練習問題6.9 *)
exception Invalid_variant

type ('a, 'b) xml =
    XLf of 'b option
  | XBr of 'a * ('a, 'b) xml list

type token =
    PCDATA of string
  | Open of string
  | Close of string


(* イメージとしては token list を左から一回だけ見ながら xml を構築していく感じ *)
(* xml_of_tokens_with_parent : string -> token list -> (string, string) xml * token list *)
(*  
  token list を受け取り、Close parent まで読み込みながら、そこまでのデータを xml にして、
  Close parent の次から残りのリストと一緒に返す 
*)
(* xmllist_of_tokens_with_parent : string -> token list -> (string, string) xml list * token list *)
(*  
  token list を受け取り、Close parent まで読み込みながら、そこまでのデータを xml list にして、
  Clsoe parent の次から残りのリストと一緒に返す
*)
let rec xml_of_tokens_sub = function
    []  -> (XLf None, [])
  | Open str1 :: Close str2 :: rest  when str1 = str2 
        -> (XBr(str1, [XLf None]), rest)
  | Open str1 :: PCDATA d :: Close str2 :: rest when str1 = str2
        -> (XBr(str1, [XLf (Some d)]), rest)
  | Open str :: rest 
        -> let (xmllist, nxtptr) = xmllist_of_tokens_with_parent str rest in
          (XBr(str, xmllist), nxtptr)
  | PCDATA s :: rest 
        -> (XLf (Some s), rest)
  | _   -> raise Invalid_variant
and xmllist_of_tokens_with_parent parent = function
    [] -> ([], [])
  | (Open str :: rest) as whole -> 
      let (xml, nxtptr) = xml_of_tokens_sub whole in
      let (xmllist, nxtnxt) = xmllist_of_tokens_with_parent parent nxtptr in
      (xml :: xmllist, nxtnxt)
  | Close parent :: rest -> ([], rest)
  | _ -> raise Invalid_variant

(* token list -> (string, string) xml *)
let xml_of_tokens tokens =
  let (xml, _) = xml_of_tokens_sub tokens
  in xml


let tokens1 = [Open "a";
                Open "b";
                Close "b";
                Open "c";
                  PCDATA "Hello";
                Close "c";
              Close "a"]

let xml1 = 
  XBr("a",  [
              XBr("b", [XLf None]);
              XBr("c", [XLf (Some "Hello")])
            ])

let tokens2 =
  [ Open "addressbook";
      Open "person";
        Open "name";
          PCDATA "Taro";
        Close "name";
        Open "tel";
          PCDATA "075-123-4567";
        Close "tel";
      Close "person";
      Open "person";
      Close "person";
      Open "person";
      Close "person"
  ]

let xml2 =
  XBr("addressbook",  [
    XBr("person", [
      XBr("name", [XLf (Some "Taro")]);
      XBr("tel", [XLf (Some "075-123-4567")])
    ]);
    XBr("person", [XLf None]);
    XBr("person", [XLf None])
  ])

let tokens3 =
  [
    Open "a";
      Open "b";
        PCDATA "100";
      Close "b";
      Open "c";
      Close "c";
      Open "d";
      Close "d"
  ]

let xml3 =
  XBr("a", [
    XBr("b", [XLf (Some "100")]);
    XBr("c", [XLf None]);
    XBr("d", [XLf None])
  ])

let tokens4 =
  [
    Open "name";
      PCDATA "Alice";
      PCDATA "Bob";
    Close "name"
  ]

let check1 = xml_of_tokens tokens1 = xml1
let check2 = xml_of_tokens tokens2 = xml2
let check3 = xml_of_tokens tokens3 = xml3
let check4 = xml_of_tokens tokens4 (* Invalid_variant *)