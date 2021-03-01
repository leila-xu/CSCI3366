type t =
  | LPar                  (* '(' *)
  | RPar                  (* ')' *)
  | Plus                  (* '+' *)
  | Times                 (* '*' *)
  | Power                 (* '^' *)
  | Int of int

(* tokenizer : string -> t list
*)

let isDigit c = ('0' <= c) && (c <= '9')
let toInt dc = (Char.code dc) - (Char.code '0')

let rec makeNumber i chars =
  match chars with
  | [] -> Int i, []
  | ch::chars ->
    (match isDigit ch with
      | true ->
        let d = (toInt ch)
        in
        makeNumber (i*10+d) chars
      | false -> Int i, chars (*giving remaining chars back*)
    );;

let rec makeBinary c chars =
  match chars with
  | '0'::cs -> makeBinary (List.append c ['0']) cs
  | '1'::cs -> makeBinary (List.append c ['1']) cs
  | cs -> c, cs

let rec makeHex c chars =
  match chars with
  | '0'::cs -> makeHex (List.append c ['0']) cs
  | '1'::cs -> makeHex (List.append c ['1']) cs
  | '2'::cs -> makeHex (List.append c ['2']) cs
  | '3'::cs -> makeHex (List.append c ['3']) cs
  | '4'::cs -> makeHex (List.append c ['4']) cs
  | '5'::cs -> makeHex (List.append c ['5']) cs
  | '6'::cs -> makeHex (List.append c ['6']) cs
  | '7'::cs -> makeHex (List.append c ['7']) cs
  | '8'::cs -> makeHex (List.append c ['8']) cs
  | '9'::cs -> makeHex (List.append c ['9']) cs
  | 'A'::cs -> makeHex (List.append c ['A']) cs
  | 'B'::cs -> makeHex (List.append c ['B']) cs
  | 'C'::cs -> makeHex (List.append c ['C']) cs
  | 'D'::cs -> makeHex (List.append c ['D']) cs
  | 'E'::cs -> makeHex (List.append c ['E']) cs
  | 'F'::cs -> makeHex (List.append c ['F']) cs
  | cs -> c, cs

let string_of_chars chars = (* char list to string converter *)
  let buf = Buffer.create 16 in
  List.iter (Buffer.add_char buf) chars;
  Buffer.contents buf

let tokenizer s =
  let rec repeat chars =
    match chars with
    | '('::cs -> LPar::repeat cs
    | ')'::cs -> RPar::repeat cs
    | '+'::cs -> Plus::repeat cs
    | '*'::cs -> Times::repeat cs
    | '^'::cs -> Power::repeat cs
    | ' '::cs -> repeat cs
    | '0'::'b'::cs ->
      let (token, chars) = makeBinary [] cs
      in Int (int_of_string("0b" ^ string_of_chars token))::repeat chars
    | '0'::'x'::cs ->
      let (token, chars) = makeHex [] cs
      in Int (int_of_string("0x" ^ string_of_chars token))::repeat chars
    | other :: chars -> (*other implies it's a number we're dealing with now *)
      (match isDigit other with
       | true ->
           let (token, chars) = makeNumber (toInt other) chars
           in
           token :: repeat chars
       | false -> repeat chars
      )
    | _ -> []
in repeat (Lib.explode s);;
