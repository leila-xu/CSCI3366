
(* file: main.ml
   author: Bob Muller, Joseph Tassarotti

   CSCI 3366 Programming Languages

   Problem set 3. To run, from parent directory:

   > dune exec bin/main.exe test
*)
open Typ

(******************************************************************
   Problem 1: 2 points

   In the file token.ml, write the function tokenizer:

   Token.tokenizer : string -> Token.t list

   Legal inputs are parentheses, +, -, *, ^ and unsigned integers expressed in base 10, base 2, or base 16.
   White space includes spaces, tabs \t and newlines \n.
*)

(******************************************************************
   Problem 2: 4 points

   In the file unify.ml, write the function disagreement:

   Unify.disagreement : Typ.t list -> Typ.t list
*)

(*********************************************************************
   Problem 3: 4 points

   In the file subst.ml, write the function compose:

   Subst.compose : Subst.t -> Subst.t -> Subst.t

   Let
   θ = {τ1/x1, ..., τm/xm}
   λ = {u1/y1, ..., un/yn}

   be substitutions. The composition of θ and λ, notation
   θ o λ, is obtained from the substitution

   {τ1λ/x1, ..., τmλ/xm, u1/y1, ..., un/yn}

   by omitting any pair

   1. τkλ/xk for which τkλ = xk and
   2. uj/yj for which yj in {x1, ..., xm}.
*)

(* Testing ********************************************************

   Some test data
*)
let v0 = Lib.fresh()
let v1 = Lib.fresh()
let v2 = Lib.fresh()

(*            t0 = ->        t1 = ->
                  /  \           /  \
                v0   ->         C    v1
                    /  \
                   C    C
*)
let t0 = Arrow { from = Var v0
               ; too  = Arrow { from = C
                              ; too  = C
                              }
               }
let t1 = Arrow { from = C
               ; too  = Var v1
               }
let t2 = Arrow { from = t1
               ; too  = Arrow { from = C
                              ; too  = C
                              }
               }
let t3 = Arrow { from = C
               ; too  = t0
               }

let s0 = Subst.fromList []
let s1 = Subst.fromList [(t0, 1); (t1, 0)]
let s2 = Subst.fromList [(Var v0, v1)]

let (x, y, z, a, b) = (1, 2, 3, C, C)

let theta = Subst.fromList [ (Arrow { from=C; too=Var y }, x)
                           ; (Var z, y)
                           ]
let lambda = Subst.fromList [ (a, x); (b, y); (Var y, z) ]

let answer = Subst.fromList [ (Arrow {from = C; too = b}, x)
                            ; (Var y, z)
                            ]

(* Part 1: test tokenizer
*)
let tokenizerTest1 () =
  Token.tokenizer "34 + 4 ^ 21" =
                            [ Token.Int 34
                            ; Token.Plus
                            ; Token.Int 4
                            ; Token.Power
                            ; Token.Int 21
                            ]
let tokenizerTest2 () =
  Token.tokenizer ")() 4" = [ Token.RPar
                            ; Token.LPar
                            ; Token.RPar
                            ; Token.Int 4
                            ]
let tokenizerTest3 () =
  Token.tokenizer "0b101 )() 0xF1" = [ Token.Int 5
                            ; Token.RPar
                            ; Token.LPar
                            ; Token.RPar
                            ; Token.Int 241
 ]

let tokenizerTest4 () =
  Token.tokenizer "0b10 + 47 ^ 0x99" =
                            [ Token.Int 2
                            ; Token.Plus
                            ; Token.Int 47
                            ; Token.Power
                            ; Token.Int 153
                            ]
let tokenizerTests () =
  Lib.run_test "tokenizer test1" tokenizerTest1 ;
  Lib.run_test "tokenizer test2" tokenizerTest2 ;
  Lib.run_test "tokenizer test3" tokenizerTest3 ;
  Lib.run_test "tokenizer test4" tokenizerTest4

(* Part 2: test disagreement
*)
let disagreementTest1 () =
  Unify.disagreement [t0; t1; t2] = [Var v0; C; t1]
let disagreementTest2 () =
  Unify.disagreement [t1; t3] = [Var v1; t0]
let disagreementTests () =
  Lib.run_test "disagreement test1" disagreementTest1 ;
  Lib.run_test "disagreement test2" disagreementTest2

(* Part 3: test compose
*)
let composeTest1 () = Subst.compose theta lambda = answer
let composeTest2 () = Subst.compose s0 s0 = s0
let composeTests () =
  Lib.run_test "substitution composition test1" composeTest1 ;
  Lib.run_test "substitution composition test2" composeTest2

(*******************************************************************)

type part =
          | Two   (* tokenizer *)
          | Three (* disagreement *)
          | Four  (* compose *)

let test part =
  match part with
  | Two   -> tokenizerTests()
  | Three -> disagreementTests()
  | Four  -> composeTests()

(* OK, run the actual tests *)
let run () =
  let () = test Two in
  let () = test Three in
  let () = test Four
  in
  ()

let _ =
  if (Array.length Sys.argv = 2 && Sys.argv.(1) = "test") then
    run ()
  else
    ()
