(* An Implementation of Bruce Schneier's Solitaire Encryption Algorithm
 * Copyright (c) 2003 Eric Mulvaney
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation; either version 2 of the License, or
 * (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program.  If not, see <http://www.gnu.org/licenses/>.
 *)

type card = Card of int | JokerA | JokerB

type key = card array

exception InvalidKey

(* Stuff for working with cards. *)

let card_of_int = function
  | 52 -> JokerA
  | 53 -> JokerB
  | x when x >= 0 && x < 52 -> Card x
  | _ -> assert false

let int_of_card = function
  | Card x -> x
  | JokerA -> 52
  | JokerB -> 53

let card_value = function
  | Card x -> (x+1)
  | _ -> 53

let new_deck () =
  Array.init 54 card_of_int

(* Stuff for generating a keystream. *)

let step_one deck =
  let rec shiftA i =
    if deck.(i) = JokerA then
      let j = (i+1) mod 54 in
      deck.(i) <- deck.(j);
      deck.(j) <- JokerA
    else shiftA (i+1) in
  shiftA 0

let step_two deck =
  let rec shiftB i =
    if deck.(i) = JokerB then
      let j = (i+1) mod 54
      and k = (i+2) mod 54 in
      deck.(i) <- deck.(j);
      deck.(j) <- deck.(k);
      deck.(k) <- JokerB
    else shiftB (i+1) in
  shiftB 0

let step_three deck =
  let rec find_jokers i = function
    | Card _ -> ()
    | _      -> (if !a < 0 then a else b) := i
  and a = ref (-1)
  and b = ref (-1) in
  Array.iteri find_jokers deck;
  let deck' = Array.copy deck
  and p = !a      (* Top-third's length    *)
  and i = !b - !a (* Middle-third's length *)
  and s = 54 - !b (* Bottom-third's length *) in
  Array.blit deck' !b deck 0     s;
  Array.blit deck' !a deck s     i;
  Array.blit deck' 0  deck (s+i) p

let step_four deck =
  let deck' = Array.copy deck
  and a = card_value deck.(53) in
  let b = 53 - a in
  Array.blit deck' a deck 0 b;
  Array.blit deck' 0 deck b a

let rec next deck =
  step_one deck;
  step_two deck;
  step_three deck;
  step_four deck;
  match deck.(card_value deck.(0)) with
    | JokerA | JokerB -> next deck
    | c -> (int_of_card c) mod 26 + 1

let encode deck c =
  let work base =
    let x = Char.code c - base in
    Char.chr ((x + next deck) mod 26 + base) in
  match c with
    | 'A' .. 'Z' -> work (Char.code 'A')
    | 'a' .. 'z' -> work (Char.code 'a')
    | _ -> c

let decode deck c =
  let work base =
    let x = Char.code c - base in
    Char.chr ((x + 26 - next deck) mod 26 + base) in
  match c with 
    | 'A' .. 'Z' -> work (Char.code 'A')
    | 'a' .. 'z' -> work (Char.code 'a')
    | _ -> c

(* Public interface. *)

let random_key () =
  Random.self_init ();
  let deck = new_deck () in
  for i = 0 to 52 do
    let card = deck.(i)
    and j = i + Random.int(53-i) + 1 in
    deck.(i) <- deck.(j);
    deck.(j) <- card
  done; deck

let copy_key deck =
  Array.copy deck

let string_of_key deck =
  let s = String.create 54 in
  for i = 0 to 53 do
    s.[i] <- match int_of_card deck.(i) with
      | 52 -> '1'
      | 53 -> '2'
      | x when x < 26 -> Char.chr (Char.code 'a' + x)
      | x             -> Char.chr (Char.code 'A' + x - 26)
  done; s

let key_of_string s =
  if String.length s <> 54 then
    raise InvalidKey;
  let deck = new_deck ()
  and flag = Array.make 54 false in
  for i = 0 to 53 do
    let x = match s.[i] with
      | 'a' .. 'z' as c -> Char.code c - Char.code 'a'
      | 'A' .. 'Z' as c -> Char.code c - Char.code 'A' + 26
      | '1' -> 52
      | '2' -> 53
      | _ -> raise InvalidKey in
    if not flag.(x) then
      flag.(x) <- true
    else
      raise InvalidKey;
    deck.(i) <- card_of_int x
  done; deck

let encrypt deck s =
  let s' = String.create (String.length s) in
  for i = 0 to String.length s' - 1 do
    s'.[i] <- encode deck s.[i]
  done; s'

let decrypt deck s =
  let s' = String.create (String.length s) in
  for i = 0 to String.length s' - 1 do
    s'.[i] <- decode deck s.[i]
  done; s'

let encrypt_file deck fin fout =
  let rec doit pos =
    try match Char.uppercase (input_char fin) with
      | 'A' .. 'Z' as c ->
        if pos > 0 && pos mod 5 = 0 then
          if pos / 5 mod 10 = 0
          then output_char fout '\n'
          else output_char fout ' ';
        output_char fout (encode deck c);
        doit (pos+1)
      | _ -> doit pos
    with End_of_file ->
      if pos > 0 && pos mod 5 <> 0 then begin
        output_char fout (encode deck 'X');
        doit (pos+1)
      end else
        output_char fout '\n' in
  doit 0

let decrypt_file deck fin fout =
  let rec doit () =
    try output_char fout (decode deck (input_char fin)); doit ()
    with End_of_file -> () in
  doit ()
