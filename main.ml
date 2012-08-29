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

open Sol

let do_encrypt = ref true
and did_nothing = ref true
and key_file = ref ""
and key = ref (random_key ())
and key_ready = ref false

let register_key filename =
  key_file := filename;
  key_ready := false

let generate_key () =
  key := random_key ();
  key_ready := true;
  did_nothing := false;
  if !key_file = "" then
    raise (Arg.Bad "A key-file must be specified");
  try
    let fout = open_out !key_file in
    output_string fout (string_of_key !key);
    output_char   fout '\n';
    close_out fout
  with Sys_error _ ->
    failwith "I can't write to the key-file."

let load_key () =
  if not !key_ready then
    if !key_file = "" then
      raise (Arg.Bad "A key-file must be specified");
  try
    let fin = open_in !key_file in begin
      try key := key_of_string (input_line fin)
      with InvalidKey ->
	failwith "Invalid key in key-file."
    end;
    close_in fin;
    key_ready := true;
  with Sys_error _ ->
    failwith "I can't read from the key-file."

let process_file infile =
  load_key ();
  did_nothing := false;
  let outfile = String.concat "" [infile; ".out"] in
  try
    let fin  = open_in  infile
    and fout = open_out outfile in
    (if !do_encrypt then encrypt_file else decrypt_file) !key fin fout;
    close_out fout;
    close_in  fin
  with Sys_error _ ->
    prerr_string  "I can't process this file: ";
    prerr_endline infile
		
let _ = 
  let do_update = ref false in
  let usage_line = "usage: (-k file) [options] (files...)"
  and args =
    [("-k", Arg.String register_key, "Specifies the key-file.");
     ("-u", Arg.Set    do_update,    "Update key-file when done.");
     ("-g", Arg.Unit   generate_key, "Generate a random key.");
     ("-e", Arg.Set    do_encrypt,   "Encrypt files [default].");
     ("-d", Arg.Clear  do_encrypt,   "Decrypt files.")] in
  Arg.parse args process_file usage_line;
  if !did_nothing then
    Arg.usage args usage_line
  else if !do_update && !key_ready then
    try
      let fout = open_out !key_file in
      output_string fout (string_of_key !key);
      output_char   fout '\n';
      close_out fout
    with Sys_error _ ->
      failwith "I can't update the key-file."
