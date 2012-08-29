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

type key

exception InvalidKey

val random_key    : unit -> key
val copy_key      : key -> key
val string_of_key : key -> string
val key_of_string : string -> key

val encrypt : key -> string -> string
val decrypt : key -> string -> string

val encrypt_file : key -> in_channel -> out_channel -> unit
val decrypt_file : key -> in_channel -> out_channel -> unit
