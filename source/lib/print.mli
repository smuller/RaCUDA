(** Printing functions using module [Format] *)

(** {2 Printing functions for standard datatypes (lists,arrays,...)} *)

(** In the following functions, optional arguments [?first], [?sep], [?last]
  denotes the formatting instructions (under the form of a [format] string)
  issued at the beginning, between two elements, and at the end.

  The functional argument(s) indicate(s) how to print elements.
*)

val list :
  ?first:(unit,Format.formatter,unit) format ->
  ?sep:(unit,Format.formatter,unit) format ->
  ?last:(unit,Format.formatter,unit) format ->
  (Format.formatter -> 'a -> unit) ->
  Format.formatter -> 'a list -> unit
  (** Print a list *)

val array :
  ?first:(unit,Format.formatter,unit) format ->
  ?sep:(unit,Format.formatter,unit) format ->
  ?last:(unit,Format.formatter,unit) format ->
  (Format.formatter -> 'a -> unit) ->
  Format.formatter -> 'a array -> unit
  (** Print an array *)

val pair :
  ?first:(unit,Format.formatter,unit) format ->
  ?sep:(unit,Format.formatter,unit) format ->
  ?last:(unit,Format.formatter,unit) format ->
  (Format.formatter -> 'a -> unit) ->
  (Format.formatter -> 'b -> unit) ->
  Format.formatter -> 'a * 'b -> unit
  (** Print a pair *)

val option :
  ?first:(unit,Format.formatter,unit) format ->
  ?last:(unit,Format.formatter,unit) format ->
  (Format.formatter -> 'a -> unit) ->
  Format.formatter -> 'a option -> unit
  (** Print an optional element *)

val hash :
    ?first:(unit, Format.formatter, unit) format ->
    ?sep:(unit, Format.formatter, unit) format ->
    ?last:(unit, Format.formatter, unit) format ->
    ?firstbind:(unit, Format.formatter, unit) format ->
    ?sepbind:(unit, Format.formatter, unit) format ->
    ?lastbind:(unit, Format.formatter, unit) format ->
    (Format.formatter -> 'a -> unit) ->
    (Format.formatter -> 'b -> unit) ->
    Format.formatter -> ('a,'b) Hashtbl.t -> unit
  (** Print an hashtable *)

val weak :
    ?first:(unit, Format.formatter, unit) format ->
    ?sep:(unit, Format.formatter, unit) format ->
    ?last:(unit, Format.formatter, unit) format ->
    (Format.formatter -> 'a -> unit) ->
    Format.formatter -> 'a Weak.t -> unit
  (** Print a weak pointer array *)

(** {2 Useful functions} *)

val string_of_print:
  (Format.formatter -> 'a -> unit) ->
  ('a -> string)
  (** Transforms a printing function into a conversion-to-string function. *)

val print_of_string:
  ('a -> string) ->
  (Format.formatter -> 'a -> unit)
  (** Transforms a conversion-to-string function to a printing function. *)

val sprintf:
  ?margin:int ->
  ('a, Format.formatter, unit, string) format4 -> 'a
  (** Better [sprintf] function than [Format.sprintf], as it takes the same
    kind of formatters as other [Format.Xprintf] functions. *)

val escaped: ?linebreak:char -> string -> string
  (** Escape a string, replacing line breaks by [linebreak] (default
    ['\n']). When used for DOT output, ['\l'] and ['\r'] produces respectively
    left or right justified lines, instead of center justified lines. *)
