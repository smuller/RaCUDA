(** Printing functions for standard datatypes *)

open Format

let list
  ?(first=("[@[":(unit,Format.formatter,unit) format))
  ?(sep = (";@ ":(unit,Format.formatter,unit) format))
  ?(last = ("@]]":(unit,Format.formatter,unit) format))
  (print_elt: Format.formatter -> 'a -> unit)
  (fmt:Format.formatter)
  (list: 'a list)
  : unit
  =
  if list=[] then begin
    fprintf fmt first;
    fprintf fmt last;
  end
  else begin
    fprintf fmt first;
    let rec do_sep = function
      [e] -> print_elt fmt e
      | e::l -> (print_elt fmt e ; fprintf fmt sep; do_sep l)
    | [] -> failwith ""
    in
    do_sep list;
    fprintf fmt last;
  end

let array
  ?(first=("[|@[":(unit,Format.formatter,unit) format))
  ?(sep = (";@ ":(unit,Format.formatter,unit) format))
  ?(last = ("@]|]":(unit,Format.formatter,unit) format))
  (print_elt: Format.formatter -> 'a -> unit)
  (fmt:Format.formatter)
  (array: 'a array)
  : unit
  =
  if array=[||] then begin
    fprintf fmt first;
    fprintf fmt last;
  end
  else begin
    fprintf fmt first;
    let first = ref true in
    Array.iter
      (begin fun e ->
	if !first then first := false else fprintf fmt sep;
	print_elt fmt e
      end)
      array
    ;
    fprintf fmt last;
  end

let pair
  ?(first=("(@[":(unit,Format.formatter,unit) format))
  ?(sep = (",@ ":(unit,Format.formatter,unit) format))
  ?(last = ("@])":(unit,Format.formatter,unit) format))
  (print_elt1: Format.formatter -> 'a -> unit)
  (print_elt2: Format.formatter -> 'b -> unit)
  (fmt:Format.formatter)
  (pair: 'a*'b)
  : unit
  =
  let (e1,e2) = pair in
  fprintf fmt first;
  print_elt1 fmt e1;
  fprintf fmt sep;
  print_elt2 fmt e2;
  fprintf fmt last

let option
    ?(first=("Some(@[":(unit,Format.formatter,unit) format))
    ?(last = ("@])":(unit,Format.formatter,unit) format))
    (print_elt:Format.formatter -> 'a -> unit)
    (fmt:Format.formatter)
    (oelt:'a option)
    =
  match oelt with
  | None -> pp_print_string fmt "None"
  | Some(elt) ->
      fprintf fmt first;
      print_elt fmt elt;
      fprintf fmt last

let hash
  ?(first : (unit, Format.formatter, unit) format = ("[@[<hv>" : (unit, Format.formatter, unit) format))
  ?(sep : (unit, Format.formatter, unit) format = (";@ ":(unit, Format.formatter, unit) format))
  ?(last : (unit, Format.formatter, unit) format = ("@]]":(unit, Format.formatter, unit) format))
  ?(firstbind : (unit, Format.formatter, unit) format = ("" : (unit, Format.formatter, unit) format))
  ?(sepbind : (unit, Format.formatter, unit) format = (" => ":(unit, Format.formatter, unit) format))
  ?(lastbind : (unit, Format.formatter, unit) format = ("":(unit, Format.formatter, unit) format))
  (print_key:Format.formatter -> 'a -> unit)
  (print_data:Format.formatter -> 'b -> unit)
  (formatter:Format.formatter)
  (hash:('a,'b) Hashtbl.t)
  : unit
  =
  Format.fprintf formatter first;
  let firstitem = ref true in
  Hashtbl.iter
    (begin fun key data ->
      if !firstitem then firstitem := false else Format.fprintf formatter sep;
      Format.fprintf formatter firstbind;
      print_key formatter key;
      Format.fprintf formatter sepbind;
      print_data formatter data;
      Format.fprintf formatter lastbind;
    end)
    hash;
  Format.fprintf formatter last

let weak
  ?(first=("[|@[":(unit,Format.formatter,unit) format))
  ?(sep = (";@ ":(unit,Format.formatter,unit) format))
  ?(last = ("@]|]":(unit,Format.formatter,unit) format))
  (print_elt: Format.formatter -> 'a -> unit)
  (fmt:Format.formatter)
  (array: 'a Weak.t)
  : unit
  =
  if (Weak.length array)=0 then begin
    fprintf fmt first;
    fprintf fmt last;
  end
  else begin
    fprintf fmt first;
    let first = ref true in
    for i=0 to pred (Weak.length array) do
      let oelt = Weak.get array i in
      match oelt with
      | None -> ()
      | Some e ->
	  if !first then first := false else fprintf fmt sep;
	  fprintf fmt "%i => %a" i print_elt e
    done;
    fprintf fmt last;
  end

let print_of_string
  (string_of_a:'a -> string)
  :
  (Format.formatter -> 'a -> unit)
  =
  begin fun fmt a -> pp_print_string fmt (string_of_a a) end

let string_of_print
  (print:Format.formatter -> 'a -> unit)
  :
  ('a -> string)
  =
  begin fun a ->
    let buffer = Buffer.create 1024 in
    let formatter = Format.formatter_of_buffer buffer in
    print formatter a;
    pp_print_flush formatter ();
    let res = Buffer.contents buffer in
    Buffer.clear buffer;
    res
  end

let sprintf ?margin format =
  let buffer = Buffer.create 1024 in
  let fmt = Format.formatter_of_buffer buffer in
  begin match margin with
  | Some n -> Format.pp_set_margin fmt n
  | None -> ()
  end;
  Format.kfprintf
    (begin fun fmt ->
      Format.pp_print_flush fmt ();
      let s = Buffer.contents buffer in
      Buffer.clear buffer;
      s
    end)
    fmt
    format

external char_code: char -> int = "%identity"
external char_chr: int -> char = "%identity"

let is_printable c = 32 <= char_code c && char_code c <= 126

let escaped ?(linebreak:char='n') s =
  let n = ref 0 in
  for i = 0 to String.length s - 1 do
    n := !n +
    (match String.unsafe_get s i with
    '"' | '\\' | '\n' | '\t' -> 2
    | c -> if is_printable c then 1 else 4)
  done;
  if !n = String.length s then s else begin
    let s' = Bytes.create !n in
    n := 0;
    for i = 0 to String.length s - 1 do
      begin
	match String.unsafe_get s i with
	('"' | '\\') as c ->
	  Bytes.unsafe_set s' !n '\\'; incr n; Bytes.unsafe_set s' !n c
	| '\n' ->
	    Bytes.unsafe_set s' !n '\\'; incr n; Bytes.unsafe_set s' !n linebreak
	| '\t' ->
	    Bytes.unsafe_set s' !n '\\'; incr n; Bytes.unsafe_set s' !n 't'
	| c ->
	    if is_printable c then
	      Bytes.unsafe_set s' !n c
	    else begin
	      let a = char_code c in
	      Bytes.unsafe_set s' !n '\\';
	      incr n;
	      Bytes.unsafe_set s' !n (char_chr (48 + a / 100));
	      incr n;
	      Bytes.unsafe_set s' !n (char_chr (48 + (a / 10) mod 10));
	      incr n;
	      Bytes.unsafe_set s' !n (char_chr (48 + a mod 10))
	    end
      end;
      incr n
    done;
    Bytes.to_string s'
  end
