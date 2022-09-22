(*
 * $Id$
 * Copyright (c) 2003, Hugues Cass� <hugues.casse@laposte.net>
 *
 * Pretty printer of XML document.
 *)


(** Provide types and pretty printing for XML.
*)


(** Attribute representation.
*)
type attr = string * string


(** Representation of nodes.
*)
type node =
  | TEXT of string       (** Simple text *)
  | COM of string       (** Commentary *)
  | PI of string * string     (** Processing instruction *)
  | ELT of string * attr list * node list (** Element *)


(** Representation of an XML document.
*)
type document = {
  version: string; (** Version, usually 1.0 *)
  encoding: string; (** Encoding (only current encoding supported now) *)
  standalone: bool; (** Standalone attribute *)
  element: node;  (** Document root element *)
}


module M = Set.Make(String)
open M

(** [validate_identifiers elt] validates that identifiers in the
    document are unique and non-empty.

    @since 4.1.0  *)
let validate_identifiers toplevel =
  let rec node ids x ~accept ~reject = match x with
  | TEXT _ | COM _ | PI _ -> accept ids
  | ELT (_,attrs,xs) as elt ->
    match List.assoc_opt "id" attrs with
    | None -> nodes ids xs ~accept ~reject
    | Some "" -> reject elt
    | Some id -> if mem id ids
      then reject elt
      else nodes (add id ids) xs ~reject ~accept
  and nodes ids xs ~accept ~reject = match xs with
    | [] -> accept ids
    | x :: xs -> node ids x ~reject ~accept:(fun ids ->
      nodes ids xs ~accept ~reject) in
  node empty toplevel
    ~accept:(fun _ -> Ok toplevel)
    ~reject:(fun elt -> Error elt)

(** [deduplicate elt] traverses [elt] and removes all
    sub-elements with duplicating identifiers.

    When several elements have the same identifier the first (in the
    DSF traversal order) element is preserved and all subsequent are
    removed.

    If all children of a parent are duplicates of some other elements,
    then the father is still preserved with an empty list of children
    (unless it is itself a duplicate of some other element).

    @since 4.1.0  *)
let deduplicate top =
  let module Ids = Set.Make(String) in
  let check_id ids attrs = match List.assoc_opt "id" attrs with
    | None -> Ok ids
    | Some id ->
      if Ids.mem id ids then Error ()
      else Ok (Ids.add id ids) in
  let rec node ids elt ~accept ~reject = match elt with
    | TEXT _ | COM _ | PI _ as elt -> accept ids elt
    | ELT (name,attrs,xs) -> match check_id ids attrs with
      | Error () -> reject ()
      | Ok ids -> nodes ids xs @@ fun ids xs ->
        accept ids (ELT (name,attrs,xs))
  and nodes ids xs accept = match xs with
    | [] -> accept ids []
    | x :: xs ->
      node ids x
        ~reject:(fun () -> nodes ids xs accept)
        ~accept:(fun ids x -> nodes ids xs @@ fun ids xs ->
                  accept ids (x::xs)) in
  node Ids.empty top ~accept:(fun _ x -> x) ~reject:(fun () -> top)



(** Build a simple document with default initialization.
    @param elt Main element of the document.
*)
let new_simple_doc elt: document =
  {
    version = "1.0";
    encoding = "iso-8859-1";
    standalone = true;
    element = deduplicate elt
  }


(** Build a full document.
    @param vers XML version.
    @param enc Document encoding.
    @param sa Stand-alone attribute.
    @param elt Document element.
*)
let new_doc vers enc sa elt: document =
  {
    version = vers;
    encoding = enc;
    standalone = sa;
    element = deduplicate elt
  }


(** Build an attribute.
    @param name Name of the attribute.
    @param cont Content of the attribute.
*)
let new_attr name cont: attr = (name, cont)


(** Build a new element.
    @param name  Name of the element.
    @param attrs Attributes.
    @param children Children nodes.
*)
let new_elt name attrs children = ELT(name, attrs, children)


(** Build a new text node.
    @param text Content of the node.
*)
let new_text text = TEXT text


(** Add children to an element node.
    @param node  Element to add to.
    @param children Children to add.
    @return   Passed element with children added to the end.
*)
let add_children node children =
  match node with
  | ELT (name, attrs, orig_children) ->
    ELT (name, attrs, List.append orig_children children)
  | _ -> raise (Invalid_argument "not an element")


(** Escape the given attribute value for output.
    @param text  Text of the attribute.
    @param quote Quote character used for the attribute, either '"' or '\''.
*)
let escape_attr text quote =
  let buf = Buffer.create 32 in
  let rec perform i =
    if i >= String.length text then Buffer.contents buf else
      begin
        (match String.get text i with
           '&'
           -> Buffer.add_string buf "&amp;"
         | '<'
           -> Buffer.add_string buf "&lt;"
         | c when c = quote
           -> Buffer.add_string buf
                (if quote = '"' then "&quote;" else "&apos;")
         | c
           -> Buffer.add_char buf c);
        perform (i + 1)
      end in
  perform 0


(** Output an attribute.
    @param out Output channel.
    @param name Name of the attribute.
    @param text Value of the attribute.
*)
let output_attr out (name, text) =
  output_char out ' ';
  output_string out name;
  output_string out "=\"";
  output_string out (escape_attr text '"');
  output_char out '"'


(** Output a node on the given channel.
    @param out  Channel to output to.
    @param node  Node to output.
    @param indent Indentation.
*)
let rec output_node out indent node =
  let output str = output_string out str in
  let rec only_text children =
    match children with
      [] -> true
    | (TEXT _)::tl -> only_text tl
    | _ -> false in
  match node with

    TEXT text
    -> output text

  | COM text
    -> begin
        output indent;
        output "<!--";
        output text;
        output "-->"
      end

  | PI (id, data)
    -> begin
        output indent;
        output "<!";
        output id;
        output " ";
        output data;
        output ">"
      end

  | ELT(name, attrs, children)
    -> begin
        output indent;
        output "<";
        output name;
        List.iter (output_attr out) attrs;
        if children = []
        then output "/>"
        else begin
          output ">";
          List.iter (output_node out (indent ^ "\t")) children;
          if not (only_text children)  then output indent;
          output "</";
          output name;
          output ">"
        end;
      end


(** Output an XML document to the given output channel.
    @param out Output channel.
    @param doc Document to output.
*)
let output_doc out doc =
  output_string out "<?xml version=\"";
  output_string out doc.version;
  output_string out "\" encoding=\"";
  output_string out doc.encoding;
  output_string out "\" standalone=\"";
  output_string out (if doc.standalone then "yes" else "no");
  output_string out "\"?>";
  output_node out "\n" doc.element;
  output_char out '\n'


(** Output the given XML document on the standard output.
    @param doc XML document to output.
*)
let output doc = output_doc stdout doc


(** Output the given XML document on the named file.
    @param filename  Path to the file to write to.
    @param doc   XML document to output.
    @raise Sys_error In case of error during opening of the file.
*)
let output_file filename doc =
  let out = open_out filename in
  output_doc out doc;
  close_out out
