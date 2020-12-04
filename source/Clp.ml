(* Copyright (C) 2012 Yosuke Onoue *)

(* Types *)

type t;;

type direction = Maximize | Minimize;;

type row = {
  row_lower : float;
  row_upper : float;
  row_elements : (int * float) array};;

type column = {
  column_obj : float;
  column_lower : float;
  column_upper : float;
  column_elements : (int * float) array};;

(* Creating model *)
external create : unit -> t = "clp_create";;

(* Getters and setters of problem parameters *)
external resize : t -> int -> int -> unit = "clp_resize";;
external number_rows : t -> int = "clp_number_rows";;
external number_columns : t -> int = "clp_number_columns";;
external number_elements : t -> int = "clp_number_elements";;
external direction : t -> direction = "clp_direction";;
external set_direction : t -> direction -> unit = "clp_set_direction";;
external add_rows : t -> row array -> unit = "clp_add_rows";;
external delete_rows : t -> int array -> unit = "clp_delete_rows";;
external add_columns : t -> column array -> unit = "clp_add_columns";;
external delete_columns : t -> int array -> unit = "clp_delete_columns";;
external row_lower : t -> float array = "clp_row_lower";;
external change_row_lower : t -> float array -> unit = "clp_change_row_lower";;
external row_upper : t -> float array = "clp_row_upper";;
external change_row_upper : t -> float array -> unit = "clp_change_row_upper";;
external column_lower : t -> float array = "clp_column_lower";;
external change_column_lower : t -> float array -> unit = "clp_change_column_lower";;
external column_upper : t -> float array = "clp_column_upper";;
external change_column_upper : t -> float array -> unit = "clp_change_column_upper";;
external objective_coefficients : t -> float array = "clp_objective_coefficients";;
external change_objective_coefficients : t -> float array -> unit = "clp_change_objective_coefficients";;

(* Getters and setters of solver parameters*)
external log_level : t -> int = "clp_log_level";;
external set_log_level : t -> int -> unit = "clp_set_log_level";;

(* Solver operations *)
external primal : t -> unit = "clp_primal";;
external dual : t -> unit = "clp_dual";;

external initial_solve : t -> unit = "clp_initial_solve"

(* Retrieving solutions *)
external objective_value : t -> float = "clp_objective_value";;
external primal_row_solution : t -> float array = "clp_primal_row_solution";;
external primal_column_solution : t -> float array = "clp_primal_column_solution";;
external dual_row_solution : t -> float array = "clp_dual_row_solution";;
external dual_column_solution : t -> float array = "clp_dual_column_solution";;

external status : t -> int = "clp_status"


(* buffering wrapper for efficiency *)

let dummycol =
  { column_obj = 0.
  ; column_lower = 0.
  ; column_upper = 0.
  ; column_elements = [| |]
  }
and dummyrow =
  { row_lower = 0.
  ; row_upper = 0.
  ; row_elements = [| |]
  }

let maxcols = 1000
let maxrows = 1000
let cols, ncols = (Array.make maxcols dummycol, ref 0)
let rows, nrows = (Array.make maxrows dummyrow, ref 0)

let st = ref (Obj.magic 0)

let reset () =
  st := create ();
  Array.fill cols 0 maxcols dummycol;
  Array.fill rows 0 maxrows dummyrow;
  nrows := 0;
  ncols := 0

let number_columns () =
  number_columns !st + !ncols

let flush_cols () =
  if !ncols > 0 then begin
    add_columns !st cols;
    ncols := 0;
  end

let add_column col =
  if !ncols = maxcols then
    flush_cols ();
  cols.(!ncols) <- col;
  incr ncols

let flush_rows () =
  if !nrows > 0 then begin
    flush_cols ();
    add_rows !st rows;
    nrows := 0;
  end

let add_row row =
  if !nrows = maxrows then
    flush_rows ();
  rows.(!nrows) <- row;
  incr nrows

let objective_coefficients () =
  flush_rows ();
  objective_coefficients !st

let change_objective_coefficients o =
  change_objective_coefficients !st o

let set_log_level n =
  set_log_level !st n

let initial_solve () =
  flush_rows ();
  initial_solve !st

let primal () =
  flush_rows ();
  primal !st

let status () =
  status !st

let primal_column_solution () =
  primal_column_solution !st
