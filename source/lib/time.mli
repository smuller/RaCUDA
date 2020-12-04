(** Small module to compute the duration of computations *)

val wrap_duration : float ref -> (unit -> 'a) -> 'a
  (** [wrap_duration duration f] executes the function [f] and stores into
     [!duration] the time spent in [f], in seconds. If [f] raises an
     exception, the exception is transmitted and the computed duration is
     still valid. *)

val wrap_duration_add : float ref -> (unit -> 'a) -> 'a
  (** Similar to [wrap_duration], but here the time spent in [f] is added to
     the value [!duration]. *)

