(*** Costs and Resource Metrics (Section 3 of submission) ***)

Require Import calc.
Require Import QArith.

Definition Cost := Q.

(** Resource Metrics **)
Inductive resmetric :=
  { MVar    : Cost;
    MPar    : Cost;
    MConst  : Cost;
    MOp     : Cost;
    MGRead  : nat -> Cost;
    MSRead  : nat -> Cost;
    MIf     : Cost;
    MDiv    : Cost;
    MVWrite : Cost;
    MGWrite : nat -> Cost;
    MSWrite : nat -> Cost;
    MSync   : Cost }.

Infix "++" := Qplus.

(** Monotonicity assumptions **)
Axiom mgread_monotonic : forall (M : resmetric) (n1 n2 : nat),
    (n1 <= n2)%nat -> M.(MGRead) n1 <= M.(MGRead) n2.
Axiom msread_monotonic : forall (M : resmetric) (n1 n2 : nat),
    (n1 <= n2)%nat -> M.(MSRead) n1 <= M.(MSRead) n2.
Axiom mgwrite_monotonic : forall (M : resmetric) (n1 n2 : nat),
    (n1 <= n2)%nat -> M.(MGWrite) n1 <= M.(MGWrite) n2.
Axiom mswrite_monotonic : forall (M : resmetric) (n1 n2 : nat),
    (n1 <= n2)%nat -> M.(MSWrite) n1 <= M.(MSWrite) n2.

(** All costs are positive **)
Axiom MVar_pos : forall M, M.(MVar) >= 0.
Axiom MPar_pos : forall M, M.(MPar) >= 0.
Axiom MConst_pos : forall M, M.(MConst) >= 0.
Axiom MOp_pos : forall M, M.(MOp) >= 0.
Axiom MGRead_pos : forall M n, M.(MGRead) n >= 0.
Axiom MSRead_pos : forall M n, M.(MSRead) n >= 0.
Axiom MIf_pos : forall M, M.(MIf) >= 0.
Axiom MDiv_pos : forall M, M.(MDiv) >= 0.
Axiom MVWrite_pos : forall M, M.(MVWrite) >= 0.
Axiom MGWrite_pos : forall M n, M.(MGWrite) n >= 0.
Axiom MSWrite_pos : forall M n, M.(MSWrite) n >= 0.
Axiom MSync_pos : forall M, M.(MSync) >= 0.

Hint Resolve MVar_pos : cost.
Hint Resolve MPar_pos : cost.
Hint Resolve MConst_pos : cost.
Hint Resolve MOp_pos : cost.
Hint Resolve MGRead_pos : cost.
Hint Resolve MSRead_pos : cost.
Hint Resolve MIf_pos : cost.
Hint Resolve MDiv_pos : cost.
Hint Resolve MVWrite_pos : cost.
Hint Resolve MGWrite_pos : cost.
Hint Resolve MSWrite_pos : cost.
Hint Resolve MSync_pos : cost.
