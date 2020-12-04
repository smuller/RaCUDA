(*** miniCUDA definition (Section 3 of submission) ***)

Require Import Arith.PeanoNat.
Include Nat.

(** Preliminaries **)

Definition var := nat.
Definition threads := list nat.

Inductive type : Type :=
| TNat : type
| TBool : type
| TAbs : type
.

Inductive value : Type :=
| VNat : nat -> value
| VBool : bool -> value
| VAbs : value
.

Definition result := list (nat * value).

Definition veq (v1 v2: value) : bool :=
  match (v1, v2) with
  | (VNat n1, VNat n2) => eqb n1 n2
  | (VBool true, VBool true) => true
  | (VBool false, VBool false) => true
  | (VAbs, VAbs) => true
  | _ => false
  end.

Lemma veq_correct : forall v1 v2,
    veq v1 v2 = true <-> v1 = v2.
Proof.
  intros v1 v2.
  split.
  intro.
  induction v1, v2; inversion H; try (induction b; discriminate).
  apply f_equal.
  unfold veq in H.
  apply eqb_eq in H.
  assumption.
  induction b, b0; try eauto; try discriminate.
  reflexivity.

  intro.
  induction v1, v2; inversion H; unfold veq; try eauto.
  subst n0.
  apply eqb_eq.
  reflexivity.
  induction b0; reflexivity.
Qed.

(** Syntax **)

Inductive opd : Type :=
| OVar   : var -> opd
| OPar   : var -> opd
| OConst : value -> opd
| OTid   : opd
| ORes  : result -> opd
.

Inductive arrayty : Type :=
| Global : arrayty
| Shared : arrayty
.

Inductive exp : Type :=
| ERes  : result -> exp
| EOpd  : opd -> exp
| EOp   : opd -> opd -> exp
| ERead : arrayty -> var -> opd -> exp
.

Inductive stmt : Type :=
| SSkip   : stmt
| SVWrite : var -> exp -> stmt
| SAWrite : arrayty -> var -> opd -> exp -> stmt
| SIf     : exp -> stmt -> stmt -> stmt
| SWhile  : exp -> stmt -> stmt
| SSeq    : stmt -> stmt -> stmt
| SSync   : stmt
| SSwitch : bool -> threads -> stmt -> stmt (* free? *)
.

(* Does a statement write to a variable? *)
Fixpoint writesto s x :=
  match s with
    SSkip => False
  | SVWrite x' _ => x = x'
  | SAWrite _ _ _ _ => False
  | SIf _ s1 s2 => (writesto s1 x) \/ (writesto s2 x)
  | SWhile _ s => (writesto s x)
  | SSeq s1 s2 => (writesto s1 x) \/ (writesto s2 x)
  | SSync => False
  | SSwitch _ _ _ => False
  end.

(** Static semantics **)
Inductive typed_value : value -> type -> Prop :=
| VSNat : forall n, typed_value (VNat n) TNat
| VSBool : forall b, typed_value (VBool b) TBool
| VSAbs : typed_value VAbs TAbs
.

Inductive typed_opd : (var -> type) -> opd -> type -> Prop :=
| OSVar : forall x S, typed_opd S (OVar x) (S x)
| OSPar : forall x S, typed_opd S (OPar x) (S x)
| OSConst : forall S v t, typed_value v t -> typed_opd S (OConst v) t
| OSTid : forall S, typed_opd S (OTid) TNat
.

Inductive typed_exp : (var -> type) -> exp -> type -> Prop :=
| ESOpd : forall S o t, typed_opd S o t -> typed_exp S (EOpd o) t
| ESOp : forall S o1 o2 t,
    typed_opd S o1 t -> typed_opd S o2 t -> typed_exp S (EOp o1 o2) t
| ESRead : forall S aty A o,
    typed_opd S o TNat -> typed_exp S (ERead aty A o) (S A)
.

Inductive valid : (var -> type) -> stmt -> Prop :=
| SSSkip : forall S, valid S SSkip
| SSVWrite : forall S x e,
    typed_exp S e (S x) -> valid S (SVWrite x e)
| SSAWrite : forall S aty A o e,
    typed_opd S o TNat -> typed_exp S e (S A) ->
    valid S (SAWrite aty A o e)
| SSIf : forall S e s1 s2,
    typed_exp S e TBool ->
    valid S s1 ->
    valid S s2 ->
    valid S (SIf e s1 s2)
| SSWhile : forall S e s,
    typed_exp S e TBool ->
    valid S s ->
    valid S (SWhile e s)
| SSSeq : forall S s1 s2,
    valid S s1 ->
    valid S s2 ->
    valid S (SSeq s1 s2)
.
