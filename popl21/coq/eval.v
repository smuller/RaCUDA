(*** miniCUDA operational semantics (Section 3 of submission) ***)

Require Import Coq.Lists.List.
Include ListNotations.

Require Import calc.
Require Import QArith.
Require Import cost.

Inductive uniq_t : threads -> Prop :=
| UTNil : uniq_t []
| UTCons : forall a T, ~ In a T -> uniq_t T -> uniq_t (a::T).

Inductive uniq_r {A: Type} : list (nat * A) -> Prop :=
| URNil : uniq_r []
| URCons : forall tv R,
    (~ exists v', In (fst tv, v') R) -> uniq_r R -> uniq_r (tv::R).

Lemma map_pres_uniqtr : forall (T: threads) (f: nat -> nat * value),
    uniq_t T ->
    (forall t, fst (f t) = t) ->
    uniq_r (map f T).
Proof.
  intros.
  induction H.

  compute.
  try constructor.

  compute.
  constructor.
  intro.
  destruct H2 as [v'].
  apply H.
  destruct (@in_map_iff nat (nat*value) f T (fst (f a), v')).
  destruct (H3 H2).
  destruct H5.
  rewrite H0 in H5.
  assert (fst (f x) = a).
  rewrite H5.
  reflexivity.
  rewrite H0 in H7.
  subst x.
  assumption.

  assumption.
Qed.

Lemma filter_pres_uniqr : forall (R: result) (f: nat * value -> bool),
    uniq_r R ->
    uniq_r (filter f R).
Proof.
  intros.
  induction H.

  compute.
  constructor.

  compute.
  case (f tv).
  constructor.
  intro.
  destruct H1.
  assert ((fix filter (l : list (nat * value)) : list (nat * value) :=
             match l with
             | [] => []
             | x :: l0 => if f x then x :: filter l0 else filter l0
             end) R = filter f R).
  reflexivity.
  rewrite H2 in H1.
  apply filter_In in H1.
  destruct H1.
  apply H.
  exists x.
  assumption.
  assumption.
  assumption.
Qed.

Lemma filter_pres_uniqt : forall (T: threads) (f: nat -> bool),
    uniq_t T ->
    uniq_t (filter f T).
Proof.
  intros.
  induction H.

  compute.
  constructor.

  compute.
  case (f a).
  constructor.
  intro.
  assert ((fix filter (l : list nat) : list nat :=
             match l with
             | [] => []
             | x :: l0 => if f x then x :: filter l0 else filter l0
             end) T = filter f T).
  reflexivity.
  rewrite H2 in H1.
  apply filter_In in H1.
  destruct H1.
  apply H.
  assumption.
  assumption.
  assumption.
Qed.

Lemma map_pres_uniqr : forall (R: result) (f: nat * value -> nat * value),
    uniq_r R ->
    (forall tv, fst (f tv) = fst tv) ->
    uniq_r (map f R).
Proof.
  intros.
  induction H.

  compute.
  constructor.

  compute.
  assert ((fix map (l : list (nat * value)) : list (nat * value) :=
            match l with
            | [] => []
            | a :: t => f a :: map t
            end) R = map f R).
  reflexivity.
  rewrite H2.
  constructor.
  intro.
  destruct H3.
  apply H.
  apply in_map_iff in H3.
  destruct H3.
  destruct H3.
  rewrite H0 in H3.
  assert (fst (f x0) = fst tv).
  rewrite H3.
  reflexivity.
  rewrite H0 in H5.
  inversion H5.
  exists (snd x0).
  rewrite <- surjective_pairing.
  assumption.

  assumption.
Qed.

Lemma map_pres_uniqrt : forall (R: result),
    uniq_r R ->
    uniq_t (map (fun tv => fst tv) R).
Proof.
  intros.
  induction H.
  compute.
  constructor.

  compute.
  assert ((let (x, _) := tv in x)
      :: (fix map (l : list (nat * value)) : list nat :=
            match l with
            | [] => []
            | a :: t => (let (x, _) := a in x) :: map t
            end) R = (fst tv)::(map (fun tv => fst tv) R)).
  reflexivity.
  rewrite H1.
  constructor.
  intro.
  apply in_map_iff in H2.
  destruct H2.
  destruct H2.
  apply H.
  exists (snd x).
  rewrite <- H2.
  rewrite <- surjective_pairing.
  assumption.

  assumption.
Qed.

Module EINDEX.
  Inductive mindex :=
  | ILVar : var -> nat -> mindex
  | IArray : arrayty -> var -> nat -> mindex
  | IPar : var -> mindex
  .

  Definition index := mindex.
  Fixpoint varOf (i: index) :=
    match i with
    | ILVar x _ => x
    | IArray _ x _ => x
    | IPar x => x
    end.
  Definition logVar x := IPar x.

  Inductive mmask :=
  | MBase : threads -> mmask
  | MAnd : mmask -> var -> mmask
  | MAndNot : mmask -> var -> mmask.

  Definition mask := mmask.

  Definition State := index -> value.
  Definition toEvalState (S: State) := S.
End EINDEX.

Module I := EINDEX.

Parameter memreads : result -> nat.
Parameter conflicts : result -> nat.

Inductive eval_opd :
  resmetric -> I.State -> threads -> opd -> result -> Cost -> Prop :=
| OCRes : forall M S T f,
    eval_opd M S T (ORes (map (fun t => (t, f t)) T))
             (map (fun t => (t, f t)) T) 0
| OCVar : forall M S T x,
    eval_opd M S T (OVar x) (map (fun t => (t, S (I.ILVar x t))) T) M.(MVar)
| OCPar : forall M S T x,
    eval_opd M S T (OPar x) (map (fun t => (t, S (I.IPar x))) T) M.(MPar)
| OCConst : forall M S T v,
    eval_opd M S T (OConst v) (map (fun t => (t, v)) T) M.(MConst)
| OCTid : forall M S T,
    eval_opd M S T OTid (map (fun t => (t, VNat t)) T) M.(MVar)
.

Fixpoint eval_opd2 (M: resmetric) (S: I.State) (T: threads) (o: opd) :
  result * Cost :=
  match o with
  | ORes R => (R, 0)
  | OVar x => (map (fun t => (t, S (I.ILVar x t))) T, M.(MVar))
  | OPar x => (map (fun t => (t, S (I.IPar x))) T, M.(MPar))
  | OConst v => (map (fun t => (t, v)) T, M.(MConst))
  | OTid => (map (fun t => (t, VNat t)) T, M.(MVar))
  end.

Hint Constructors eval_opd : eval.

Inductive eval_exp :
  resmetric -> I.State -> threads -> exp -> result -> Cost -> Prop :=
| ECRes: forall M S T f,
    eval_exp M S T (ERes (map (fun t => (t, f t)) T))
             (map (fun t => (t, f t)) T) 0
| ECOpd : forall M S T o vs C,
    eval_opd M S T o vs C -> eval_exp M S T (EOpd o) vs C
| ECOp : forall M S T o1 o2 vs1 vs2 C1 C2,
    eval_opd M S T o1 vs1 C1 ->
    eval_opd M S T o2 vs2 C2 ->
    eval_exp M S T (EOp o1 o2) vs1 (C1 ++ C2 ++ M.(MOp))
| ECGRead : forall M S T G o inds ic,
    eval_opd M S T o (map (fun tn => (fst tn, VNat (snd tn))) inds) ic ->
    eval_exp M S T (ERead Global G o)
             (map (fun ti => (fst ti, S (I.IArray Global G (snd ti)))) inds)
             (ic ++
                 (M.(MGRead)
                      (memreads (map (fun tn => (fst tn, VNat (snd tn))) inds))))
| ECSRead : forall M S T A o inds ic,
    eval_opd M S T o (map (fun tn => (fst tn, VNat (snd tn))) inds) ic ->
    eval_exp M S T (ERead Shared A o)
             (map (fun ti => (fst ti, S (I.IArray Shared A (snd ti)))) inds)
             (ic ++
                 (M.(MSRead)
                      (conflicts (map (fun tn => (fst tn, VNat (snd tn))) inds))))
.

Inductive InF {A: Type} : nat -> A -> list (nat * A) -> Prop :=
| InFEq : forall t v R, InF t v ((t, v)::R)
| InFCons : forall t v t' v' R,
    t <> t' -> InF t v R -> InF t v ((t', v')::R).

Lemma InF_In {A: Type} : forall t v (R: list (nat * A)),
    InF t v R -> In (t, v) R.
Proof.
  intros.
  induction H.
  apply in_eq.
  apply in_cons.
  assumption.
Qed.

Lemma In_InF {A: Type} : forall t v (R: list (nat * A)),
    uniq_r R -> In (t, v) R -> InF t v R.
Proof.
  intros.
  induction H.
  inversion H0.
  case (eq_nat_dec t (fst tv)); intro.
  apply in_inv in H0.
  destruct H0.
  rewrite H0.
  apply InFEq.
  destruct H.
  exists v.
  rewrite <- e.
  assumption.
  rewrite (surjective_pairing tv).
  apply InFCons.
  assumption.
  apply IHuniq_r.
  apply in_inv in H0.
  destruct H0.
  rewrite H0 in n.
  destruct n.
  reflexivity.
  assumption.
Qed.

Definition assign (S: I.State) (x: var) (vs: result) (S': I.State) :
  Prop :=
  forall (i : I.index),
    (forall t v, (i = I.ILVar x t /\ InF t v vs) -> S' i = v) /\
    (~(exists t v, (i = I.ILVar x t /\ In (t, v) vs)) -> S' i = S i).

Definition Gassign (S: I.State) (At: arrayty) (A: var) (inds: list (nat * nat))
           (vs: result) (S': I.State) :
  Prop :=
  (forall t t' n, In (t, n) inds -> In (t', n) inds -> t = t') (* Race free *)
  /\
  forall (i : I.index),
    (forall t v n, (i = I.IArray At A n /\ InF t n inds /\ InF t v vs) ->
                 S' i = v) /\
    (~(exists t v n, (i = I.IArray At A n /\ In (t, n) inds /\ In (t, v) vs)) ->
     S' i = S i).

Definition is_true (tv: nat * value) :=
  match snd tv with
  | VBool true => true
  | _ => false
  end.

Definition is_false (tv: nat * value) :=
  match snd tv with
  | VBool false => true
  | _ => false
  end.

Inductive eval_stmt :
  resmetric -> I.State -> threads -> stmt -> I.State -> Cost -> Prop :=
| SCSkip : forall M S T,
    eval_stmt M S T SSkip S 0
| SCVWrite : forall M S T S' x e vs C ,
    eval_exp M S T e vs C ->
    assign S x vs S' ->
    eval_stmt M S T (SVWrite x e) S' (C ++ M.(MVWrite))
| SCGWrite : forall M S T S' A e o inds C1 vs C2,
    uniq_r inds ->
    T = map fst inds ->
    eval_opd M S T o (map (fun tn => (fst tn , VNat (snd tn))) inds) C1 ->
    eval_exp M S T e vs C2 ->
    Gassign S Global A inds vs S' ->
    eval_stmt M S T (SAWrite Global A o e) S'
              (C1 ++ C2 ++
                  (M.(MGWrite)
                       (memreads (map (fun tn => (fst tn , VNat (snd tn))) inds))))
| SCSWrite : forall M S T S' A e o inds C1 vs C2,
    uniq_r inds ->
    T = map fst inds ->
    eval_opd M S T o (map (fun tn => (fst tn , VNat (snd tn))) inds) C1 ->
    eval_exp M S T e vs C2 ->
    Gassign S Shared A inds vs S' ->
    eval_stmt M S T (SAWrite Shared A o e) S'
              (C1 ++ C2 ++
                  (M.(MSWrite)
                       (conflicts (map (fun tn => (fst tn , VNat (snd tn))) inds))))
| SCIfT : forall M S T e s1 s2 C S' C',
    eval_exp M S T e (map (fun t => (t, VBool true)) T) C ->
    eval_stmt M S T s1 S' C' ->
    eval_stmt M S T (SIf e s1 s2) S' (C ++ C' ++ M.(MIf))
| SCIfF : forall M S T e s1 s2 C S' C',
    eval_exp M S T e (map (fun t => (t, VBool false)) T) C ->
    eval_stmt M S T s2 S' C' ->
    eval_stmt M S T (SIf e s1 s2) S' (C ++ C' ++ M.(MIf))
| SCIfD : forall M S T R TT TF e s1 s2 C C1 C2 S1 S2,
    eval_exp M S T e R C ->
    TT = map (fun tv => fst tv) (filter is_true R) ->
    TF = map (fun tv => fst tv) (filter is_false R) ->
    TT <> [] -> TF <> [] ->
    True -> (* assign S x (map (fun tv => (fst tv, VGhost (snd tv))) R) S' -> *)
    eval_stmt M S TT s1 S1 C1 ->
    eval_stmt M S1 TF s2 S2 C2 ->
    eval_stmt M S T (SIf e s1 s2) S2 (C ++ C1 ++ C2 ++ M.(MIf) ++ M.(MDiv))
              (*
| SCWhile : forall M S T e s S' C',
    eval_stmt M S T (SIf e (SSeq s (SWhile e s)) SSkip) S' C' ->
    eval_stmt M S T (SWhile e s) S' C'
               *)
| SCWhileAll : forall M S T e s S1 S2 C C1 C2,
    eval_exp M S T e (map (fun t => (t, VBool true)) T) C ->
    eval_stmt M S T s S1 C1 ->
    eval_stmt M S1 T (SWhile e s) S2 C2 ->
    eval_stmt M S T (SWhile e s) S2 (C ++ C1 ++ C2 ++ M.(MIf))
| SCWhileSome : forall M S T R TT e s S1 S2 C C1 C2,
    eval_exp M S T e R C ->
    TT = map (fun tv => fst tv) (filter is_true R) ->
    TT <> [] -> TT <> T ->
    eval_stmt M S TT s S1 C1 ->
    eval_stmt M S1 TT (SWhile e s) S2 C2 ->
    eval_stmt M S T (SWhile e s) S2 (C ++ C1 ++ C2 ++ M.(MIf) ++ M.(MDiv))
| SCWhileNone : forall M S T e s C,
    eval_exp M S T e (map (fun t => (t, VBool false)) T) C ->
    eval_stmt M S T (SWhile e s) S (C ++ M.(MIf))
| SCSeq : forall M S T s1 s2 S1 C1 S2 C2,
    eval_stmt M S T s1 S1 C1 ->
    eval_stmt M S1 T s2 S2 C2 ->
    eval_stmt M S T (SSeq s1 s2) S2 (C1 ++ C2)
| SCSync : forall M S T,
    eval_stmt M S T SSync S M.(MSync)
.

Definition typed_state (E: var -> type) (S: I.State) :=
  forall i, typed_value (S i) (E (I.varOf i)).

Definition typed_result (R : result) (t: type) :=
  forall th v, In (th, v) R -> typed_value v t.

Lemma safe_opd : forall M E S T o t R C,
    typed_opd E o t ->
    typed_state E S ->
    eval_opd M S T o R C ->
    typed_result R t.
Proof.
  intros M E S T o t R C.
  intro opd_typed.
  intro state_typed.
  intro eval_opd.

  induction eval_opd; inversion opd_typed;
  unfold typed_result; intros th V H_In; apply in_map_iff in H_In;
    destruct H_In as [x' H_In1];
    destruct H_In1 as [H_In1 H_In2]; inversion H_In1;  try (apply state_typed).

  + (* OSValue *)
    rewrite <- H5.
    assumption.

  + (* OSTid *)
    constructor.
Qed.

Hint Constructors eval_opd : eval.
Hint Constructors eval_exp : eval.
Hint Constructors eval_stmt : eval.
Hint Constructors typed_opd : eval.
Hint Constructors typed_exp : eval.
Hint Constructors valid : eval.

(** Preservation **)

Lemma pres_exp : forall M E S T e t R C,
    typed_exp E e t ->
    typed_state E S ->
    eval_exp M S T e R C ->
    typed_result R t.
Proof.
  intros M E S T e t R C.
  intros e_typed S_typed e_eval.

  induction e_eval.

  + (* ECRes *)
    inversion e_typed.
    
  + (* ECOpd *)
    inversion e_typed.
    apply (safe_opd _ _ _ _ _ _ _ _ H2 S_typed H).

  + (* ECOp *)
    inversion e_typed.
    apply (safe_opd _ _ _ _ _ _ _ _ H4 S_typed H).

  + (* ECGRead *)
    inversion e_typed.
    unfold typed_result; intros.
    apply in_map_iff in H6; destruct H6; destruct H6; inversion H6.
    apply S_typed.

  + (* ECSRead *)
    inversion e_typed.
    unfold typed_result; intros.
    apply in_map_iff in H6; destruct H6; destruct H6; inversion H6.
    apply S_typed.
Qed.

Lemma exists_t_dec {A: Type} : forall (th: nat) (R: list (nat * A)),
    (exists v, InF th v R) \/ ~(exists v, In (th, v) R).
Proof.
  intros th R.
  induction R.
  right. intro. destruct H. eauto.
  inversion IHR.
  left. destruct H.
  case (eq_nat_dec (fst a) th); intro Heq.
  exists (snd a).
  rewrite (surjective_pairing a).
  simpl.
  rewrite <- Heq.
  apply InFEq.
  exists x.
  rewrite (surjective_pairing a).
  apply InFCons.
  apply not_eq_sym.
  assumption.
  assumption.
  case (eq_nat_dec (fst a) th); intro Heq.
  left.
  exists (snd a).
  rewrite (surjective_pairing a).
  simpl.
  rewrite <- Heq.
  apply InFEq.
  right.
  intro.
  destruct H0.
  apply in_inv in H0.
  destruct H0.
  rewrite H0 in Heq.
  compute in Heq.
  apply Heq.
  reflexivity.
  apply H.
  exists x.
  assumption.
Qed.

Lemma assign_preserve : forall E S x R S',
    typed_state E S ->
    assign S x R S' ->
    typed_result R (E x) ->
    typed_state E S'.
Proof.
  intros E S x R S'.
  intros S_typed S_assign R_typed.
  unfold typed_state.
  intro i.
  case i.
  + (* ILVar *)
    intros v n.
    case (eq_nat_dec v x); intros.
    case (exists_t_dec n R); intros.
    destruct (S_assign (I.ILVar v n)).
    destruct H.
    rewrite (H0 n x0); eauto.
    unfold typed_result in R_typed.
    compute.
    rewrite e.
    apply (R_typed n x0).
    apply InF_In.
    assumption.
    destruct (S_assign (I.ILVar v n)).
    rewrite H1; eauto.
    intro.
    destruct H2.
    destruct H2.
    destruct H2.
    apply H.
    inversion H2.
    exists x1.
    assumption.
    destruct (S_assign (I.ILVar v n)).
    rewrite H0; eauto.
    intro.
    destruct H1. destruct H1. destruct H1.
    inversion H1.
    contradiction.

  + (* IArray *)
    intros.
    destruct (S_assign (I.IArray a v n)).
    rewrite H0; eauto.
    intro.
    destruct H1. destruct H1. destruct H1.
    inversion H1.

  + (* IPar *)
    intros.
    destruct (S_assign (I.IPar v)).
    rewrite H0; eauto.
    intro.
    destruct H1. destruct H1. destruct H1.
    inversion H1.
Qed.

Lemma eq_at_dec: forall (At At': arrayty), { At = At' } + { At <> At'}.
Proof.
  intros.
  case At; case At'; eauto; right; discriminate.
Qed.

Lemma Exists_In: forall inds (n: nat),
    List.Exists (fun tn => snd tn = n) inds ->
    exists t: nat, In (t, n) inds.
Proof.
  intros.
  apply Exists_exists in H.
  destruct H.
  destruct H.
  exists (fst x).
  rewrite <- H0.
  rewrite <- (surjective_pairing x).
  assumption.
Qed.
  
Lemma Gassign_preserve : forall E S At inds A R S',
    uniq_r inds ->
    typed_state E S ->
    Gassign S At A inds R S' ->
    typed_result R (E A) ->
    typed_state E S'.
Proof.
  intros E S At inds A R S'.
  intros H_uniq S_typed S_assign R_typed.
  unfold typed_state.
  intro i.
  case i.
  + (* ILVar *)
    intros.
    destruct S_assign.
    destruct (H0 (I.ILVar v n)).
    rewrite (H2); eauto.
    intro.
    destruct H3. destruct H3. destruct H3. destruct H3.
    inversion H3.

  + (* IArray *)
    intros At' A' n.
    case (eq_at_dec At' At); intros.
    case (eq_nat_dec A' A); intros.
    subst At' A'.
    case (List.Exists_dec (fun tn => snd tn = n) inds); intros.
    apply eq_nat_dec.
    apply Exists_In in e.
    destruct e.
    apply In_InF in H; trivial.
    case (exists_t_dec x R); intros.
    destruct H0.
    destruct S_assign.
    destruct (H2 (I.IArray At A n)).
    rewrite (H3 x x0 n); eauto.
    unfold typed_result in R_typed.
    compute.
    apply (R_typed x x0).
    apply InF_In.
    assumption.
    destruct S_assign.
    destruct (H2 (I.IArray At A n)).
    rewrite (H4); eauto.
    intro.
    destruct H5.
    destruct H5.
    destruct H5.
    destruct H5.
    apply H0.
    destruct H6.
    apply InF_In in H.
    inversion H5.
    subst x2.
    rewrite (H1 x x0 n); trivial.
    exists x1.
    assumption.
    destruct S_assign. destruct (H0 (I.IArray At A n)).
    rewrite (H2); eauto.
    intro.
    destruct H3. destruct H3. destruct H3. destruct H3.
    destruct H4.
    inversion H3.
    subst x1.
    apply n0.
    apply List.Exists_exists.
    exists (x, n). split; eauto.
    destruct S_assign.
    destruct (H0 (I.IArray At' A' n)).
    rewrite H2; eauto.
    intro.
    do 4 destruct H3.
    inversion H3.
    contradiction.
    destruct S_assign.
    destruct (H0 (I.IArray At' A' n)).
    rewrite H2; eauto.
    intro.
    do 4 destruct H3.
    inversion H3.
    contradiction.
    
  + (* IPar *)
    intros.
    destruct S_assign.
    destruct (H0 (I.IPar v)).
    rewrite H2; eauto.
    intro.
    do 4 destruct H3.
    inversion H3.
Qed.

Lemma preservation : forall M E S T s S' C,
    valid E s ->
    typed_state E S ->
    eval_stmt M S T s S' C ->
    typed_state E S'.
Proof.
  intros M E S T s S' C.
  intros s_valid S_typed s_eval.

  induction s_eval; inversion s_valid; try eauto with eval;
    try (apply (assign_preserve E S x vs));
    try (apply (Gassign_preserve E S aty inds A vs));
    try (apply (pres_exp M E S T e _ _ C));
    try (apply (pres_exp M E S T e _ _ C2));
    try (subst aty); try eauto with eval.
Qed.
(*
  + (* SCIfD *).
    apply IHs_eval2.
    assumption.
    apply IHs_eval1.
    assumption.
    apply (assign_preserve E S x
                 (map (fun tv : nat * value => (fst tv, VGhost (snd tv))) R)).
    assumption.
    assumption.
    unfold typed_result.
    intros th v H_in.
    apply in_map_iff in H_in.
    destruct H_in.
    destruct H12.
    inversion H12.
    apply VSGhost.
Qed.
 *)

Hint Constructors uniq_r : eval.
Hint Constructors uniq_t : eval.
Hint Resolve map_pres_uniqtr : eval.
Hint Resolve map_pres_uniqrt : eval.
Hint Resolve filter_pres_uniqr : eval.

Lemma uniqr_eval_opd: forall M S T o R C,
    uniq_t T ->
    eval_opd M S T o R C ->
    uniq_r R.
Proof.
  intros.
  inversion H0; try eauto with eval.
Qed.

Hint Resolve uniqr_eval_opd : eval.

Lemma fst_eval_T_opd: forall M S T o R C,
    eval_opd M S T o R C ->
    map (fun tv => fst tv) R = T.
Proof.
  intros.
  induction H; eauto; rewrite map_map; simpl; apply map_id.
Qed.

Hint Resolve fst_eval_T_opd : infer.

Lemma fst_eval_T: forall M S T e R C,
    eval_exp M S T e R C ->
    map (fun tv => fst tv) R = T.
Proof.
  intros.
  induction H; eauto with infer.

  rewrite map_map.
  induction T.
  reflexivity.
  unfold map.
  simpl.
  apply f_equal.
  assumption.

  apply fst_eval_T_opd in H.
  rewrite map_map in H.
  rewrite map_map.
  simpl.
  simpl in H.
  assumption.

  apply fst_eval_T_opd in H.
  rewrite map_map in H.
  rewrite map_map.
  simpl.
  simpl in H.
  assumption.
Qed.

Hint Resolve fst_eval_T : infer.


Lemma uniqr_eval_exp: forall M S T e R C,
    uniq_t T ->
    eval_exp M S T e R C ->
    uniq_r R.
Proof.
  intros.
  apply fst_eval_T in H0.
  subst T.
  remember (map fst R) as T.
  generalize dependent R.
  induction H; intros.
  apply sym_eq in HeqT.
  apply map_eq_nil in HeqT.
  subst R.
  constructor.
  induction R.
  inversion HeqT.
  rewrite map_cons in HeqT.
  inversion HeqT.
  subst a.
  constructor.
  intro.
  destruct H1.
  apply H.
  rewrite H3.
  apply in_map_iff.
  exists (fst a0, x).
  split; trivial.
  apply IHuniq_t.
  assumption.
Qed.

(** Expression evaluation is deterministic **)

Lemma eval_opd_deterministic: forall M M' S T o R C R' C',
    eval_opd M S T o R C ->
    eval_opd M' S T o R' C' ->
    R = R'.
Proof.
  intros.
  induction H; inversion H0; try eauto.
Qed.

Hint Resolve eval_opd_deterministic :infer.

Lemma eval_exp_deterministic: forall M M' S T e R C R' C',
    eval_exp M S T e R C ->
    eval_exp M' S T e R' C' ->
    R = R'.
Proof.
  intros.
  induction H; inversion H0; try eauto with infer.

  + (* ECGRead *)
    assert (map (fun tn : nat * nat => (fst tn, VNat (snd tn))) inds =
            map (fun tn : nat * nat => (fst tn, VNat (snd tn))) inds0).
    apply (eval_opd_deterministic M M' S T o _ ic _ ic0); eauto.
    set (f := (fun tv : nat * value => (fst tv,
                              match snd tv with
                              | VNat n => S (I.IArray Global G n)
                              | _ => VAbs
                              end))).
    assert (map f (map (fun tn : nat * nat => (fst tn, VNat (snd tn))) inds)
            = map (fun ti : nat * nat => (fst ti, S (I.IArray Global G (snd ti)))) inds).
    apply map_map.
    assert (map f (map (fun tn : nat * nat => (fst tn, VNat (snd tn))) inds0)
            = map (fun ti : nat * nat => (fst ti, S (I.IArray Global G (snd ti)))) inds0).
    apply map_map.
    rewrite <- H10.
    rewrite <- H11.
    apply f_equal.
    assumption.

  + (* ECSRead *)
    assert (map (fun tn : nat * nat => (fst tn, VNat (snd tn))) inds =
            map (fun tn : nat * nat => (fst tn, VNat (snd tn))) inds0).
    apply (eval_opd_deterministic M M' S T o _ ic _ ic0); eauto.
    set (f := (fun tv : nat * value => (fst tv,
                              match snd tv with
                              | VNat n => S (I.IArray Shared A n)
                              | _ => VAbs
                              end))).
    assert (map f (map (fun tn : nat * nat => (fst tn, VNat (snd tn))) inds)
            = map (fun ti : nat * nat => (fst ti, S (I.IArray Shared A (snd ti)))) inds).
    apply map_map.
    assert (map f (map (fun tn : nat * nat => (fst tn, VNat (snd tn))) inds0)
            = map (fun ti : nat * nat => (fst ti, S (I.IArray Shared A (snd ti)))) inds0).
    apply map_map.
    rewrite <- H10.
    rewrite <- H11.
    apply f_equal.
    assumption.
Qed.
