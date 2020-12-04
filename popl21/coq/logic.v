(*** Quantitative Program Logic (Section 4 of submission) ***)

Require Import List.
Require Import calc.
Require Import QArith.
Require Import cost.
Require Import eval.

(*** Definitions and Assumptions ***)

Definition State := I.State.

Parameter Pctx : Type.
Parameter PT : Pctx -> threads -> Prop.
Parameter PST : Pctx -> threads -> State -> Prop.
Parameter PG : Pctx -> State -> Prop.
Parameter Pand : Pctx -> Pctx -> Pctx.
Notation "P1 /s\ P2" := (Pand P1 P2) (at level 82).
Parameter Pand_correct1 :
  forall P1 P2 T, PT (P1 /s\ P2) T -> PT P1 T /\ PT P2 T.
Parameter Pand_correct2 :
  forall P1 P2 T, PT P1 T -> PT P2 T -> PT (P1 /s\ P2) T.
Parameter Pand_correct3 :
  forall P1 P2 T S, PST (P1 /s\ P2) T S -> PST P1 T S /\ PST P2 T S.
Parameter Pand_correct4 :
  forall P1 P2 T S, PST P1 T S -> PST P2 T S -> PST (P1 /s\ P2) T S.
Parameter Pand_correct5 :
  forall P1 P2 S, PG (P1 /s\ P2) S -> PG P1 S /\ PG P2 S.
Parameter Pand_correct6 :
  forall P1 P2 S, PG P1 S -> PG P2 S -> PG (P1 /s\ P2) S.
Parameter PT_sub: forall P T T',
    PT P T ->
    (forall t, In t T' -> In t T) ->
    PT P T'.
Parameter PST_sub: forall P T T' S,
    PST P T S ->
    (forall t, In t T' -> In t T) ->
    PST P T' S.
Parameter PT_reconstruct: forall P T T1 T2,
    (forall t, (In t T1 \/ In t T2) <-> In t T) ->
    PT P T1 ->
    PT P T2 ->
    PT P T.
Parameter PST_reconstruct: forall P T T1 T2 S,
    (forall t, (In t T1 \/ In t T2) <-> In t T) ->
    PT P T ->
    PST P T1 S ->
    PST P T2 S ->
    PST P T S.
Parameter PST_depend: forall P T S S',
    (forall x t, In t T -> S' (I.ILVar x t) = S (I.ILVar x t)) ->
    PST P T S ->
    PST P T S'.
Parameter PG_depend: forall P T S S',
    (forall x t, In t T -> S' (I.ILVar x t) = S (I.ILVar x t)) ->
    PG P S ->
    PG P S'.

Parameter Qctx : Type.
Parameter Qap : Qctx -> State -> list var -> Cost -> Prop.
Parameter Qgt : Qctx -> Qctx -> Prop.
Parameter Qplus : Qctx -> Cost -> Qctx.
Parameter Qplus_correct : forall Q C S X C',
    Qap Q S X C' <-> Qap (Qplus Q C) S X (C' ++ C).
Notation "Q <+ C" := (Qplus Q C) (at level 81, left associativity).

Parameter Qgt_exists: forall Q Q' S X C,
    Qgt Q Q' ->
    Qap Q S X C ->
    exists C', C' <= C /\ Qap Q' S X C'.
Parameter Qap_sub_X: forall Q S X C X',
    (forall x, In x X' -> In x X) ->
    Qap Q S X C ->
    exists C', Qap Q S X' C' /\ C' <= C.

Inductive cond :=
  { P : Pctx;
    Q : Qctx;
    X : list var }.

Notation "{{ P1 ; Q1 ; X1 }}" := {| P := P1; Q := Q1; X := X1|}.

Definition compat (S: State) (T: threads) (c: cond) : Prop :=
  PT c.(P) T
  /\ PST c.(P) T S
  /\ PG c.(P) S
  /\ (forall x t1 t2, In x c.(X) -> In t1 T -> In t2 T ->
                      S (I.ILVar x t1) = S (I.ILVar x t2)).

Definition PimpliesP (P1 P2 : Pctx) :=
  (forall T, PT P1 T -> PT P2 T)
  /\ (forall T S, PT P1 T -> PST P1 T S -> PST P2 T S)
  /\ (forall S, PG P1 S -> PG P2 S).
Definition Pimplies (P: Pctx) (J : State -> threads -> Prop) :=
  forall T S, PT P T -> PST P T S -> PG P S -> J S T.

Notation "S ;; T |= c" := (compat S T c) (at level 10).
Notation "P ===> J" := (Pimplies P J) (at level 15).
Notation "P1 =P=> P2" := (PimpliesP P1 P2) (at level 10).

Open Scope nat_scope.

Definition numreads_lessthan
           (o: opd) (n: nat) (S: I.State) (T: threads) :=
  forall (M: resmetric) (R: result) (C: Cost),
    eval_opd M S T o R C ->
    memreads R <= n.

Definition conflicts_lessthan
           (o: opd) (n: nat) (S: I.State) (T: threads) :=
  forall (M: resmetric) (R: result) (C: Cost),
    eval_opd M S T o R C ->
    conflicts R <= n.

Definition unif (e: exp) (S: I.State) (T: threads) :=
  forall (M: resmetric) (R: result) (C: Cost),
    eval_exp M S T e R C ->
    exists v, forall (th: nat) (v': value), In (th, v') R -> v' = v.

Parameter Pe : exp -> Pctx.
Parameter Pe_correct1: forall e S T,
    (forall M R C,
        eval_exp M S T e R C ->
        forall (th: nat) (v': value), In (th, v') R -> v' = VBool true) ->
    PT (Pe e) T /\ PST (Pe e) T S /\ PG (Pe e) S.
Parameter Pe_correct2: forall e S T,
    PT (Pe e) T -> PST (Pe e) T S -> PG (Pe e) S ->
    (forall M R C,
        eval_exp M S T e R C ->
        forall (th: nat) (v': value), In (th, v') R -> v' = VBool true).

Parameter Pne : exp -> Pctx.
Parameter Pne_correct1: forall e S T,
    (forall M R C,
        eval_exp M S T e R C ->
        forall (th: nat) (v': value), In (th, v') R -> v' = VBool false) ->
    PT (Pne e) T /\ PST (Pne e) T S /\ PG (Pne e) S.
Parameter Pne_correct2: forall e S T,
    PT (Pne e) T -> PST (Pne e) T S -> PG (Pne e) S ->
    (forall M R C,
        eval_exp M S T e R C ->
        forall (th: nat) (v': value), In (th, v') R -> v' = VBool false).

Open Scope Q_scope.

(** Subsets of threads evaluate to subsets of the same results **)

Lemma in_map_eq: forall (T T': threads) (f f0: nat -> value) t,
    map (fun t : nat => (t, f0 t)) T' = map (fun t : nat => (t, f t)) T ->
    In t T ->
    f t = f0 t.
Proof.
  intros.
  generalize dependent T'.
  induction T; intros.
  inversion H0.
  apply in_inv in H0.
  destruct H0.
  subst a.
  induction T'.
  inversion H.
  inversion H.
  reflexivity.
  induction T'.
  inversion H.
  inversion H.
  apply (IHT H0 T').
  assumption.
Qed.  
  
Lemma eval_opd_det_sub: forall M M' S T T' o R R' C C' t v v',
    eval_opd M S T o R C ->
    eval_opd M' S T' o R' C' ->
    In (t, v) R ->
    In (t, v') R' ->
    v = v'.
Proof.
  intros.
  induction H; try (inversion H0; subst R';
    apply in_map_iff in H1;
    apply in_map_iff in H2;
    destruct H1;
    destruct H2;
    destruct H1;
    destruct H2;
    inversion H1;
    inversion H2; try reflexivity).
  subst x.
  apply (in_map_eq T T'); eauto.

  subst v0 v' v1.
  reflexivity.
Qed.

Hint Resolve eval_opd_det_sub : infer.

Lemma eval_exp_det_sub: forall M M' S T T' e R R' C C' t v v',
    eval_exp M S T e R C ->
    eval_exp M' S T' e R' C' ->
    In (t, v) R ->
    In (t, v') R' ->
    v = v'.
Proof.
  intros.
  induction H; inversion H0; subst R'; eauto with infer.

  + apply in_map_iff in H1.
    do 2 destruct H1.
    apply in_map_iff in H2.
    do 2 destruct H2.
    inversion H1.
    inversion H2.
    subst x.
    apply (in_map_eq T T'); eauto.

  + apply in_map_iff in H1.
    do 2 destruct H1.
    apply in_map_iff in H2.
    do 2 destruct H2.
    inversion H1.
    inversion H2.
    apply (eval_opd_det_sub M M' _ T _ _ (map (fun tn => (fst tn, VNat (snd tn))) inds)
                            _ ic _ t (VNat (snd x)) (VNat (snd x0)))
      in H10.
    inversion H10.
    reflexivity.
    assumption.
    apply in_map_iff.
    exists x. split; eauto.
    subst t.
    reflexivity.
    apply in_map_iff.
    exists x0. split; eauto.
    rewrite H15.
    reflexivity.

  + apply in_map_iff in H1.
    do 2 destruct H1.
    apply in_map_iff in H2.
    do 2 destruct H2.
    inversion H1.
    inversion H2.
    apply (eval_opd_det_sub M M' _ T _ _ (map (fun tn => (fst tn, VNat (snd tn))) inds)
                            _ ic _ t (VNat (snd x)) (VNat (snd x0)))
      in H10.
    inversion H10.
    reflexivity.
    assumption.
    apply in_map_iff.
    exists x. split; eauto.
    subst t.
    reflexivity.
    apply in_map_iff.
    exists x0. split; eauto.
    rewrite H15.
    reflexivity.
Qed.

(** Various helper lemmas **)

Lemma is_true_true : forall x,
    is_true x = true -> snd x = VBool true.
Proof.
  intros.
  destruct x.
  induction v; inversion H.
  rewrite H.
  induction b.
  reflexivity.
  inversion H.
Qed.

Lemma is_false_true : forall x,
    is_false x = true -> snd x = VBool false.
Proof.
  intros.
  destruct x.
  induction v; inversion H.
  induction b.
  inversion H.
  reflexivity.
Qed.

Lemma Pe_from_eval: forall M S T e C,
    eval_exp M S T e (map (fun t => (t, VBool true)) T) C ->
    PT (Pe e) T /\ PST (Pe e) T S /\ PG (Pe e) S.
Proof.
  intros.
  apply Pe_correct1.
  intros.
  apply (eval_exp_deterministic M _ _ _ _ (map (fun t => (t, VBool true)) T) C) in H0.
  subst R.
  apply in_map_iff in H1.
  destruct H1.
  destruct H0.
  inversion H0.
  reflexivity.
  assumption.
Qed.

Lemma Pe_TT: forall M S T e R C TT,
    eval_exp M S T e R C ->
    TT = map fst (filter is_true R) ->
    PT (Pe e) TT /\ PST (Pe e) TT S /\ PG (Pe e) S.
Proof.
  intros.
  apply Pe_correct1.
  intros.
  assert (In (th, VBool true) R).
  assert (In th TT).
  rewrite <- (fst_eval_T M0 S TT e R0 C0).
  apply in_map_iff.
  exists (th, v').
  split; trivial.
  assumption.
  subst TT.
  apply in_map_iff in H3.
  destruct H3.
  destruct H0.
  apply filter_In in H3.
  destruct H3.
  apply is_true_true in H4.
  rewrite <- H0.
  rewrite <- H4.
  rewrite <- surjective_pairing.
  assumption.
  apply sym_eq.
  apply (eval_exp_det_sub M M0 S T TT e R R0 C C0 th (VBool true) v'); try assumption.
Qed.


Lemma Pne_from_eval: forall M S T e C,
    eval_exp M S T e (map (fun t => (t, VBool false)) T) C ->
    PT (Pne e) T /\ PST (Pne e) T S /\ PG (Pne e) S.
Proof.
  intros.
  apply Pne_correct1.
  intros.
  apply (eval_exp_deterministic M _ _ _ _ (map (fun t => (t, VBool false)) T) C) in H0.
  subst R.
  apply in_map_iff in H1.
  destruct H1.
  destruct H0.
  inversion H0.
  reflexivity.
  assumption.
Qed.

Lemma Pne_TF: forall M S T e R C TF,
    eval_exp M S T e R C ->
    TF = map fst (filter is_false R) ->
    PT (Pne e) TF /\ PST (Pne e) TF S /\ PG (Pne e) S.
Proof.
  intros.
  apply Pne_correct1.
  intros.
  assert (In (th, VBool false) R).
  assert (In th TF).
  rewrite <- (fst_eval_T M0 S TF e R0 C0).
  apply in_map_iff.
  exists (th, v').
  split; trivial.
  assumption.
  subst TF.
  apply in_map_iff in H3.
  destruct H3.
  destruct H0.
  apply filter_In in H3.
  destruct H3.
  apply is_false_true in H4.
  rewrite <- H0.
  rewrite <- H4.
  rewrite <- surjective_pairing.
  assumption.
  apply sym_eq.
  apply (eval_exp_det_sub M M0 S T TF e R R0 C C0 th (VBool false) v'); try assumption.
Qed.

(*** Definitions of program logic ***)

Inductive copd : resmetric -> Pctx -> opd -> Cost -> Prop :=
| OQVar : forall M P x, copd M P (OVar x) M.(MVar)
| OQPar : forall M P x, copd M P (OPar x) M.(MPar)
| OQConst : forall M P x, copd M P (OConst x) M.(MConst)
| OQTid : forall M P, copd M P OTid M.(MVar)
.

Inductive cexp : resmetric -> Pctx -> exp -> Cost -> Prop :=
| EQOpd : forall M P o C, copd M P o C -> cexp M P (EOpd o) C
| EQOp : forall M P o1 o2 C1 C2,
    copd M P o1 C1 -> copd M P o2 C2 ->
    cexp M P (EOp o1 o2) (C1 ++ C2 ++ M.(MOp))
| EQGRead : forall M (P: Pctx) A o C n,
    copd M P o C -> P ===> numreads_lessthan o n ->
    cexp M P (ERead Global A o) (C ++ M.(MGRead) n)
| EQSRead : forall M P A o C n,
    copd M P o C -> P ===> conflicts_lessthan o n ->
    cexp M P (ERead Shared A o) (C ++ M.(MSRead) n)
.

Hint Constructors copd : infer.
Hint Constructors cexp : infer.

Definition varMinus (X1 X2 : list var) s :=
  forall x,
    In x X2 <-> (In x X1 /\ ~ (writesto s x)).

Definition StateSub
           (S: I.State) (x: var) (e: exp) (S' : I.State) :=
  exists T M R C,
    eval_exp M S T e R C ->
    assign S x R S'.
(*
Definition StateSubG
           (S: I.State) aty (A: var) (o: opd) (e: exp) (S' : I.State) :=
  exists T M inds vs C1 C2,
    eval_opd M S T o (map (fun tn => (fst tn , VNat (snd tn))) inds) C1 ->
    eval_exp M S T e vs C2 ->
    Gassign S aty A inds vs S'.
*)
Parameter PSub : Pctx -> var -> exp -> Pctx.
Parameter PSubG : Pctx -> arrayty -> var -> opd -> exp -> Pctx.
Parameter QSub : Qctx ->  var -> exp -> Qctx.
Parameter PSub_correct :
  forall P x e (S S': I.State) T Q X,
    StateSub S x e S' ->
    ((S';; T |= {{P; Q; X}}) <-> (S;; T |= {{PSub P x e; Q; X}})).
(*
Parameter PSub_correctG :
  forall P aty A o e (S S': I.State) T Q X,
    StateSubG S aty A o e S' ->
    ((S';; T |= {{P; Q; X}}) <-> (S;; T |= {{PSubG P aty A o e; Q; X}})).
*)
Parameter QSub_correct :
  forall P Q x e (S S': I.State) X C,
    In x X ->
    (forall M T R Ce,
        PT P T -> PST P T S -> PG P S ->
        eval_exp M S T e R Ce ->
        exists v, forall (th: nat) (v': value), In (th, v') R -> v' = v
    ) ->
    StateSub S x e S' ->
    (Qap Q S' X C) <-> (Qap (QSub Q x e) S X C).
Parameter QSub_correct' :
  forall Q x e,
    Qgt (QSub Q x e) Q.
Parameter QSub_correct'' :
  forall Q x e (S S': I.State) X C,
    ~ In x X ->
    StateSub S x e S' ->
    (Qap Q S' X C) <-> (Qap Q S X C).
Parameter QSub_correct''' :
  forall Q aty A inds vs (S S': I.State) X C,
    Gassign S aty A inds vs S' ->
    (Qap Q S' X C) <-> (Qap Q S X C).

Inductive cstmt : resmetric -> cond -> stmt -> cond -> Prop :=
| QSkip : forall M P Q X, cstmt M {{P; Q; X}} SSkip {{P; Q; X}}
| QIf1 : forall M P Q X e s1 s2 C P' Q' X',
    cexp M P e C ->
    (P ===> unif e) ->
    cstmt M {{(P /s\ (Pe e)); Q; X}} s1 {{P'; Q'; X'}} ->
    cstmt M {{(P /s\ (Pne e)); Q; X}} s2 {{P'; Q'; X'}} ->
    cstmt M {{P; (Q <+ MIf M <+ C); X}} (SIf e s1 s2) {{ P'; Q'; X'}}
| QIf2 : forall M P Q X e s1 s2 C P' Q' X',
    cexp M P e C ->
    (P =P=> (Pe e)) ->
    cstmt M {{P; Q; X}} s1 {{P'; Q'; X'}} ->
    cstmt M {{P; (Q <+ MIf M <+ C); X}} (SIf e s1 s2) {{ P'; Q'; X'}}
| QIf3 : forall M P Q X e s1 s2 C P' Q' X',
    cexp M P e C ->
    (P =P=> (Pne e)) ->
    cstmt M {{P; Q; X}} s2 {{P'; Q'; X'}} ->
    cstmt M {{P; (Q <+ MIf M <+ C); X}} (SIf e s1 s2) {{ P'; Q'; X'}}
| QIf4 : forall M P Q X e s1 s2 C P1 Q1 X1 P' X' P2 Q2 X2 P'' X'',
    cexp M P e C ->
    cstmt M {{(P /s\ (Pe e)); Q; X}} s1 {{P1; Q1; X1}} ->
    cstmt M {{P'; Q1; X'}} s2 {{P2; Q2; X2}} ->
    ((P /s\ Pne e) =P=> P') ->
    (P1 =P=> P') ->
    (P1 =P=> P'') ->
    (P2 =P=> P'') ->
    (varMinus X1 X' s1) ->
    (varMinus X2 X'' s2) ->
    cstmt M {{P; (Q <+ MIf M <+ C <+ MDiv M); X}} (SIf e s1 s2) {{P''; Q2; X''}}
| QWhile1 : forall M P Q X e s C,
    cexp M P e C ->
    (P ===> unif e) ->
    cstmt M {{P /s\ (Pe e); Q; X}} s {{P; Q <+ MIf M <+ C; X}} ->
    cstmt M {{P; Q <+ MIf M <+ C; X}} (SWhile e s) {{P /s\ (Pne e); Q; X}}
| QWhile2 : forall M P Q X X' e s C,
    cexp M P e C ->
    cstmt M {{P /s\ (Pe e); Q; X}} s {{P; Q <+ MIf M <+ C <+ MDiv M; X}} ->
    varMinus X X' s ->
    cstmt M {{P; Q <+ MIf M <+ C <+ MDiv M; X}} (SWhile e s) {{P /s\ (Pne e); Q; X'}}
| QSeq : forall M P Q X s1 s2 P1 Q1 X1 P' Q' X',
    cstmt M {{P; Q; X}} s1 {{P1; Q1; X1}} ->
    cstmt M {{P1; Q1; X1}} s2 {{P'; Q'; X'}} ->
    cstmt M {{P; Q; X}} (SSeq s1 s2) {{P'; Q'; X'}}
| QVWrite1 : forall M P' Q' X x e C,
    (PSub P' x e ===> unif e) ->
    In x X ->
    cexp M (PSub P' x e) e C ->
    cstmt M {{PSub P' x e; (QSub Q' x e) <+ MVWrite M <+ C; X}} (SVWrite x e)
          {{P'; Q'; X}}
| QVWrite2 : forall M P' Q X X' x e C,
    cexp M (PSub P' x e) e C ->
    varMinus X X' (SVWrite x e) ->
    cstmt M {{PSub P' x e; Q <+ MVWrite M <+ C; X}} (SVWrite x e)
          {{P'; Q; X'}}
| QGWrite : forall M P Q X A o e C1 C2 n,
    (P ===> numreads_lessthan o n) ->
    copd M P o C1 ->
    cexp M P e C2 ->
    cstmt M {{P; Q <+ MGWrite M n <+ C1 <+ C2; X}} (SAWrite Global A o e)
          {{P; Q; X}}
| QSWrite : forall M P Q X A o e C1 C2 n,
    (P ===> conflicts_lessthan o n) ->
    copd M P o C1 ->
    cexp M P e C2 ->
    cstmt M {{P; Q <+ MSWrite M n <+ C1 <+ C2; X}} (SAWrite Shared A o e)
          {{P; Q; X}}
| QWeak : forall M P1 Q1 C X1 P2 Q2 X2 s P2' Q2' X2' P1' Q1' X1',
    cstmt M {{P2; Q2; X2}} s {{P2'; Q2'; X2'}} ->
    (P1 =P=> P2) ->
    (Qgt Q1 Q2) ->
    (forall x, In x X2 -> In x X1) ->
    (P2' =P=> P1') ->
    (Qgt Q2' Q1') ->
    (forall x, In x X1' -> In x X2') ->
    cstmt M {{P1; Q1 <+ C; X1}} s {{P1'; Q1' <+ C; X1'}}.

(*** Soundness of operand and expression evaluation
 *** (Lemma 1 of submission) ***)

Lemma copd_sound : forall M P o C S T R C',
    copd M P o C ->
    eval_opd M S T o R C' ->
    C' <= C.
Proof.
  intros.
  induction H; inversion H0; apply Qle_refl.
Qed.

Hint Resolve copd_sound : infer.

Lemma cexp_sound : forall M P C S T e R C',
  cexp M P e C ->
  PT P T ->
  PST P T S ->
  PG P S ->
  eval_exp M S T e R C' ->
  C' <= C.
Proof.
  intros M P C S T e R C' H_cexp H_PT H_PST H_PG H_eval.

  induction H_cexp; inversion H_eval; try eauto with infer;
    try (apply Qle_refl); try eauto with infer.

  + (* EQOp *)
    apply (copd_sound _ P0 _ C1) in H6; eauto.
    apply (copd_sound _ P0 _ C2) in H9; eauto.
    apply (Qplus_le_compat).
    assumption.
    apply Qplus_le_compat.
    assumption.
    apply Qle_refl.

  + (* EQGRead *)
    apply Qplus_le_compat.
    apply (copd_sound _ P0 _ C) in H8; eauto.
    apply mgread_monotonic.
    unfold Pimplies in H0.
    unfold numreads_lessthan in H0.
    apply (H0 T S H_PT H_PST H_PG M _ _ H8).

  + (* EQSRead *)
    apply Qplus_le_compat.
    apply (copd_sound _ P0 _ C) in H8; eauto.
    apply msread_monotonic.
    unfold Pimplies in H0.
    apply (H0 T S H_PT H_PST H_PG M _ _ H8).
Qed.

(** More helper lemmas **)

Lemma compat_Q : forall Q P Q' X S T,
    S;; T |= {{P; Q; X}} ->
    S;; T |= {{P; Q'; X}}.
Proof.
  unfold compat.
  intros.
  assumption.
Qed.

Axiom Qeq: forall C1 C2, C1 == C2 -> C1 = C2.

Lemma Qplus_ap: forall Q C C0 S X,
    Qap (Q <+ C) S X C0 -> Qap Q S X (C0 - C).
Proof.
  intros Q C C0 S X H_Q.
  destruct (Qplus_correct Q C S X (C0 - C)).
  apply H0.
  assert (C0 - C ++ C == C0).
  ring.
  apply Qeq in H1.
  rewrite H1.
  assumption.
Qed.

Lemma compat_P_implies: forall P P' Q X S T,
    (P =P=> P') ->
    S;; T |= {{P; Q; X}} ->
    S;; T |= {{P'; Q; X}}.
Proof.
  intros.
  split; [apply H; apply H0 | split; [apply H; apply H0
                                     | split; [apply H; apply H0 | apply H0]]].
Qed.

Hint Unfold varMinus : infer.

Lemma cstmt_mono_X : forall M c s c',
    cstmt M c s c' ->
    forall x, In x (X c') -> In x (X c).
Proof.
  intros.
  induction H; eauto with infer.

  + apply IHcstmt1.
    apply H7.
    apply IHcstmt2.
    apply H8.
    assumption.

  + apply H2. assumption.

  + apply H1. assumption.
Qed.

Lemma compat_X: forall X' P Q X S T,
    S;; T |= {{P; Q; X}} ->
    (forall x, In x X' -> In x X) ->
    S;; T |= {{P; Q; X'}}.
Proof.
  intros.
  split; [apply H | split; [apply H | split; [apply H | ]]].
  intros.
  apply H; try assumption.
  apply H0; try assumption.
Qed.

Parameter Qgt_refl : forall Q, Qgt Q Q.
Parameter Qgt_plus : forall Q C, Qgt (Q <+ C) Q.
Parameter Qgt_trans : forall Q1 Q2 Q3, Qgt Q1 Q2 -> Qgt Q2 Q3 -> Qgt Q1 Q3.
Parameter Qgt_plus_compat: forall Q Q' C, Qgt Q Q' -> Qgt (Q <+ C) (Q' <+ C).

Hint Resolve Qgt_refl: Qhints.
Hint Resolve Qgt_plus: Qhints.
Hint Resolve Qgt_trans: Qhints.
Hint Resolve Qgt_plus_compat: Qhints.

Lemma Qgt_plus' : forall Q C Q',
    Qgt Q Q' ->
    Qgt (Q <+ C) Q'.
Proof.
  intros.
  apply (Qgt_trans _ Q0); eauto with Qhints.
Qed.

Hint Resolve Qgt_plus': Qhints.

Lemma cstmt_mono_Q : forall M c s c',
    cstmt M c s c' ->
    Qgt (Q c) (Q c').
Proof.
  intros.
  induction H; try solve [repeat apply Qgt_plus'; eauto with Qhints].

  + repeat apply Qgt_plus'.
    apply (QSub_correct').

  + apply Qgt_plus_compat.
    apply (Qgt_trans _ Q2); try assumption.
    apply (Qgt_trans _ Q2'); try assumption.
Qed.

Lemma compat_T_sub: forall S T T' c,
    S;; T |= c ->
    (forall t, In t T' -> In t T) ->
    S;; T' |= c.
Proof.
  intros.
  split. apply (PT_sub _ T); [apply H | assumption].
  split. apply (PST_sub _ T); [apply H | assumption].
  split. apply H.
  intros.
  apply H; try assumption; try apply H0; assumption.
Qed.

Lemma map_id: forall (T: threads),
    map (fun x => x) T = T.
Proof.
  intros.
  induction T; simpl; eauto.
  rewrite IHT.
  reflexivity.
Qed.


Require Import Logic.FunctionalExtensionality.

Lemma map_eq: forall (A B: Type) (L: list A) (f g: A -> B),
    (forall a, In a L -> f a = g a) ->
    map f L = map g L.
Proof.
  intros.
  induction L.
  reflexivity.
  compute.
  rewrite H.
  assert ((fix map (l : list A) : list B :=
             match l with
             | [] => []
             | a0 :: t => f a0 :: map t
             end) L = map f L).
  reflexivity.
  rewrite H0.
  assert ((fix map (l : list A) : list B :=
             match l with
             | [] => []
             | a0 :: t => g a0 :: map t
             end) L = map g L).
  reflexivity.
  rewrite H1.
  rewrite IHL.
  reflexivity.
  intros.
  apply H.
  apply in_cons.
  assumption.
  apply in_eq.
Qed.

Lemma filter_map: forall (T: threads) (f: nat -> bool) (g: nat -> nat * value),
    (forall x, fst (g x) = x) ->
    filter (fun tv => f (fst tv)) (map g T) =
    map g (filter f T).
Proof.
  intros.
  induction T.
  reflexivity.
  simpl.
  remember (f a) as b.
  induction b.
  rewrite (H a).
  rewrite <- Heqb.
  simpl.
  apply f_equal.
  apply IHT.
  rewrite (H a).
  rewrite <- Heqb.
  apply IHT.
Qed.

Lemma R_eq_opd: forall M S T o R C,
    eval_opd M S T o R C ->
    exists f, R = map (fun t => (t, f t)) T.
Proof.
  intros.
  induction H; [ exists f
                     | exists (fun t => S (I.ILVar x t))
                     | exists (fun t => S (I.IPar x))
                     | exists (fun t => v)
                     | exists (fun t => VNat t)];
  apply map_eq; intros; eauto.
Qed.

Hint Resolve R_eq_opd: infer.

Lemma eq_map: forall (A B: Type) (L: list A) (f g: A -> B),
    map f L = map g L ->
    (forall x, In x L -> f x = g x).
Proof.
  intros.
  induction L.
  apply in_nil in H0.
  contradiction.
  simpl in H.
  inversion H.
  apply in_inv in H0.
  destruct H0.
  subst x.
  assumption.
  apply IHL; assumption.
Qed.

Lemma filter_T_sub_opd: forall M S T o R C f,
    eval_opd M S T o R C ->
    forall t, In t (map (fun tv : nat * value => fst tv) (filter f R)) ->
              In t T.
Proof.
  intros.
  induction H; eauto with infer;

  apply in_map_iff in H0;
  destruct H0;
  destruct H;
  apply filter_In in H0;
  destruct H0;
  apply in_map_iff in H0;
  destruct H0;
  destruct H0;
  rewrite <- H0 in H;
  simpl in H;
  try (subst x1; assumption);
    try (subst x0; assumption).
Qed.

Hint Resolve filter_T_sub_opd : infer.

Lemma filter_T_sub: forall M S T e R C f,
    eval_exp M S T e R C ->
    forall t, In t (map (fun tv : nat * value => fst tv) (filter f R)) ->
              In t T.
Proof.
  intros.
  induction H; eauto with infer.

  apply in_map_iff in H0.
  destruct H0.
  destruct H.
  apply filter_In in H0.
  destruct H0.
  apply in_map_iff in H0.
  destruct H0.
  destruct H0.
  subst x.
  simpl in H.
  subst x0.
  assumption.
  
  apply fst_eval_T_opd in H.
  rewrite map_map in H.
  simpl in H.
  apply in_map_iff in H0.
  destruct H0.
  destruct H0.
  apply filter_In in H1.
  destruct H1.
  apply in_map_iff in H1.
  destruct H1.
  destruct H1.
  rewrite <- H1 in H0.
  simpl in H0.
  rewrite <- H.
  apply in_map_iff.
  exists x0.
  split; assumption.

  apply fst_eval_T_opd in H.
  rewrite map_map in H.
  simpl in H.
  apply in_map_iff in H0.
  destruct H0.
  destruct H0.
  apply filter_In in H1.
  destruct H1.
  apply in_map_iff in H1.
  destruct H1.
  destruct H1.
  rewrite <- H1 in H0.
  simpl in H0.
  rewrite <- H.
  apply in_map_iff.
  exists x0.
  split; assumption.
Qed.

Lemma P_invar_arr : forall P T S aty A inds vs S',
    PST P T S ->
    Gassign S aty A inds vs S' ->
    PST P T S'.
Proof.
  intros.
  apply (PST_depend _ _ S); try assumption. 
  intros.
  destruct H0.
  destruct (H2 (I.ILVar x t)).
  rewrite H4; eauto.
  intro.
  do 4 destruct H5.
  inversion H5.
Qed.

Hint Resolve P_invar_arr : infer.

(** States of inactive threads are unaffected by evaluation **)
Lemma P_others_invar : forall M S T s S' Cs P T',
    (forall t, In t T' -> ~ In t T) ->
    PST P T' S ->
    eval_stmt M S T s S' Cs ->
    PST P T' S'.
Proof.
  intros.
  induction H1; eauto with infer.

  + apply (PST_depend _ _ S); try assumption.
    intros.
    destruct (H2 (I.ILVar x0 t)).
    apply H5.
    intro.
    destruct H6.
    destruct H6.
    destruct H6.
    apply (H t).
    assumption.
    rewrite <- (fst_eval_T M S _ e vs C).
    apply in_map_iff.
    exists (x1, x2).
    split.
    inversion H6.
    reflexivity.
    assumption.
    assumption.

  + apply IHeval_stmt2.
    intros.
    intro.
    subst TF.
    apply (filter_T_sub M S T e _ C) in H8; try assumption.
    apply H in H7.
    contradiction.
    apply IHeval_stmt1.
    intros.
    intro.
    subst TT.
    apply (filter_T_sub M S T e _ C) in H8; try assumption.
    apply H in H7.
    contradiction.
    assumption.

  + apply IHeval_stmt2.
    intros.
    intro.
    subst TT.
    apply (filter_T_sub M S T e _ C) in H6; try assumption.
    apply H in H6.
    contradiction.
    assumption.
    apply IHeval_stmt1.
    intros.
    intro.
    subst TT.
    apply (filter_T_sub M S T e _ C) in H6; try assumption.
    apply H in H6.
    contradiction.
    assumption.
    assumption.
Qed.


Hint Unfold writesto: infer.

(** States of non-written variables are unaffected **)

Lemma S_notwritten: forall M S T s S' Cs x,
    ~ writesto s x ->
    eval_stmt M S T s S' Cs ->
    forall t, S' (I.ILVar x t) = S (I.ILVar x t).
Proof.
  intros.
  induction H0; eauto with infer;
    try solve [apply IHeval_stmt; intro; apply H; eauto with infer].

  destruct (H1 (I.ILVar x t)).
  apply H3.
  intro.
  destruct H4.
  destruct H4.
  destruct H4.
  inversion H4.
  apply H.
  unfold writesto.
  assumption.

  destruct H4.
  destruct (H5 (I.ILVar x t)).
  rewrite H7; eauto.
  intro.
  do 4 destruct H8.
  inversion H8.

  destruct H4.
  destruct (H5 (I.ILVar x t)).
  rewrite H7; eauto.
  intro.
  do 4 destruct H8.
  inversion H8.

  rewrite IHeval_stmt2; try (intro; apply H; eauto with infer).
  apply IHeval_stmt1; try (intro; apply H; eauto with infer).

  rewrite IHeval_stmt2; try (intro; apply H; eauto with infer).
  apply IHeval_stmt1; try (intro; apply H; eauto with infer).

  rewrite IHeval_stmt2; try (intro; apply H; eauto with infer).
  apply IHeval_stmt1; try (intro; apply H; eauto with infer).

  rewrite IHeval_stmt2; try (intro; apply H; eauto with infer).
  apply IHeval_stmt1; try (intro; apply H; eauto with infer).
Qed.

Lemma TF_notin_TT: forall R TT TF,
    uniq_r R ->
    TT = map (fun tv => fst tv) (filter is_true R) ->
    TF = map (fun tv => fst tv) (filter is_false R) ->
    (forall t, In t TF -> ~ In t TT).
Proof.
  intros R TT TF H_uniq.
  intros.
  subst TT TF.
  apply in_map_iff in H1.
  destruct H1.
  destruct H.
  intro.
  apply in_map_iff in H1.
  destruct H1.
  destruct H1.
  apply filter_In in H0.
  destruct H0.
  apply filter_In in H2.
  destruct H2.
  apply is_false_true in H3.
  apply is_true_true in H4.
  induction H_uniq.
  apply in_nil in H0.
  assumption.
  apply in_inv in H0.
  destruct H0.
  apply in_inv in H2.
  destruct H2.
  subst x0.
  subst x.
  rewrite H3 in H4.
  discriminate.
  apply H5.
  exists (snd x0).
  subst tv.
  rewrite H.
  rewrite <- H1.
  rewrite <- surjective_pairing.
  assumption.
  apply in_inv in H2.
  destruct H2.
  apply H5.
  exists (snd x).
  subst tv.
  rewrite H1.
  rewrite <- H.
  rewrite <- surjective_pairing.
  assumption.
  apply IHH_uniq; assumption.
Qed.

Lemma TT_notin_TF: forall R TT TF,
    uniq_r R ->
    TT = map (fun tv => fst tv) (filter is_true R) ->
    TF = map (fun tv => fst tv) (filter is_false R) ->
    (forall t, In t TT -> ~ In t TF).
Proof.
  intros R TT TF H_uniq.
  intros.
  subst TT TF.
  apply in_map_iff in H1.
  destruct H1.
  destruct H.
  intro.
  apply in_map_iff in H1.
  destruct H1.
  destruct H1.
  apply filter_In in H0.
  destruct H0.
  apply filter_In in H2.
  destruct H2.
  apply is_true_true in H3.
  apply is_false_true in H4.
  induction H_uniq.
  apply in_nil in H0.
  assumption.
  apply in_inv in H0.
  destruct H0.
  apply in_inv in H2.
  destruct H2.
  subst x0.
  subst x.
  rewrite H3 in H4.
  discriminate.
  apply H5.
  exists (snd x0).
  subst tv.
  rewrite H.
  rewrite <- H1.
  rewrite <- surjective_pairing.
  assumption.
  apply in_inv in H2.
  destruct H2.
  apply H5.
  exists (snd x).
  subst tv.
  rewrite H1.
  rewrite <- H.
  rewrite <- surjective_pairing.
  assumption.
  apply IHH_uniq; assumption.
Qed.

Lemma TF_TT_T : forall Sig M S T e R C TT TF,
    typed_exp Sig e TBool ->
    typed_state Sig S ->
    eval_exp M S T e R C ->
    TT = map (fun tv => fst tv) (filter is_true R) ->
    TF = map (fun tv => fst tv) (filter is_false R) ->
    forall t : nat, In t TT \/ In t TF <-> In t T.
  intros Sig M S T e R C TT TF H_typed H_typedS. intros.
  subst TT TF.
  split; intro.
  destruct H0.
  apply (filter_T_sub M S _ e R C is_true); try assumption.
  apply (filter_T_sub M S _ e R C is_false); try assumption.
  rewrite <- (fst_eval_T M S T e R C) in H0; try assumption.
  apply in_map_iff in H0.
  destruct H0.
  destruct H0.
  assert (snd x = VBool true \/ snd x = VBool false).
  apply (pres_exp M Sig _ _ _ TBool) in H; try assumption.
  rewrite (surjective_pairing x) in H1.
  apply H in H1.
  inversion H1.
  induction b; [left | right]; reflexivity.
  destruct H2.
  left.
  apply in_map_iff.
  exists x.
  split. assumption.
  apply filter_In.
  split. assumption.
  unfold is_true.
  rewrite H2.
  reflexivity.
  right.
  apply in_map_iff.
  exists x.
  split. assumption.
  apply filter_In.
  split. assumption.
  unfold is_false.
  rewrite H2.
  reflexivity.
Qed.

Lemma filter_true: forall A f (L: list A),
    (forall x, In x L -> f x = true) ->
    filter f L = L.
Proof.
  intros.
  induction L.
  reflexivity.
  simpl.
  rewrite (H a).
  apply f_equal.
  apply IHL.
  intros.
  apply H.
  apply in_cons.
  assumption.
  apply in_eq.
Qed.

Lemma arr_assign: forall S S' T c aty A inds vs,
    S ;; T |= c ->
    Gassign S aty A inds vs S' ->
    S' ;; T |= c.
Proof.
  intros.
  destruct H.
  destruct H1.
  destruct H2.
  split.
  assumption.
  split.
  apply (P_invar_arr _ _ S aty A inds vs); assumption.
  split.
  apply (PG_depend _ T S); eauto.
  intros.
  destruct H0.
  destruct (H5 (I.ILVar x t)).
  apply H7.
  intro.
  do 4 destruct H8.
  inversion H8.
  intros.
  destruct H0.
  destruct (H7 (I.ILVar x t1)).
  destruct (H7 (I.ILVar x t2)).
  rewrite H9.
  rewrite H11.
  apply H3; eauto.
  intro.
  do 4 destruct H12.
  inversion H12.
  intro.
  do 4 destruct H12.
  inversion H12.
Qed.
  
(*** Soundness of statement evaluation (Theorem 1 of submission) ***)

Theorem cstmt_sound : forall Sig M c s c' S T S' C Cs,
    valid Sig s ->
    typed_state Sig S ->
    cstmt M c s c' ->
    (S ;; T |= c) ->
    T <> [] ->
    uniq_t T ->
    eval_stmt M S T s S' Cs ->
    Qap (Q c) S (X c) C ->
    exists C', (S' ;; T |= c') /\ Qap (Q c') S' (X c') C' /\ C - Cs >= C'.
Proof.
  intros Sig M c s c' S T S' C Cs H_valid H_typedS H_cstmt H_comp H_T H_uniq H_eval H_Q.
  generalize dependent T.
  generalize dependent S.
  generalize dependent S'.
  generalize dependent C.
  generalize dependent Cs.
  generalize dependent c.

  induction 1; intros Cs C0 S' S H_typedS H_Q T H_comp H_T H_uniq H_eval; eauto.

  + (* QSkip *)
    inversion H_eval.
    subst M0 S0 T0 S' Cs.
    exists C0.
    split. assumption.
    split. assumption.
    assert (C0 - 0 == C0). ring.
    rewrite H.
    apply Qle_refl.

  + (* QIf1 *)
    apply Qplus_ap in H_Q.
    apply Qplus_ap in H_Q.
    inversion H_eval.
    - (* SCIfT *)
      subst M0 S0 T0 e0 s0 s3 S'0 Cs.
      assert (S;; T |= {{P0 /s\ Pe e; Q0; X0}}) as H_comp'.
      apply (compat_Q (Q0 <+ MIf M <+ C)).
      destruct (Pe_correct1 e S T).
      intros.
      apply (eval_exp_deterministic M0 _ _ _ _ R C2) in H9; try assumption.
      subst R.
      apply in_map_iff in H2.
      destruct H2.
      destruct H2.
      inversion H2.
      reflexivity.
      split.
      apply Pand_correct2.
      apply H_comp.
      assumption.
      split.
      apply Pand_correct4.
      apply H_comp.
      apply H2.
      split.
      apply Pand_correct6.
      apply H_comp.
      apply H2.
      apply H_comp.
      assert (valid Sig s1) as H_valid'.
      inversion H_valid. assumption.
      destruct (IHH_cstmt1 H_valid' _ _ _ _ H_typedS H_Q _ H_comp' H_T H_uniq H10) as [C'1 IH].
      exists C'1.
      split. apply IH.
      split. apply IH.
      assert (C0 - (C1 ++ C' ++ MIf M) == C0 - C1 - MIf M - C'). ring.
      rewrite H1.
      apply (Qle_trans _ (C0 - C - MIf M - C')).
      apply IH.
      repeat apply Qplus_le_compat;
        try (apply Qle_refl); try apply Qopp_le_compat; eauto.
      apply (cexp_sound _ P0 C) in H9; try assumption; try apply H_comp.

    - (* SCIfF *)
      subst M0 S0 T0 e0 s0 s3 S'0 Cs.
      assert (S;; T |= {{P0 /s\ Pne e; Q0; X0}}) as H_comp'.
      apply (compat_Q (Q0 <+ MIf M <+ C)).
      destruct (Pne_correct1 e S T).
      intros.
      apply (eval_exp_deterministic M0 _ _ _ _ R C2) in H9; try assumption.
      subst R.
      apply in_map_iff in H2.
      destruct H2.
      destruct H2.
      inversion H2.
      reflexivity.
      split.
      apply Pand_correct2.
      apply H_comp.
      assumption.
      split.
      apply Pand_correct4.
      apply H_comp.
      apply H2.
      split.
      apply Pand_correct6.
      apply H_comp.
      apply H2.
      apply H_comp.
      assert (valid Sig s2) as H_valid'.
      inversion H_valid. assumption.
      destruct (IHH_cstmt2 H_valid'  _ _ _ _ H_typedS H_Q _ H_comp' H_T H_uniq H10) as [C'1 IH].
      exists C'1.
      split. apply IH.
      split. apply IH.
      assert (C0 - (C1 ++ C' ++ MIf M) == C0 - C1 - MIf M - C'). ring.
      rewrite H1.
      apply (Qle_trans _ (C0 - C - MIf M - C')).
      apply IH.
      repeat apply Qplus_le_compat;
        try (apply Qle_refl); try apply Qopp_le_compat; eauto.
      apply (cexp_sound _ P0 C) in H9; try assumption; try apply H_comp.

    - (* SCIfD *)
      induction TT, TF.
      destruct H7. reflexivity.
      destruct H7. reflexivity.
      destruct H8. reflexivity.
      assert (In a (map (fun tv : nat * value => fst tv) (filter is_true R))).
      rewrite <- H5.
      apply in_eq.
      assert (In n (map (fun tv : nat * value => fst tv) (filter is_false R))).
      rewrite <- H6.
      apply in_eq.
      destruct H_comp.
      destruct H20.
      destruct H21.
      edestruct (H0 T S H19 H20 H21 M R C1).
      apply H4.
      assert (In (a, VBool true) R).
      apply in_map_iff in H17.
      destruct H17.
      destruct H17.
      apply filter_In in H24.
      destruct H24.
      rewrite (surjective_pairing x0) in H24.
      rewrite H17 in H24.
      apply is_true_true in H25.
      rewrite H25 in H24.
      assumption.
      apply H23 in H24.
      assert (In (n, VBool false) R).
      apply in_map_iff in H18.
      destruct H18.
      destruct H18.
      apply filter_In in H25.
      destruct H25.
      rewrite (surjective_pairing x0) in H25.
      rewrite H18 in H25.
      apply is_false_true in H26.
      rewrite H26 in H25.
      assumption.
      apply H23 in H25.
      subst x.
      discriminate.

  + (* QIf2 *)
    apply Qplus_ap in H_Q.
    apply Qplus_ap in H_Q.
    inversion H_eval.
    - (* SCIfT *)
      subst M0 S0 T0 e0 s0 s3 S'0 Cs.
      assert (S;; T |= {{P0; Q0; X0}}) as H_comp'.
      apply (compat_Q (Q0 <+ MIf M <+ C)).
      assumption.
      assert (valid Sig s1) as H_valid'.
      inversion H_valid. assumption.
      destruct (IHH_cstmt H_valid'  _ _ _ _ H_typedS H_Q _ H_comp' H_T H_uniq H10) as [C'1 IH].
      exists C'1.
      split. apply IH.
      split. apply IH.
      assert (C0 - (C1 ++ C' ++ MIf M) == C0 - C1 - MIf M - C'). ring.
      rewrite H1.
      apply (Qle_trans _ (C0 - C - MIf M - C')).
      apply IH.
      repeat apply Qplus_le_compat;
        try (apply Qle_refl); try apply Qopp_le_compat; eauto.
      apply (cexp_sound _ P0 C) in H9; try assumption; try apply H_comp.

    - (* SCIfF *)
      destruct H0.
      induction T.
      destruct H_T. reflexivity.
      assert (In (a, VBool false) (map (fun t : nat => (t, VBool false)) (a :: T))).
      apply in_map_iff.
      exists a.
      split; [reflexivity | apply in_eq].
      destruct H_comp.
      destruct H14.
      destruct H11.
      destruct H15.
      apply (Pe_correct2 e S (a::T) (H0 _ H13) (H11 _ _ H13 H14) (H16 _ H15) M _ C1) in H12;
        try assumption.
      discriminate.

    - (* SCIfD *)
      induction TF.
      destruct H8. reflexivity.
      assert (In (a, VBool false) R).
      assert (In a (map (fun tv : nat * value => fst tv) (filter is_false R))).
      rewrite <- H6.
      apply in_eq.
      apply in_map_iff in H17.
      destruct H17.
      destruct H17.
      apply filter_In in H18.
      destruct H18.
      apply is_false_true in H19.
      rewrite (surjective_pairing x) in H18.
      rewrite H17 in H18.
      rewrite H19 in H18.
      assumption.
      destruct H_comp.
      destruct H19.
      destruct H0.
      destruct H20.
      destruct H21.
      apply (Pe_correct2 e S T (H0 _ H18) (H21 _ _ H18 H19) (H23 _ H20)  M _ C1) in H17;
        try assumption.
      discriminate.

  + (* QIf3 *)
    apply Qplus_ap in H_Q.
    apply Qplus_ap in H_Q.
    inversion H_eval.
    - (* SCIfT *)
      destruct H0.
      induction T.
      destruct H_T. reflexivity.
      assert (In (a, VBool true) (map (fun t : nat => (t, VBool true)) (a :: T))).
      apply in_map_iff.
      exists a.
      split; [reflexivity | apply in_eq].
      destruct H_comp.
      destruct H14.
      destruct H15.
      destruct H11.
      apply (Pne_correct2 e S (a::T) (H0 _ H13) (H11 _ _ H13 H14) (H17 _ H15) M _ C1) in H12;
        try assumption.
      discriminate.

    - (* SCIfF *)
      subst M0 S0 T0 e0 s0 s3 S'0 Cs.
      assert (S;; T |= {{P0; Q0; X0}}) as H_comp'.
      apply (compat_Q (Q0 <+ MIf M <+ C)).
      assumption.
      assert (valid Sig s2) as H_valid'.
      inversion H_valid. assumption.
      destruct (IHH_cstmt H_valid' _ _ _ _ H_typedS H_Q _ H_comp' H_T H_uniq H10) as [C'1 IH].
      exists C'1.
      split. apply IH.
      split. apply IH.
      assert (C0 - (C1 ++ C' ++ MIf M) == C0 - C1 - MIf M - C'). ring.
      rewrite H1.
      apply (Qle_trans _ (C0 - C - MIf M - C')).
      apply IH.
      repeat apply Qplus_le_compat;
        try (apply Qle_refl); try apply Qopp_le_compat; eauto.
      apply (cexp_sound _ P0 C) in H9; try assumption; try apply H_comp.

    - (* SCIfD *)
      induction TT.
      destruct H7. reflexivity.
      assert (In (a, VBool true) R).
      assert (In a (map (fun tv : nat * value => fst tv) (filter is_true R))).
      rewrite <- H5.
      apply in_eq.
      apply in_map_iff in H17.
      destruct H17.
      destruct H17.
      apply filter_In in H18.
      destruct H18.
      apply is_true_true in H19.
      rewrite (surjective_pairing x) in H18.
      rewrite H17 in H18.
      rewrite H19 in H18.
      assumption.
      destruct H_comp.
      destruct H19.
      destruct H0.
      destruct H20.
      destruct H21.
      apply (Pne_correct2 e S T (H0 _ H18) (H21 _ _ H18 H19) (H23 _ H20) M _ C1) in H17;
        try assumption.
      discriminate.

  + (* QIf4 *)
    repeat apply Qplus_ap in H_Q.
    inversion H_eval.
    - (* SCIfT *)
      subst M0 S0 T0 e0 s0 s3 S'0 Cs.
      assert (S;; T |= {{P0 /s\ Pe e; Q0; X0}}) as H_comp'.
      destruct (Pe_from_eval M S T e C1); try assumption.
      apply (compat_Q (Q0 <+ MIf M <+ C <+ MDiv M)).
      split. apply (Pand_correct2). apply H_comp. apply H6.
      split. apply (Pand_correct4). apply H_comp. apply H7.
      split. apply (Pand_correct6). apply H_comp. apply H7.
      apply H_comp.
      assert (valid Sig s1) as H_valid'.
      inversion H_valid. assumption.
      destruct (IHH_cstmt1 H_valid' _ _ _ _ H_typedS H_Q _ H_comp' H_T H_uniq H15) as [C'1 IH1].
      destruct IH1 as [IH1 IH1'].
      destruct IH1' as [IH1' IH1''].
      assert (forall x : var, In x X'' -> In x X1) as H_in.
      intros.
      apply H4.
      apply (cstmt_mono_X _ _ _ _ H_cstmt2).
      apply H5.
      assumption.
      assert (Qgt Q1 Q2).
      apply (cstmt_mono_Q _ _ _ _ H_cstmt2).
      destruct (Qgt_exists _ _ _ _ _ H6 IH1') as [C''].
      destruct H7.
      apply (Qap_sub_X _ _ _ _ X'') in H8.
      destruct H8 as [C'''].
      exists C'''.
      split.
      apply (compat_P_implies P1); try assumption.
      apply (compat_X _ _ _ X1); assumption.
      split.
      apply H8.
      assert (C0 - (C1 ++ C' ++ MIf M) == C0 - 0 - C1 - MIf M - C'). ring.
      rewrite H9.
      apply (Qle_trans _ C'').
      apply H8.
      apply (Qle_trans _ C'1).
      apply H7.
      apply (Qle_trans _ (C0 - MDiv M - C - MIf M - C')).
      apply IH1''.
      repeat apply Qplus_le_compat; try apply Qle_refl; apply Qopp_le_compat.
      eauto with cost.
      apply (cexp_sound _ P0 C) in H14; try assumption; try apply H_comp.
      intros.
      apply H_in.
      assumption.

    - (* SCIfF *)
      subst M0 S0 T0 e0 s0 s3 S'0 Cs.
      assert (forall x : var, In x X' -> In x X0) as H_in.
      intros.
      apply (cstmt_mono_X _ _ _ _ H_cstmt1).
      apply H4.
      assumption.
      assert (S;; T |= {{P'; Q1; X'}}) as H_comp'.
      apply (compat_X _ _ _ X0).
      apply (compat_P_implies (P0 /s\ Pne e)).
      assumption.
      destruct (Pne_correct1 e S T).
      intros.
      apply (eval_exp_deterministic M0 _ _ _ _ R C2) in H14; try assumption.
      subst R.
      apply in_map_iff in H7.
      destruct H7.
      destruct H7.
      inversion H7.
      reflexivity.
      split.
      apply Pand_correct2.
      apply H_comp.
      assumption.
      split.
      apply Pand_correct4.
      apply H_comp.
      apply H7.
      split.
      apply Pand_correct6.
      apply H_comp.
      apply H7.
      apply H_comp.
      assumption.
      assert (Qgt Q0 Q1).
      apply (cstmt_mono_Q _ _ _ _ H_cstmt1).
      destruct (Qgt_exists _ _ _ _ _ H6 H_Q) as [C'1].
      destruct H7.
      apply (Qap_sub_X _ _ _ _ X') in H8; try assumption.
      assert (valid Sig s2) as H_valid'.
      inversion H_valid. assumption.
      destruct H8 as [C'1'].
      destruct H8 as [H8 H8'].
      destruct (IHH_cstmt2 H_valid' _ _ _ _ H_typedS H8 _ H_comp' H_T H_uniq H15) as [C''].
      destruct H9.
      destruct H10.
      apply (Qap_sub_X _ _ _ _ X'') in H10.
      destruct H10 as [C'''].
      exists C'''.
      split.
      apply (compat_P_implies P2); try assumption.
      apply (compat_X _ _ _ X2).
      apply H9.
      apply H5.
      split.
      apply H10.
      assert (C0 - (C1 ++ C' ++ MIf M) == C0 - 0 - C1 - MIf M - C'). ring.
      rewrite H12.
      apply (Qle_trans _ C''). apply H10.
      apply (Qle_trans _ (C'1' - C')).
      apply H11.
      apply (Qle_trans _ (C'1 - C')).
      apply Qplus_le_compat; try apply Qle_refl.
      assumption.
      apply (Qle_trans _ (C0 - MDiv M - C - MIf M - C')).
      apply Qplus_le_compat.
      apply H7.
      apply Qle_refl.
      repeat apply Qplus_le_compat; try apply Qle_refl; apply Qopp_le_compat.
      eauto with cost.
      apply (cexp_sound _ P0 C) in H14; try assumption; try apply H_comp.
      apply H5.

    - (* SCIfD *)
      subst M0 S0 T0 e0 s0 s3 S' Cs.
      assert (S;; TT |= {{P0 /s\ Pe e; Q0; X0}}) as H_comp'.
      destruct (Pe_TT M S T e R C1 TT); try assumption.
      apply (compat_Q (Q0 <+ MIf M <+ C <+ MDiv M)).
      apply (compat_T_sub _ _ TT) in H_comp.
      split. apply (Pand_correct2). apply H_comp. apply H6.
      split. apply (Pand_correct4). apply H_comp. apply H7.
      split. apply (Pand_correct6). apply H_comp. apply H7.
      apply H_comp.
      subst TT.
      apply (filter_T_sub _ _ _ _ _ _ _ H9).
      assert (uniq_t TT) as H_uniq'.
      subst TT.
      apply map_pres_uniqrt.
      apply filter_pres_uniqr.
      apply (uniqr_eval_exp M S T e _ C1); try assumption.
      assert (valid Sig s1) as H_valid'.
      inversion H_valid. assumption.
      destruct (IHH_cstmt1 H_valid' _ _ _ _ H_typedS H_Q _ H_comp' H12 H_uniq' H20) as [C'1 IH1].
      destruct IH1 as [IH1 IH1'].
      destruct IH1' as [IH1' IH1''].
      assert (S1;; TF |= {{P'; Q1; X'}}) as H_comp''.
      split.
      apply H0.
      destruct (Pne_TF M S T e R C1 TF); try assumption.
      apply Pand_correct2.
      assert (S;; TF |= {{P0; Q0; X0}}).
      apply (compat_T_sub _ T).
      assumption.
      subst TF.
      apply (filter_T_sub _ _ _ _ _ _ _ H9).
      apply H8.
      apply H6.
      split.
      apply (P_others_invar M S TT s1 _ C2).
      apply (TF_notin_TT R); try assumption.
      apply (uniqr_eval_exp M S T e _ C1); try assumption.
      destruct (Pne_TF M S T e R C1 TF); try assumption.
      assert (S;; TF |= {{P0; Q0; X0}}).
      apply (compat_T_sub _ T).
      assumption.
      subst TF.
      apply (filter_T_sub _ _ _ _ _ _ _ H9).
      apply H0.
      apply Pand_correct2.
      apply H8.
      apply H6.
      apply Pand_correct4.
      apply H8.
      apply H7.
      assumption.
      split.
      apply H1.
      apply IH1.
      intros.
      apply H4 in H6.
      destruct H6.
      rewrite (S_notwritten _ _ _ _ _ _ _ H14 H20).
      rewrite (S_notwritten _ _ _ _ _ _ _ H14 H20).
      assert (S;; TF |= {{P0; Q0; X0}}).
      apply (compat_T_sub _ T).
      assumption.
      subst TF.
      apply (filter_T_sub _ _ _ _ _ _ _ H9).
      apply H15.
      apply (cstmt_mono_X _ _ _ _ H_cstmt1).
      assumption.
      assumption.
      assumption.
      apply (Qap_sub_X _ _ _ _ X') in IH1'.
      destruct IH1' as [C'1' H_Q']. destruct H_Q' as [H_Q' IH1'].
      assert (uniq_t TF) as H_uniq''.
      subst TF.
      apply map_pres_uniqrt.
      apply filter_pres_uniqr.
      apply (uniqr_eval_exp M S T e _ C1); assumption.
      assert (valid Sig s2) as H_valid''.
      inversion H_valid. assumption.
      assert (typed_state Sig S1) as H_typedS'.
      apply (preservation M _ S TT s1 _ C2); eauto.
      destruct (IHH_cstmt2 H_valid'' _ _ _ _ H_typedS' H_Q' _ H_comp'' H13 H_uniq'' H21) as [C'' IH2].
      destruct IH2 as [IH2 IH2'].
      destruct IH2' as [IH2' IH2''].
      apply (Qap_sub_X _ _ _ _ X'') in IH2'.
      destruct IH2' as [C'''].

      exists C'''.
      split.
      assert (PT P'' T).
      apply (PT_reconstruct _ _ TT TF).
      apply (TF_TT_T Sig M S _ e R C1); try assumption.
      inversion H_valid.
      assumption.
      apply H2.
      apply IH1.
      apply H3.
      apply IH2.
      split.
      apply H7.
      split.
      apply (PST_reconstruct _ _ TT TF).
      apply (TF_TT_T Sig M S _ e R C1); try assumption.
      inversion H_valid.
      assumption.
      apply H7.
      apply H2.
      apply IH1.
      apply (P_others_invar M S1 TF s2 _ C3).
      apply (TT_notin_TF R); try assumption.
      apply (uniqr_eval_exp M S T e _ C1); assumption.
      apply IH1.
      assumption.
      apply H3.
      apply IH2.
      apply IH2.
      split.
      apply H3.
      apply IH2.
      intros.
      apply H5 in H8.
      destruct H8.
      rewrite (S_notwritten _ _ _ _ _ _ _ H16 H21).
      rewrite (S_notwritten _ _ _ _ _ _ _ H16 H21).
      apply (cstmt_mono_X _ _ _ _ H_cstmt2) in H8.
      apply H4 in H8.
      destruct H8.
      rewrite (S_notwritten _ _ _ _ _ _ _ H18 H20).
      rewrite (S_notwritten _ _ _ _ _ _ _ H18 H20).
      apply (cstmt_mono_X _ _ _ _ H_cstmt1) in H8.
      apply H_comp.
      assumption.
      assumption.
      assumption.
      split.
      apply H6.
      assert (C0 - (C1 ++ C2 ++ C3 ++ MIf M ++ MDiv M) ==
              C0 - MDiv M - C1 - MIf M - C2 - C3).
      ring.
      rewrite H7.
      apply (Qle_trans _ C'').
      apply H6.
      apply (Qle_trans _ (C'1' - C3)).
      assumption.
      apply (Qle_trans _ (C'1 - C3)).
      apply Qplus_le_compat; try apply Qle_refl. assumption.
      apply Qplus_le_compat; try apply Qle_refl.
      apply (Qle_trans _ (C0 - MDiv M - C - MIf M - C2)).
      assumption.
      repeat apply Qplus_le_compat; try apply Qle_refl;
        try apply Qopp_le_compat; eauto with cost.
      apply (cexp_sound _ P0 C) in H9; try assumption; try apply H_comp.
      apply H5.
      apply H4.

  + (* QWhile1 *)
    remember (SWhile e s) as s0.
    assert (True) as H1. trivial.
    generalize dependent C0.
    induction H_eval; inversion Heqs0; intros C_0 H_Q; subst e0 s0.
    repeat apply Qplus_ap in H_Q.
    assert (S;; T |= {{P0 /s\ Pe e; Q0; X0}}) as H_comp'.
    destruct (Pe_from_eval M S T e C0); try assumption.
    split.
    apply (Pand_correct2).
    apply H_comp.
    assumption.
    split. apply (Pand_correct4).
    apply H_comp.
    apply H4.
    split. apply (Pand_correct6). apply H_comp. apply H4.
    apply H_comp.
    assert (valid Sig s) as H_valid'.
    inversion H_valid. assumption.
    destruct (IHH_cstmt H_valid' _ _ _ _ H_typedS H_Q _ H_comp' H_T H_uniq H_eval1) as [C'1 IH1].
    destruct IH1 as [IH1 IH1'].
    destruct IH1' as [IH1' IH1''].
    assert (typed_state Sig S1) as H_typedS'.
    apply (preservation M _ S T s _ C1); try assumption.
    edestruct (IHH_eval2 Heqs0 H_valid H H_cstmt IHH_cstmt H_typedS' IH1 H_T H_uniq _ IH1')
      as [C'' IH2].
    exists C''.
    split. apply IH2.
    split. apply IH2.
    assert (C_0 - (C0 ++ C1 ++ C2 ++ MIf M) == C_0 - C0 - MIf M - C1 - C2).
    ring.
    rewrite H3.
    apply (Qle_trans _ (C'1 - C2)).
    apply IH2.
    apply Qplus_le_compat.
    apply (Qle_trans _ (C_0 - C - MIf M - C1)).
    assumption.
    repeat apply Qplus_le_compat; try apply Qle_refl;
      try apply Qopp_le_compat; eauto with cost.
    apply (cexp_sound _ P0 C) in H2; try assumption; try apply H_comp.
    apply Qle_refl.

    unfold unif in H0.
    edestruct (H0) as [v].
    apply H_comp.
    apply H_comp.
    apply H_comp.
    apply H2.
    induction TT.
    destruct H4.
    reflexivity.
    assert (In a (map (fun tv : nat * value => fst tv) (filter is_true R))).
    rewrite <- H3.
    apply in_eq.
    apply in_map_iff in H7.
    do 2 destruct H7.
    apply filter_In in H8.
    destruct H8.
    apply is_true_true in H9.
    rewrite (surjective_pairing x) in H8.
    apply H6 in H8.
    rewrite H9 in H8.
    subst v.
    destruct H5.
    rewrite H3.
    assert (filter is_true R = R).
    apply filter_true.
    intros.
    rewrite (surjective_pairing x0) in H5.
    apply H6 in H5.
    unfold is_true.
    rewrite H5.
    reflexivity.
    rewrite H5.
    apply (fst_eval_T M S _ e _ C0); try assumption.

    repeat apply Qplus_ap in H_Q.
    destruct (Pne_correct1 e S T).
    intros.
    apply (eval_exp_deterministic M _ _ _ _ (map (fun t : nat => (t, VBool false)) T) C0) in H3.
    subst R.
    apply in_map_iff in H4.
    destruct H4.
    destruct H3.
    inversion H3.
    reflexivity.
    assumption.
    exists (C_0 - C - MIf M).
    split.
    split.
    apply (Pand_correct2); [apply H_comp | apply H3].
    split. apply Pand_correct4; [apply H_comp | apply H4].
    split. apply Pand_correct6; [apply H_comp | apply H4].
    intros.
    apply H_comp.
    assumption.
    assumption.
    assumption.
    split.
    assumption.
    assert (C_0 - (C0 ++ MIf M) == C_0 - C0 - MIf M). ring.
    rewrite H5.
    repeat apply Qplus_le_compat; try apply Qle_refl;
      try apply Qopp_le_compat; eauto with cost.
    apply (cexp_sound _ P0 C) in H2; try assumption; try apply H_comp.

  + (* QWhile2 *)
    remember (SWhile e s) as s0.
    generalize dependent C0.
    induction H_eval; inversion Heqs0; intros C_0 H_Q; subst e0 s0.
    repeat apply Qplus_ap in H_Q.
    assert (S;; T |= {{P0 /s\ Pe e; Q0; X0}}) as H_comp'.
    destruct (Pe_from_eval M S T e C0); try assumption.
    split. apply (Pand_correct2). apply H_comp. apply H2.
    split. apply (Pand_correct4). apply H_comp. apply H3.
    split. apply (Pand_correct6). apply H_comp. apply H3.
    apply H_comp.
    assert (valid Sig s) as H_valid'.
    inversion H_valid. assumption.
    destruct (IHH_cstmt H_valid' _ _ _ _ H_typedS H_Q _ H_comp' H_T H_uniq H_eval1) as [C'1 IH1].
    destruct IH1 as [IH1 IH1'].
    destruct IH1' as [IH1' IH1''].
    assert (typed_state Sig S1) as H_typedS'.
    apply (preservation M _ S T s _ C1); try assumption.
    edestruct (IHH_eval2 Heqs0 H_valid H H_cstmt IHH_cstmt H_typedS' IH1 H_T H_uniq _ IH1')
      as [C'' IH2].
    exists C''.
    split. apply IH2.
    split. apply IH2.
    assert (C_0 - (C0 ++ C1 ++ C2 ++ MIf M) == C_0 - 0 - C0 - MIf M - C1 - C2).
    ring.
    rewrite H2.
    apply (Qle_trans _ (C'1 - C2)).
    apply IH2.
    apply Qplus_le_compat.
    apply (Qle_trans _ (C_0 - MDiv M - C - MIf M - C1)).
    assumption.
    repeat apply Qplus_le_compat; try apply Qle_refl;
      try apply Qopp_le_compat; eauto with cost.
    apply (cexp_sound _ P0 C) in H1; try assumption; try apply H_comp.
    apply Qle_refl.

    repeat apply Qplus_ap in H_Q.
    assert (S;; TT |= {{P0 /s\ Pe e; Q0; X0}}) as H_comp'.
    destruct (Pe_TT M S T e R C0 TT); try assumption.
    split. apply (Pand_correct2). apply (PT_sub _ T). apply H_comp.
    subst TT.
    apply (filter_T_sub _ _ _ _ _ _ _ H1).
    apply H5.
    split. apply (Pand_correct4).
    apply (PST_sub _ T). apply H_comp. subst TT.
    apply (filter_T_sub _ _ _ _ _ _ _ H1).
    apply H6.
    split. apply (Pand_correct6). apply H_comp. apply H6.
    intros.
    apply H_comp.
    assumption.
    subst TT.
    apply (filter_T_sub M S _ e R C0 is_true); try assumption.
    subst TT.
    apply (filter_T_sub M S _ e R C0 is_true); try assumption.
    assert (uniq_t TT) as H_uniq'.
    subst TT.
    apply map_pres_uniqrt.
    apply filter_pres_uniqr.
    apply (uniqr_eval_exp M S T e _ C0); assumption.
    assert (valid Sig s) as H_valid'.
    inversion H_valid. assumption.
    destruct (IHH_cstmt H_valid' _ _ _ _ H_typedS H_Q _ H_comp' H3 H_uniq' H_eval1) as [C'1 IH1].
    destruct IH1 as [IH1 IH1'].
    destruct IH1' as [IH1' IH1''].
    assert (typed_state Sig S1) as H_typedS'.
    apply (preservation M _ S TT s _ C1); try assumption.
    edestruct (IHH_eval2 Heqs0 H_valid H H_cstmt IHH_cstmt H_typedS' IH1 H3 H_uniq' _ IH1')
      as [C'' IH2].
    exists C''.
    set (TF := map (fun tv : nat * value => fst tv) (filter is_false R)).
    assert (S;; TF |= {{P0 /s\ Pne e; Q0; X0}}) as H_compF.
    destruct (Pne_TF M S T e R C0 TF); try assumption.
    reflexivity.
    split. apply (Pand_correct2). apply (PT_sub _ T). apply H_comp.
    subst TF.
    apply (filter_T_sub _ _ _ _ _ _ _ H1).
    apply H5.
    split. apply (Pand_correct4).
    apply (PST_sub _ T). apply H_comp. subst TF.
    apply (filter_T_sub _ _ _ _ _ _ _ H1).
    apply H6.
    split. apply (Pand_correct6). apply H_comp. apply H6.
    intros.
    apply H_comp.
    assumption.
    apply (filter_T_sub M S _ e R C0 is_false); try assumption.
    apply (filter_T_sub M S _ e R C0 is_false); try assumption.
    assert (PT (P {{P0 /s\ Pne e; Q0; X'}}) T).
    apply (PT_reconstruct _ _ TT TF).
    apply (TF_TT_T Sig M S _ e R C0); try assumption.
    inversion H_valid.
    assumption.
    reflexivity.
    apply IH2.
    apply H_compF.
    split.
    split.
    assumption.
    split.
    apply (PST_reconstruct _ _ TT TF).
    apply (TF_TT_T Sig M S _ e R C0); try assumption.
    inversion H_valid.
    assumption.
    reflexivity.
    assumption.
    apply IH2.
    apply (P_others_invar M S1 TT (SWhile e s) _ C2); try assumption.
    apply (TF_notin_TT R); try assumption.
    apply (uniqr_eval_exp M S T e _ C0); assumption.
    reflexivity.
    apply (P_others_invar M S TT s _ C1); try assumption.
    apply (TF_notin_TT R); try assumption.
    apply (uniqr_eval_exp M S T e _ C0); assumption.
    reflexivity.
    apply H_compF.
    split.
    apply IH2.
    intros.
    apply H0 in H6.
    destruct H6.
    assert (~ writesto (SWhile e s) x).
    intro.
    apply H9.
    eauto with infer.
    rewrite (S_notwritten _ _ _ _ _ _ _ H10 H_eval2).
    rewrite (S_notwritten _ _ _ _ _ _ _ H10 H_eval2).
    rewrite (S_notwritten _ _ _ _ _ _ _ H9 H_eval1).
    rewrite (S_notwritten _ _ _ _ _ _ _ H9 H_eval1).
    apply H_comp; assumption.
    split.
    apply IH2.
    assert (C_0 - (C0 ++ C1 ++ C2 ++ MIf M ++ MDiv M) ==
            C_0 - MDiv M - C0 - MIf M - C1 - C2).
    ring.
    rewrite H6.
    apply (Qle_trans _ (C'1 - C2)).
    apply IH2.
    apply Qplus_le_compat.
    apply (Qle_trans _ (C_0 - MDiv M - C - MIf M - C1)).
    assumption.
    repeat apply Qplus_le_compat; try apply Qle_refl;
      try apply Qopp_le_compat; eauto with cost.
    apply (cexp_sound _ P0 C) in H1; try assumption; try apply H_comp.
    apply Qle_refl.

    repeat apply Qplus_ap in H_Q.
    destruct (Pne_correct1 e S T).
    intros.
    apply (eval_exp_deterministic M _ _ _ _ (map (fun t : nat => (t, VBool false)) T) C0) in H2.
    subst R.
    apply in_map_iff in H3.
    destruct H3.
    destruct H2.
    inversion H2.
    reflexivity.
    assumption.
    apply (Qap_sub_X _ _ _ _ X') in H_Q.
    destruct H_Q as [C'].
    exists C'.
    split.
    split.
    apply (Pand_correct2); [apply H_comp | apply H2].
    split. apply Pand_correct4; [apply H_comp | apply H3].
    split. apply Pand_correct6; [apply H_comp | apply H3].
    intros.
    apply H_comp.
    apply H0.
    assumption.
    assumption.
    assumption.
    split.
    apply H4.
    assert (C_0 - (C0 ++ MIf M) == C_0 - 0 - C0 - MIf M). ring.
    rewrite H5.
    apply (Qle_trans _ (C_0 - MDiv M - C - MIf M)).
    apply H4.
    repeat apply Qplus_le_compat; try apply Qle_refl;
      try apply Qopp_le_compat; eauto with cost.
    apply (cexp_sound _ P0 C) in H1; try assumption; try apply H_comp.
    apply H0.

  + (* QSeq *)
    inversion H_eval.
    assert (valid Sig s1) as H_valid'.
    inversion H_valid. assumption.
    destruct (IHH_cstmt1 H_valid'  _ _ _ _ H_typedS H_Q _ H_comp H_T H_uniq H4) as [C'1 IH1].
    destruct IH1 as [IH1 IH1'].
    destruct IH1' as [IH1' IH1''].
    assert (valid Sig s2) as H_valid''.
    inversion H_valid. assumption.
    assert (typed_state Sig S1) as H_typedS'.
    apply (preservation M _ S T s1 _ C1); try assumption.
    destruct (IHH_cstmt2 H_valid''  _ _ _ _ H_typedS' IH1' _ IH1 H_T H_uniq H7) as [C'2 IH2].
    exists C'2.
    split. apply IH2.
    split. apply IH2.
    assert (C0 - (C1 ++ C2) == C0 - C1 - C2). ring.
    rewrite H8.
    apply (Qle_trans _ (C'1 - C2)).
    apply IH2.
    apply Qplus_le_compat.
    assumption.
    apply Qle_refl.

  + (* QVWrite1 *)
    inversion H_eval.
    subst M0 S0 T0 x0 e0 S'0 Cs.
    exists (C0 - C - MVWrite M).
    split.
    apply (compat_Q (QSub Q' x e <+ MVWrite M <+ C)).
    apply (PSub_correct P' x e S S' T); try eauto.
    unfold StateSub.
    exists T. exists M. exists vs. exists C1.
    intros.
    assumption.
    split.
    repeat apply Qplus_ap in H_Q.
    apply (QSub_correct (PSub P' x e) Q' x e S S'); try assumption.
    intros.
    apply H in H5; try assumption.
    exists T. exists M. exists vs. exists C1.
    intros.
    assumption.
    assert (C0 - (C1 ++ MVWrite M) == C0 - C1 - MVWrite M).
    ring.
    rewrite H2.
    repeat apply Qplus_le_compat; try apply Qle_refl.
    apply Qopp_le_compat.
    apply (cexp_sound _ (PSub P' x e) C) in H7; try assumption; try apply H_comp.

  + (* QVWrite2 *)
    inversion H_eval.
    subst M0 S0 T0 x0 e0 S'0 Cs.
    repeat apply Qplus_ap in H_Q.
    apply (Qap_sub_X _ _ _ _ X') in H_Q.
    destruct H_Q as [C'].
    exists C'.
    split.
    apply (compat_Q (Q0 <+ MVWrite M <+ C)).
    apply (PSub_correct P' x e S S' T); try eauto.
    unfold StateSub.
    exists T. exists M. exists vs. exists C1.
    intros.
    assumption.
    apply (compat_X _ _ _ X0).
    assumption.
    apply H0.
    split.

    apply (QSub_correct'' Q0 x e S S').
    intro.
    unfold varMinus in H0.
    destruct (H0 x).
    apply H3.
    assumption.
    constructor.
    exists T. exists M. exists vs. exists C1.
    intros.
    assumption.
    apply H1.
    assert (C0 - (C1 ++ MVWrite M) == C0 - C1 - MVWrite M).
    ring.
    rewrite H2.
    apply (Qle_trans _ (C0 - C - MVWrite M)).
    apply H1.
    repeat apply Qplus_le_compat; try apply Qle_refl.
    apply Qopp_le_compat.
    apply (cexp_sound _ (PSub P' x e) C) in H6; try assumption; try apply H_comp.
    apply H0.

  + (* QGWrite *)
    inversion H_eval.
    subst M0 S0 T0 A0 o0 e0 S' Cs.
    repeat apply Qplus_ap in H_Q.
    exists (C0 - C2 - C1 - MGWrite M n).
    split. apply (arr_assign S _ _ _ Global A inds vs); eauto.
    split. apply (QSub_correct''' Q0 Global A inds vs S S'0 X0 _) in H_Q; eauto.
    assert (C0 - (C3 ++
   C4 ++
   MGWrite M
   (memreads (map (fun tn : nat * nat => (fst tn, VNat (snd tn))) inds))) ==
            C0 - C4 - C3 -
            MGWrite M (memreads (map (fun tn : nat * nat => (fst tn, VNat (snd tn))) inds))).
    ring.
    rewrite H2.
    repeat apply Qplus_le_compat; try apply Qle_refl; apply Qopp_le_compat.
    apply (cexp_sound _ P0 C2) in H13; try assumption; try apply H_comp.
    apply (copd_sound _ P0 _ C1) in H10; try assumption; try apply H_comp.
    unfold numreads_lessthan in H.
    apply mgwrite_monotonic.
    eapply H.
    apply H_comp.
    apply H_comp.
    apply H_comp.
    apply H10.

  + (* QSWrite *)
    inversion H_eval.
    subst M0 S0 T0 A0 o0 e0 S' Cs.
    repeat apply Qplus_ap in H_Q.
    exists (C0 - C2 - C1 - MSWrite M n).
    split. apply (arr_assign S _ _ _ Shared A inds vs); eauto.
    split. apply (QSub_correct''' Q0 Shared A inds vs S S'0 X0 _) in H_Q; eauto.
    assert (C0 - (C3 ++
   C4 ++
   MSWrite M
   (conflicts (map (fun tn : nat * nat => (fst tn, VNat (snd tn))) inds))) ==
            C0 - C4 - C3 -
            MSWrite M (conflicts (map (fun tn : nat * nat => (fst tn, VNat (snd tn))) inds))).
    ring.
    rewrite H2.
    repeat apply Qplus_le_compat; try apply Qle_refl; apply Qopp_le_compat.
    apply (cexp_sound _ P0 C2) in H13; try assumption; try apply H_comp.
    apply (copd_sound _ P0 _ C1) in H10; try assumption; try apply H_comp.
    unfold conflicts_lessthan in H.
    apply mswrite_monotonic.
    eapply H.
    apply H_comp.
    apply H_comp.
    apply H_comp.
    apply H10.

  + (* QWeak *)
    assert (S;; T |={{P2; Q2; X2}}) as H_comp'.
    apply (compat_Q (Q1 <+ C)); try assumption.
    apply (compat_P_implies P1); try assumption.
    apply (compat_X _ _ _ X1); try assumption.
    destruct (Qgt_exists _ _ S X1 (C0 - C) H0) as [C1].
    apply Qplus_ap.
    assumption.
    destruct H5.
    apply (Qap_sub_X _ _ _ _ X2) in H6.
    destruct H6 as [C1'].
    destruct H6 as [H6 H6'].
    destruct (IHH_cstmt H_valid _ _ _ _ H_typedS H6 _ H_comp' H_T H_uniq H_eval) as [C'1].
    assert (Qgt (Q2' <+ C) (Q1' <+ C)).
    apply Qgt_plus_compat.
    assumption.
    destruct H7 as [H7 H7'].
    destruct H7' as [H7' H7''].
    apply (Qap_sub_X _ _ _ _ X1') in H7'.
    destruct H7' as [C'' H7'].
    destruct (Qgt_exists _ _ S' X1' (C'' ++ C) H8) as [C'''].
    apply Qplus_correct.
    apply H7'.
    exists C'''.
    split.
    apply (compat_Q Q2').
    apply (compat_P_implies P2'); try assumption.
    apply (compat_X _ _ _ X2').
    apply H7.
    assumption.
    split.
    apply H9.
    apply (Qle_trans _ (C'' ++ C)).
    apply H9.
    apply (Qle_trans _ (C'1 ++ C)).
    apply Qplus_le_compat; try apply Qle_refl.
    apply H7'.
    apply (Qle_trans _ (C1' - Cs ++ C)).
    apply Qplus_le_compat.
    apply H7''.
    apply Qle_refl.
    apply (Qle_trans _ (C1 - Cs ++ C)).
    repeat apply Qplus_le_compat; try apply Qle_refl.
    apply H6'.
    apply (Qle_trans _ (C0 - C - Cs ++ C)).
    repeat apply Qplus_le_compat; try apply Qle_refl.
    assumption.
    assert (C0 - C - Cs ++ C == C0 - Cs). ring.
    rewrite H10.
    apply Qle_refl.
    assumption.
    assumption.
Qed.
