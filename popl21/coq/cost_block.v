(*** Parallel cost semantics and soundness proof (Section 5 of submission) ***)

Require Import calc.
Require Import List.
Require Import cost.
Require Import eval.

Require Import QArith.

Open Scope nat_scope.

Definition warp := nat.
Definition parcost : Set := list (Cost * Cost).
Definition warp_of_thread (t: nat) := div t 32.

Fixpoint find_warp (w: nat) (t: threads) :=
  match t with
  | cons n t' => if Nat.eqb (warp_of_thread n) w then true else find_warp w t'
  | nil => false
  end.

Fixpoint seq_comp (n: nat) (t: threads) (C1: parcost) (C2: Cost * Cost) :=
  match C1 with
  | ws::t1 =>
    (if find_warp n t then ((fst ws) ++ (fst C2), (snd ws) ++ (snd C2)) else ws)
         ::(seq_comp (n + 1) t t1 C2)
  | _ => nil
  end.

Notation "P o+( T ) Q" := (seq_comp 0 T P Q) (at level 100).

Definition div_comp (TT TF: threads) (C1: parcost) (C2: Cost * Cost) :=
  seq_comp 0
           (List.map fst
                     (List.filter
                        (fun tt => Nat.eqb (warp_of_thread (fst tt))
                                           (warp_of_thread (snd tt)))
                        (List.list_prod TT TF)))
           C1 C2.

(* Paired cost metric for work and span *)
Inductive wsmetric :=
  { WSMVar    : Cost * Cost;
    WSMPar    : Cost * Cost;
    WSMConst  : Cost * Cost;
    WSMOp     : Cost * Cost;
    WSMGRead  : nat -> Cost * Cost;
    WSMSRead  : nat -> Cost * Cost;
    WSMIf     : Cost * Cost;
    WSMDiv    : Cost * Cost;
    WSMVWrite : Cost * Cost;
    WSMGWrite : nat -> Cost * Cost;
    WSMSWrite : nat -> Cost * Cost;
    WSMSync   : Cost * Cost }.

(** Cost semantics **)
Inductive par_eval_opd :
  wsmetric -> parcost -> I.State -> threads -> opd -> result -> parcost -> Prop :=
| POCRes : forall M C S T f,
    par_eval_opd M C S T (ORes (map (fun t => (t, f t)) T))
                 (map (fun t => (t, f t)) T) C
| POCVar : forall M C S T x,
    par_eval_opd M C S T (OVar x) (map (fun t => (t, S (I.ILVar x t))) T)
                 (C o+( T ) (M.(WSMVar)))
| POCPar : forall M C S T x,
    par_eval_opd M C S T (OPar x) (map (fun t => (t, S (I.IPar x))) T)
                 (C o+( T ) M.(WSMPar))
| POCConst : forall M C S T v,
    par_eval_opd M C S T (OConst v) (map (fun t => (t, v)) T)
                 (C o+( T ) M.(WSMConst))
| POCTid : forall M C S T,
    par_eval_opd M C S T OTid (map (fun t => (t, VNat t)) T) (C o+( T ) M.(WSMVar))
.


Hint Constructors par_eval_opd : cost_block.

Inductive par_eval_exp :
  wsmetric -> parcost -> I.State -> threads -> exp -> result -> parcost -> Prop :=
| PECRes : forall M C S T f,
    par_eval_exp M C S T (ERes (map (fun t => (t, f t)) T))
                 (map (fun t => (t, f t)) T) C
| PECOpd : forall M C0 S T o vs C,
    par_eval_opd M C0 S T o vs C -> par_eval_exp M C0 S T (EOpd o) vs C
| PECOp : forall M C0 S T o1 o2 vs1 vs2 C1 C2,
    par_eval_opd M C0 S T o1 vs1 C1 ->
    par_eval_opd M C1 S T o2 vs2 C2 ->
    par_eval_exp M C0 S T (EOp o1 o2) vs1 (C2 o+(T) M.(WSMOp))
| PECGRead : forall M C0 S T G o inds C,
    par_eval_opd M C0 S T o (map (fun tn => (fst tn, VNat (snd tn))) inds) C ->
    par_eval_exp M C0 S T (ERead Global G o)
             (map (fun ti => (fst ti, S (I.IArray Global G (snd ti)))) inds)
             (C o+(T)
                 (M.(WSMGRead)
                      (memreads (map (fun tn => (fst tn, VNat (snd tn))) inds))))
| PECSRead : forall M C0 S T A o inds C,
    par_eval_opd M C0 S T o (map (fun tn => (fst tn, VNat (snd tn))) inds) C ->
    par_eval_exp M C0 S T (ERead Shared A o)
             (map (fun ti => (fst ti, S (I.IArray Shared A (snd ti)))) inds)
             (C o+(T)
                 (M.(WSMSRead)
                      (conflicts (map (fun tn => (fst tn, VNat (snd tn))) inds))))
.

Hint Constructors par_eval_exp : cost_block.

Open Scope Q_scope.

Definition max (a b: Cost) : Cost :=
  if Qle_bool a b then b else a.

Fixpoint work (C: parcost) : Cost :=
  match C with
  | [] => 0
  | ws::C' =>
    fst ws + (work C')
  end.

Fixpoint span (C: parcost) : Cost :=
  match C with
  | [] => 0
  | ws::C' =>
    max (snd ws) (span C')
  end.


Inductive par_eval_stmt :
  wsmetric -> parcost -> I.State -> threads -> stmt -> I.State -> parcost -> Prop :=
| PSCNone : forall M C S s,
    par_eval_stmt M C S [] s S C
| PSCSkip : forall M C S T,
    T <> [] -> par_eval_stmt M C S T SSkip S C
| PSCVWrite : forall M C0 S T S' x e vs C ,
    T <> [] ->
    par_eval_exp M C0 S T e vs C ->
    assign S x vs S' ->
    par_eval_stmt M C0 S T (SVWrite x e) S' (C o+(T) M.(WSMVWrite))
| PSCGWrite : forall M C0 S S' T A e o inds C1 vs C2,
    T <> [] ->
    uniq_r inds ->
    T = map fst inds ->
    par_eval_opd M C0 S T o (map (fun tn => (fst tn , VNat (snd tn))) inds) C1 ->
    par_eval_exp M C1 S T e vs C2 ->
    Gassign S Global A inds vs S' ->
    par_eval_stmt M C0 S T (SAWrite Global A o e) S'
              (C2 o+(T)
                  (M.(WSMGWrite)
                       (memreads (map (fun tn => (fst tn , VNat (snd tn))) inds))))
| PSCSWrite : forall M C0 S S' T A e o inds C1 vs C2,
    T <> [] ->
    uniq_r inds ->
    T = map fst inds ->
    par_eval_opd M C0 S T o (map (fun tn => (fst tn , VNat (snd tn))) inds) C1 ->
    par_eval_exp M C1 S T e vs C2 ->
    Gassign S Shared A inds vs S' ->
    par_eval_stmt M C0 S T (SAWrite Shared A o e) S'
              (C2 o+(T)
                  (M.(WSMSWrite)
                       (conflicts (map (fun tn => (fst tn , VNat (snd tn))) inds))))
| PSCIf : forall M C0 S T R TT TF e s1 s2 C C1 C2 S1 S2,
    T <> [] ->
    par_eval_exp M C0 S T e R C ->
    TT = map (fun tv => fst tv) (filter is_true R) ->
    TF = map (fun tv => fst tv) (filter is_false R) ->
    par_eval_stmt M C S TT s1 S1 C1 ->
    par_eval_stmt M C1 S1 TF s2 S2 C2 ->
    par_eval_stmt M C0 S T (SIf e s1 s2) S2
                  (div_comp TT TF (C2 o+(T) M.(WSMIf)) M.(WSMDiv))
              (*
| SCWhile : forall M S T e s S' C',
    eval_stmt M S T (SIf e (SSeq s (SWhile e s)) SSkip) S' C' ->
    eval_stmt M S T (SWhile e s) S' C'
               *)
| PSCWhile : forall M C0 S T R TT TF e s S1 S2 C C1 C2,
    T <> [] ->
    par_eval_exp M C0 S T e R C ->
    TT = map (fun tv => fst tv) (filter is_true R) ->
    TF = map (fun tv => fst tv) (filter is_false R) ->
    par_eval_stmt M C S TT s S1 C1 ->
    par_eval_stmt M C1 S1 TT (SWhile e s) S2 C2 ->
    par_eval_stmt M C0 S T (SWhile e s) S2
                  (div_comp TT TF (C2 o+(T) M.(WSMIf)) M.(WSMDiv))
| PSCSeq : forall M C0 S T s1 s2 S1 C1 S2 C2,
    T <> [] ->
    par_eval_stmt M C0 S T s1 S1 C1 ->
    par_eval_stmt M C1 S1 T s2 S2 C2 ->
    par_eval_stmt M C0 S T (SSeq s1 s2) S2 C2
| PSCSync : forall M C S T,
    T <> [] ->
    par_eval_stmt M C S T SSync S
                  ((map (fun ws => (fst ws, span C)) C) o+(T) M.(WSMSync))
| PSCSwitchFree : forall M C S T f s S' C',
    T <> [] -> (filter f T) <> [] ->
    par_eval_stmt M C S (filter f T) s S' C' ->
    par_eval_stmt M C S T (SSwitch true (filter f T) s) S' C'
| PSCSwitch : forall M C S T f s S' C',
    T <> [] -> (filter f T) <> [] ->
    par_eval_stmt M C S (filter f T) s S' C' ->
    par_eval_stmt M C S T (SSwitch false (filter f T) s) S' (C' o+(T) M.(WSMDiv))
.

Definition ws (Mw Ms: resmetric) : wsmetric :=
  {| WSMVar    := (Mw.(MVar), Ms.(MVar));
     WSMPar    := (Mw.(MPar), Ms.(MPar));
     WSMConst  := (Mw.(MConst), Ms.(MConst));
     WSMOp     := (Mw.(MOp), Ms.(MOp));
     WSMGRead  := (fun n => (Mw.(MGRead) n, Ms.(MGRead) n));
     WSMSRead  := (fun n => (Mw.(MSRead) n, Ms.(MSRead) n));
     WSMIf     := (Mw.(MIf), Ms.(MIf));
     WSMDiv    := (Mw.(MDiv), Ms.(MDiv));
     WSMVWrite := (Mw.(MVWrite), Ms.(MVWrite));
     WSMGWrite := (fun n => (Mw.(MGWrite) n, Ms.(MGWrite) n));
     WSMSWrite := (fun n => (Mw.(MSWrite) n, Ms.(MSWrite) n));
     WSMSync   := (Mw.(MSync), Ms.(MSync));
  |}.

Hint Unfold ws : cost_block.

Definition of_nat n := inject_Z (Z.of_nat n). 

Require Import Psatz.
Require Import Znat.

Lemma of_nat_succ : forall n, of_nat (S n) == (of_nat n) + 1.
Proof.
  intros.
  unfold of_nat.
  assert ((S n = n + 1)%nat).
  lia.
  rewrite H.
  rewrite Nat2Z.inj_add.
  rewrite inject_Z_plus.
  assert (Z.of_nat 1 = 1%Z).
  unfold Z.of_nat.
  reflexivity.
  rewrite H0.
  unfold inject_Z.
  apply Qeq_refl.
Qed.
  
Lemma mult_length : forall (L: parcost) x n,
    n * (of_nat (length (x::L))) == n + n * (of_nat (length L)).
Proof.
  intros.
  unfold length.
  rewrite of_nat_succ.
  ring.
Qed.

Lemma Qplus_eq_compat: forall a b c d, a == b -> c == d -> a + c == b + d.
Proof.
  intros.
  lra.
Qed.  

Lemma work_plus: forall C T Mw Ms n (f: resmetric -> Cost),
    (forall x, f x >= 0) ->
    work (seq_comp n T C (f Mw, f Ms)) <=
    (work C) + (f Mw) * (of_nat (length C)).
Proof.
  intros.
  generalize dependent n.
  induction C; intro n; unfold ws; simpl.
  apply (Qle_trans _ (0 + 0) _); try rewrite Qplus_0_r; try rewrite Qplus_0_l.
  apply Qle_refl.
  assert (f Mw * of_nat 0 == 0). unfold of_nat. ring.
  rewrite H0. apply Qle_refl.

  unfold work.
  fold work.
  rewrite of_nat_succ.
  assert (fst a + work C + f Mw * (of_nat (length C) + 1) ==
          fst a + f Mw + (work C + f Mw * (of_nat (length C)))). ring.
  rewrite H0.
  apply Qplus_le_compat.
  destruct (find_warp n T).
  simpl.
  apply Qle_refl.
  assert ((fst a)%Q == fst a + 0). rewrite Qplus_0_r.
  apply Qeq_refl.
  rewrite H1 at 1.
  apply Qplus_le_compat; eauto with cost qarith.  
  apply IHC.
Qed.

Hint Resolve work_plus: cost_block.

Lemma work_plus_var:
  forall C T Mw Ms, work (C o+(T) (ws Mw Ms).(WSMVar))
                    <= (work C) + Mw.(MVar) * (of_nat (length C)).
Proof. intros. apply work_plus. eauto with cost. Qed.

Lemma work_plus_par:
  forall C T Mw Ms, work (C o+(T) (ws Mw Ms).(WSMPar))
                    <= (work C) + Mw.(MPar) * (of_nat (length C)).
Proof. intros. apply work_plus. eauto with cost. Qed.

Lemma work_plus_const:
  forall C T Mw Ms, work (C o+(T) (ws Mw Ms).(WSMConst))
                    <= (work C) + Mw.(MConst) * (of_nat (length C)).
Proof. intros. apply work_plus. eauto with cost. Qed.

Lemma work_plus_op:
  forall C T Mw Ms, work (C o+(T) (ws Mw Ms).(WSMOp))
                    <= (work C) + Mw.(MOp) * (of_nat (length C)).
Proof. intros. apply work_plus. eauto with cost. Qed.

Lemma work_plus_gread:
  forall C T Mw Ms n, work (C o+(T) ((ws Mw Ms).(WSMGRead) n))
                      <= (work C) + (Mw.(MGRead) n) * (of_nat (length C)).
Proof. intros. apply (work_plus _ _ _ _ _ (fun M => MGRead M n)).
       eauto with cost. Qed.

Lemma work_plus_sread:
  forall C T Mw Ms n, work (C o+(T) ((ws Mw Ms).(WSMSRead) n))
                      <= (work C) + (Mw.(MSRead) n) * (of_nat (length C)).
Proof. intros. apply (work_plus _ _ _ _ _ (fun M => MSRead M n)).
       eauto with cost. Qed.

Lemma work_plus_if:
  forall C T Mw Ms, work (C o+(T) (ws Mw Ms).(WSMIf))
                    <= (work C) + Mw.(MIf) * (of_nat (length C)).
Proof. intros. apply work_plus. eauto with cost. Qed.

Lemma work_plus_div:
  forall C TF TT Mw Ms, work (div_comp TF TT C ((ws Mw Ms).(WSMDiv)))
                    <= (work C) + Mw.(MDiv) * (of_nat (length C)).
Proof. intros. apply work_plus. eauto with cost. Qed.

Lemma work_plus_vwrite:
  forall C T Mw Ms, work (C o+(T) (ws Mw Ms).(WSMVWrite))
                    <= (work C) + Mw.(MVWrite) * (of_nat (length C)).
Proof. intros. apply work_plus. eauto with cost. Qed.

Lemma work_plus_gwrite:
  forall C T Mw Ms n, work (C o+(T) ((ws Mw Ms).(WSMGWrite) n))
                      <= (work C) + (Mw.(MGWrite) n) * (of_nat (length C)).
Proof. intros. apply (work_plus _ _ _ _ _ (fun M => MGWrite M n)).
       eauto with cost. Qed.

Lemma work_plus_swrite:
  forall C T Mw Ms n, work (C o+(T) ((ws Mw Ms).(WSMSWrite) n))
                      <= (work C) + (Mw.(MSWrite) n) * (of_nat (length C)).
Proof. intros. apply (work_plus _ _ _ _ _ (fun M => MSWrite M n)).
       eauto with cost. Qed.

Lemma work_plus_sync:
  forall C T Mw Ms, work (C o+(T) (ws Mw Ms).(WSMSync))
                    <= (work C) + Mw.(MSync) * (of_nat (length C)).
Proof. intros. apply work_plus. eauto with cost. Qed.

Hint Resolve work_plus_var : cost_block.
Hint Resolve work_plus_par : cost_block.
Hint Resolve work_plus_const : cost_block.
Hint Resolve work_plus_op : cost_block.
Hint Resolve work_plus_gread : cost_block.
Hint Resolve work_plus_sread : cost_block.
Hint Resolve work_plus_if : cost_block.
Hint Resolve work_plus_div : cost_block.
Hint Resolve work_plus_vwrite : cost_block.
Hint Resolve work_plus_gwrite : cost_block.
Hint Resolve work_plus_swrite : cost_block.
Hint Resolve work_plus_sync : cost_block.

Lemma seq_comp_empty:
  forall C Cp n, seq_comp n [] C Cp = C.
Proof.
  intros.
  generalize dependent n.
  induction C; intro n.
  reflexivity.
  unfold seq_comp.
  unfold find_warp.
  unfold seq_comp in IHC.
  rewrite IHC.
  reflexivity.
Qed.

Lemma filter_nil: forall {A} (f: A -> bool), filter f [] = []. 
  intros.
  unfold filter.
  reflexivity.
Qed.

Lemma plus_div_true: forall C T Mw Ms,
    div_comp T [] C (ws Mw Ms).(WSMDiv) = C.
Proof.
  intros.
  unfold div_comp.
  assert (map fst
           (filter
              (fun tt : nat * nat =>
               warp_of_thread (fst tt) =? warp_of_thread (snd tt))
              (list_prod T [])) = []).
  apply length_zero_iff_nil.
  rewrite map_length.
  assert (list_prod T ([] : threads) = []).
  apply length_zero_iff_nil.
  rewrite prod_length.
  simpl.
  apply mul_0_r.
  rewrite H.
  rewrite filter_nil.
  reflexivity.
  rewrite H.
  unfold seq_comp.
  rewrite seq_comp_empty.
  reflexivity.
Qed.

Lemma plus_div_false: forall C T Mw Ms,
    div_comp [] T C (ws Mw Ms).(WSMDiv) = C.
Proof.
  intros.
  unfold div_comp.
  assert (map fst
           (filter
              (fun tt : nat * nat =>
               warp_of_thread (fst tt) =? warp_of_thread (snd tt))
              (list_prod [] T)) = []).
  apply length_zero_iff_nil.
  rewrite map_length.
  assert (list_prod ([] : threads) T = []).
  apply length_zero_iff_nil.
  rewrite prod_length.
  simpl.
  reflexivity.
  rewrite H.
  rewrite filter_nil.
  reflexivity.
  rewrite H.
  unfold seq_comp.
  rewrite seq_comp_empty.
  reflexivity.
Qed.

Hint Rewrite plus_div_true : step.
Hint Rewrite plus_div_false : step.
  
Lemma work_plus_div_true:
  forall C T Mw Ms, work (div_comp T [] C (ws Mw Ms).(WSMDiv)) == work C.
Proof.
  intros.
  rewrite plus_div_true.
  apply Qeq_refl.
Qed.

Lemma work_plus_div_false:
  forall C T Mw Ms, work (div_comp [] T C (ws Mw Ms).(WSMDiv)) == work C.
Proof.
  intros.
  rewrite plus_div_false.
  apply Qeq_refl.
Qed.

Hint Resolve work_plus_div_true : cost_block.
Hint Resolve work_plus_div_false : cost_block.
  
Lemma Qle_plus: forall a b, b >= 0 -> a <= a + b.
Proof.
  intros.
  apply (Qle_trans _ (a + 0)).
  rewrite Qplus_0_r.
  apply Qle_refl.
  apply Qplus_le_compat.
  apply Qle_refl.
  assumption.  
Qed.

Hint Resolve Qle_plus : cost_block.

Lemma max_plus: forall a b c, c >= 0 -> max (a + c) (b + c) == (max a b) + c.
Proof.
  intros.
  unfold max.
  remember (Qle_bool (a + c) (b + c)) as b0.
  remember (Qle_bool a b) as b1.
  case b0, b1; eauto with qarith; eauto with qarith cost_block.
  apply sym_eq in Heqb0. apply Qle_bool_imp_le in Heqb0.
  assert (~(a <= b)).
  intro.
  apply Qle_bool_iff in H0.
  rewrite H0 in Heqb1.
  discriminate.
  destruct H0.
  assert (a == (a + c) - c). ring.
  assert (b == (b + c) - c). ring.
  rewrite H0. rewrite H1.
  apply Qplus_le_compat.
  assumption.
  apply Qle_refl.

  apply sym_eq in Heqb1. apply Qle_bool_imp_le in Heqb1.
  assert (~(a + c <= b + c)).
  intro.
  apply Qle_bool_iff in H0.
  rewrite H0 in Heqb0.
  discriminate.
  destruct H0.
  apply Qplus_le_compat; eauto with qarith.
Qed.

Lemma max_plus2: forall a b c, c >= 0 -> max a (b + c) <= (max a b) + c.
Proof.
  intros.
  unfold max.
  remember (Qle_bool a (b + c)) as b0.
  remember (Qle_bool a b) as b1.
  case b0, b1; eauto with qarith; eauto with qarith cost_block.
  apply sym_eq in Heqb0. apply Qle_bool_imp_le in Heqb0.
  assert (~(a <= b)).
  intro.
  apply Qle_bool_iff in H0.
  rewrite H0 in Heqb1.
  discriminate.
  apply Qnot_le_lt in H0.
  apply Qlt_le_weak in H0.
  apply Qplus_le_compat.
  assumption.
  apply Qle_refl.

  apply sym_eq in Heqb1. apply Qle_bool_imp_le in Heqb1.
  apply (Qle_trans _ (a + 0)).
  rewrite Qplus_0_r.
  apply Qle_refl.
  apply Qplus_le_compat; assumption.
Qed.  

Lemma max_le: forall a b c, b <= c -> max a b <= max a c.
Proof.
  intros.
  unfold max.
  remember (Qle_bool a b) as b0.
  remember (Qle_bool a c) as b1.
  case b0, b1; eauto with qarith.
  assert (~(a <= c)).
  intro.
  apply Qle_bool_iff in H0.
  rewrite H0 in Heqb1.
  discriminate.
  apply Qnot_le_lt in H0.
  apply Qlt_le_weak in H0.
  apply (Qle_trans _ c); eauto.
  apply Qle_bool_iff.
  apply sym_eq.
  assumption.
Qed.

Lemma max_le2: forall a b c d, a <= c -> b <= d -> max a b <= max c d.
Proof.
  intros.
  unfold max.
  remember (Qle_bool a b) as b0.
  remember (Qle_bool c d) as b1.
  case b0, b1; eauto with qarith.
  assert (~(c <= d)).
  intro.
  apply Qle_bool_iff in H1.
  rewrite H1 in Heqb1.
  discriminate.
  apply Qnot_le_lt in H1.
  apply Qlt_le_weak in H1.
  apply (Qle_trans _ d); eauto.
  apply sym_eq in Heqb1.
  apply Qle_bool_iff in Heqb1.
  apply (Qle_trans _ c); trivial.
Qed.
  
Lemma span_plus: forall C T Mw Ms n (f: resmetric -> Cost),
    (forall x, f x >= 0) ->
    span (seq_comp n T C (f Mw, f Ms)) <= (span C) + (f Ms).
Proof.
  intros.
  generalize dependent n.
  induction C; intro n; unfold ws; simpl.
  apply (Qle_trans _ (0 + 0) _); try rewrite Qplus_0_r; try rewrite Qplus_0_l.
  apply Qle_refl.
  eauto.

  unfold span.
  fold span.
  destruct (find_warp n T).
  simpl.
  apply (Qle_trans _ (max (snd a + f Ms) (span C + f Ms))).
  apply max_le.
  apply IHC.
  rewrite max_plus.
  apply Qle_refl.
  apply H.

  apply (Qle_trans _ (max (snd a) (span C + f Ms))).
  apply max_le.
  apply IHC.
  apply max_plus2.
  apply H.
Qed.

Hint Resolve span_plus: cost_block.

Lemma span_plus_var:
  forall C T Mw Ms, span (C o+(T) (ws Mw Ms).(WSMVar))
                    <= (span C) + Ms.(MVar).
Proof. intros. apply span_plus. eauto with cost. Qed.

Lemma span_plus_par:
  forall C T Mw Ms, span (C o+(T) (ws Mw Ms).(WSMPar))
                    <= (span C) + Ms.(MPar).
Proof. intros. apply span_plus. eauto with cost. Qed.

Lemma span_plus_const:
  forall C T Mw Ms, span (C o+(T) (ws Mw Ms).(WSMConst))
                    <= (span C) + Ms.(MConst).
Proof. intros. apply span_plus. eauto with cost. Qed.

Lemma span_plus_op:
  forall C T Mw Ms, span (C o+(T) (ws Mw Ms).(WSMOp))
                    <= (span C) + Ms.(MOp).
Proof. intros. apply span_plus. eauto with cost. Qed.

Lemma span_plus_gread:
  forall C T Mw Ms n, span (C o+(T) ((ws Mw Ms).(WSMGRead) n))
                      <= (span C) + (Ms.(MGRead) n).
Proof. intros. apply (span_plus _ _ _ _ _ (fun M => MGRead M n)).
       eauto with cost. Qed.

Lemma span_plus_sread:
  forall C T Mw Ms n, span (C o+(T) ((ws Mw Ms).(WSMSRead) n))
                      <= (span C) + (Ms.(MSRead) n).
Proof. intros. apply (span_plus _ _ _ _ _ (fun M => MSRead M n)).
       eauto with cost. Qed.

Lemma span_plus_if:
  forall C T Mw Ms, span (C o+(T) (ws Mw Ms).(WSMIf))
                    <= (span C) + Ms.(MIf).
Proof. intros. apply span_plus. eauto with cost. Qed.

Lemma span_plus_div:
  forall C T Mw Ms, span (C o+(T) (ws Mw Ms).(WSMDiv))
                    <= (span C) + Ms.(MDiv).
Proof. intros. apply span_plus. eauto with cost. Qed.

Lemma span_plus_vwrite:
  forall C T Mw Ms, span (C o+(T) (ws Mw Ms).(WSMVWrite))
                    <= (span C) + Ms.(MVWrite).
Proof. intros. apply span_plus. eauto with cost. Qed.

Lemma span_plus_gwrite:
  forall C T Mw Ms n, span (C o+(T) ((ws Mw Ms).(WSMGWrite) n))
                      <= (span C) + (Ms.(MGWrite) n).
Proof. intros. apply (span_plus _ _ _ _ _ (fun M => MGWrite M n)).
       eauto with cost. Qed.

Lemma span_plus_swrite:
  forall C T Mw Ms n, span (C o+(T) ((ws Mw Ms).(WSMSWrite) n))
                      <= (span C) + (Ms.(MSWrite) n).
Proof. intros. apply (span_plus _ _ _ _ _ (fun M => MSWrite M n)).
       eauto with cost. Qed.

Lemma span_plus_sync:
  forall C T Mw Ms, span (C o+(T) (ws Mw Ms).(WSMSync))
                    <= (span C) + Ms.(MSync).
Proof. intros. apply span_plus. eauto with cost. Qed.

Hint Resolve span_plus_var : cost_block.
Hint Resolve span_plus_par : cost_block.
Hint Resolve span_plus_const : cost_block.
Hint Resolve span_plus_op : cost_block.
Hint Resolve span_plus_gread : cost_block.
Hint Resolve span_plus_sread : cost_block.
Hint Resolve span_plus_if : cost_block.
Hint Resolve span_plus_div : cost_block.
Hint Resolve span_plus_vwrite : cost_block.
Hint Resolve span_plus_gwrite : cost_block.
Hint Resolve span_plus_swrite : cost_block.
Hint Resolve span_plus_sync : cost_block.

Lemma span_plus_div_true:
  forall C T Mw Ms, span (div_comp T [] C (ws Mw Ms).(WSMDiv)) == span C.
Proof.                    
  intros.
  rewrite plus_div_true.
  apply Qeq_refl.
Qed.

Lemma span_plus_div_false:
  forall C T Mw Ms, span (div_comp [] T C (ws Mw Ms).(WSMDiv)) == span C.
Proof.                    
  intros.
  rewrite plus_div_false.
  apply Qeq_refl.
Qed.

Hint Resolve span_plus_div_true : cost_block.
Hint Resolve span_plus_div_false : cost_block.

Lemma comp_len_eq : forall n T C1 C2,
    length (seq_comp n T C1 C2) = length C1.
Proof.
  intros.
  generalize dependent n.
  induction C1; intro; unfold seq_comp.
  eauto.
  unfold length.
  apply f_equal.
  apply IHC1.
Qed.

Hint Rewrite comp_len_eq : cost_block.
Hint Resolve comp_len_eq : cost_block.
  
Lemma warps_eq_opd : forall M C S T o R C',
    par_eval_opd M C S T o R C' -> length C = length C'.
Proof.
  intros.
  induction H; eauto with cost_block.
Qed.

Hint Resolve warps_eq_opd : cost_block.
Hint Rewrite warps_eq_opd : cost_block.

Lemma warps_eq_exp : forall M C S T e R C',
    par_eval_exp M C S T e R C' -> length C = length C'.
Proof.
  intros.
  induction H; eauto with cost_block;
    try apply warps_eq_opd in H; try apply warps_eq_opd in H0;
      rewrite comp_len_eq; try lia.
Qed.

Hint Resolve warps_eq_exp : cost_block.
Hint Rewrite warps_eq_exp : cost_block.

Lemma warps_eq_stmt : forall M C S T s R C',
    par_eval_stmt M C S T s R C' -> length C = length C'.
Proof.
  intros.
  induction H; try unfold div_comp;
    try apply warps_eq_opd in H0; try apply warps_eq_opd in H1;
      try apply warps_eq_opd in H2; try apply warps_eq_exp in H3;
        try apply warps_eq_exp in H0; try apply warps_eq_exp in H1;
          try apply warps_eq_exp in H4;
          try rewrite comp_len_eq; try lia; try rewrite comp_len_eq; try lia.

  rewrite map_length.
  reflexivity.
Qed.

Hint Resolve warps_eq_stmt : cost_block.
Hint Rewrite warps_eq_stmt : cost_block.

Hint Resolve warps_eq_stmt : lengths.
Hint Resolve warps_eq_exp : lengths.
Hint Resolve warps_eq_opd : lengths.

Lemma le_rew: forall a b c d, a <= d -> d + b <= c -> a + b <= c.
Proof.
  intros.
  apply (Qle_trans _ (d + b)).
  apply Qplus_le_compat.
  assumption.
  apply Qle_refl.
  assumption.
Qed.

Lemma Qeq_le_weak: forall x y, x == y -> x <= y.
Proof.
  intros.
  lra.
Qed.

Hint Constructors par_eval_stmt : cost_block.

Axiom parcosts_eq : forall (C C': parcost) a b a' b',
    C = C' -> a == a' -> b == b' ->
    (a, b)::C = (a', b')::C'.

Lemma plus_zero: forall C T n, seq_comp n T C (0, 0) = C.
Proof.
  intros.
  generalize dependent n.
  induction C.
  simpl.
  reflexivity.
  intro n.
  unfold seq_comp.
  fold seq_comp.
  case (find_warp n T).
  simpl.
  rewrite IHC.
  rewrite (surjective_pairing a) at 3.
  apply parcosts_eq; eauto with cost_block; lra.
  rewrite IHC.
  reflexivity.
Qed.  

Hint Rewrite plus_zero : cost_block.

Tactic Notation "eqlengths" constr(C1) constr(C2) ident(H) :=
  assert (length C1 = length C2) as H;
  eauto with lengths;
  apply (f_equal of_nat) in H;
  assert (of_nat (length C1) == of_nat (length C2));
  rewrite H; try apply Qeq_refl.


Lemma span_pos: forall C, 0 <= span C.
Proof.
  intros.
  induction C.
  apply Qle_refl.
  unfold span.
  fold span.
  unfold max.
  remember (Qle_bool (snd a) (span C)) as b.
  destruct b.
  assumption.
  apply (Qle_trans _ (span C)).
  assumption.
  apply Qlt_le_weak.
  apply Qnot_le_lt.
  intro.
  apply Qle_bool_iff in H.
  rewrite H in Heqb.
  discriminate.
Qed.

(** Lemma 2 -- broken into operands and expressions, and separate
 ** lemmas for work and span **)

Lemma eval_opd_work_equiv : forall Mw Ms C S T o S' C',
  par_eval_opd (ws Mw Ms) C S T o S' C' ->
  exists Cw, eval_opd Mw S T o S' Cw /\
             work C' <= work C + Cw * (of_nat (length C)).
Proof.
  intros.
  remember (ws Mw Ms) as M.
  induction H; subst M; eexists; split; eauto with eval cost cost_block.
  lra.
Qed.

Lemma eval_opd_span_equiv : forall Mw Ms C S T o S' C',
  par_eval_opd (ws Mw Ms) C S T o S' C' ->
  exists Cs, eval_opd Ms S T o S' Cs /\
             span C' <= span C + Cs.
Proof.
  intros.
  remember (ws Mw Ms) as M.
  induction H; subst M; eexists; split; eauto with eval cost cost_block.
  lra.
Qed.

Hint Resolve eval_opd_work_equiv : cost_block.
Hint Resolve eval_opd_span_equiv : cost_block.

Lemma eval_exp_work_equiv : forall Mw Ms C S T e S' C',
  par_eval_exp (ws Mw Ms) C S T e S' C' ->
  exists Cw, eval_exp Mw S T e S' Cw /\
             work C' <= work C + Cw * (of_nat (length C)).
Proof.
  intros.
  remember (ws Mw Ms) as M.
  induction H; subst M.

  - (* Res *)
    exists 0.
    split; eauto with eval cost cost_block.
    lra.
    
  - (* Opd *)
    apply eval_opd_work_equiv in H; do 2 destruct H.
    eexists; split; eauto with eval cost cost_block.

  - (* Op *)
    eqlengths C0 C1 HL0.
    eqlengths C1 C2 HL1.
    apply eval_opd_work_equiv in H; do 2 destruct H.
    apply eval_opd_work_equiv in H0; do 2 destruct H0.
    eexists. split. eauto with eval cost cost_block.
    apply (Qle_trans _ (work C2 + Mw.(MOp) * of_nat (length C2))).
    eauto with cost_block.
    apply (le_rew _ _ _ _ H4).
    rewrite <- Qplus_assoc.
    apply (le_rew _ _ _ _ H3).
    psatz Q 2.

  - (* GRead *)
    eqlengths C0 C HL0.
    apply eval_opd_work_equiv in H; do 2 destruct H.
    eexists. split. eauto with eval cost cost_block.
    apply (Qle_trans _ (work C + Mw.(MGRead)
                                      (memreads
                                         (map (fun tn : nat * nat => (fst tn, VNat (snd tn))) inds)) * of_nat (length C))).
    eauto with cost_block.
    apply (le_rew _ _ _ _ H1).
    psatz Q 2.

  - (* SRead *)
    eqlengths C0 C HL0.
    apply eval_opd_work_equiv in H; do 2 destruct H.
    eexists. split. eauto with eval cost cost_block.
    apply (Qle_trans _ (work C + Mw.(MSRead)
                                      (conflicts
                                         (map (fun tn : nat * nat => (fst tn, VNat (snd tn))) inds)) * of_nat (length C))).
    eauto with cost_block.
    apply (le_rew _ _ _ _ H1).
    psatz Q 2.    
Qed.

Lemma eval_exp_span_equiv : forall Mw Ms C S T e S' C',
  par_eval_exp (ws Mw Ms) C S T e S' C' ->
  exists Cs, eval_exp Ms S T e S' Cs /\
             span C' <= span C + Cs.
Proof.
  intros.
  remember (ws Mw Ms) as M.
  induction H; subst M.

  - (* Res *)
    exists 0.
    split.
    eauto with eval cost cost_block.
    lra.

  - (* Opd *)
    apply eval_opd_span_equiv in H; do 2 destruct H.
    eexists; split; eauto with eval cost cost_block.

  - (* Op *)
    apply eval_opd_span_equiv in H; do 2 destruct H.
    apply eval_opd_span_equiv in H0; do 2 destruct H0.
    eexists. split. eauto with eval cost cost_block.
    apply (Qle_trans _ (span C2 + Ms.(MOp))).
    eauto with cost_block.
    apply (le_rew _ _ _ _ H2).
    rewrite <- Qplus_assoc.
    apply (le_rew _ _ _ _ H1).
    psatz Q 2.

  - (* GRead *)
    apply eval_opd_span_equiv in H; do 2 destruct H.
    eexists. split. eauto with eval cost cost_block.
    apply (Qle_trans _ (span C + Ms.(MGRead)
                                      (memreads
                                         (map (fun tn : nat * nat => (fst tn, VNat (snd tn))) inds)))).
    eauto with cost_block.
    apply (le_rew _ _ _ _ H0).
    psatz Q 2.

  - (* SRead *)
    apply eval_opd_span_equiv in H; do 2 destruct H.
    eexists. split. eauto with eval cost cost_block.
    apply (Qle_trans _ (span C + Ms.(MSRead)
                                      (conflicts
                                         (map (fun tn : nat * nat => (fst tn, VNat (snd tn))) inds)))).
    eauto with cost_block.
    apply (le_rew _ _ _ _ H0).
    psatz Q 2.    
Qed.

Require logic.

Lemma R_eq_exp: forall M S T e R C,
    eval_exp M S T e R C ->
    exists f, R = map (fun t => (t, f t)) T.
Proof.
  intros.
  induction H; eauto with infer.

  apply logic.R_eq_opd in H.
  destruct H.
  exists (fun t => S (I.IArray Global G (match x t with
                                         | VNat i => i
                                         | _ => 0
                                         end))).
  apply (f_equal (map (fun tn => (fst tn,
                                  S (I.IArray Global G
                                              (match snd tn with
                                               | VNat i => i
                                               | _ => 0
                                               end)))))) in H.
  rewrite map_map in H.
  simpl in H.
  rewrite H.
  rewrite map_map.
  simpl.
  reflexivity.

  apply logic.R_eq_opd in H.
  destruct H.
  exists (fun t => S (I.IArray Shared A (match x t with
                                         | VNat i => i
                                         | _ => 0
                                         end))).
  apply (f_equal (map (fun tn => (fst tn,
                                  S (I.IArray Shared A
                                              (match snd tn with
                                               | VNat i => i
                                               | _ => 0
                                               end)))))) in H.
  rewrite map_map in H.
  simpl in H.
  rewrite H.
  rewrite map_map.
  simpl.
  reflexivity.
Qed.  

Lemma map_filter_map_true : forall T,
    T =
    map (fun tv => fst tv) (filter is_true (map (fun n => (n, VBool true)) T)).
Proof.
  intros.
  induction T.
  reflexivity.
  unfold map.
  unfold filter.
  unfold is_true.
  simpl.
  apply f_equal.
  assumption.
Qed.

Lemma map_filter_map_false : forall T,
    T =
    map (fun tv => fst tv) (filter is_false (map (fun n => (n, VBool false)) T)).
Proof.
  intros.
  induction T.
  reflexivity.
  unfold map.
  unfold filter.
  unfold is_false.
  simpl.
  apply f_equal.
  assumption.
Qed.

Lemma map_filter_map_emp_true : forall T,
    [] =
    map (fun tv => fst tv) (filter is_true (map (fun n => (n, VBool false)) T)).
Proof.
  intros.
  induction T.
  reflexivity.
  unfold map.
  unfold filter.
  unfold is_true.
  simpl.
  assumption.
Qed.

Lemma map_filter_map_emp_false : forall T,
    [] =
    map (fun tv => fst tv) (filter is_false (map (fun n => (n, VBool true)) T)).
Proof.
  intros.
  induction T.
  reflexivity.
  unfold map.
  unfold filter.
  unfold is_false.
  simpl.
  assumption.
Qed.

Hint Resolve map_filter_map_true : cost_block.
Hint Resolve map_filter_map_false : cost_block.
Hint Resolve map_filter_map_emp_true : cost_block.
Hint Resolve map_filter_map_emp_false : cost_block.

Lemma rem_false: forall Sig M S T e R C,
  typed_exp Sig e TBool ->
  typed_state Sig S ->
  eval_exp M S T e R C ->
  [] = map (fun tv => fst tv) (filter is_true R) ->
  T = map (fun tv => fst tv) (filter is_false R) /\
  R = map (fun t : nat => (t, VBool false)) T.
Proof.
  intros.
  assert (typed_result R TBool).
  apply (pres_exp M Sig S T e _ _ C); eauto.
  apply R_eq_exp in H1.
  destruct H1.
  assert (R = map (fun t : nat => (t, VBool false)) T).
  generalize dependent R.
  induction T; intros.
  unfold map in H1.
  subst R.
  reflexivity.  
  unfold map.
  unfold map in H1.
  unfold typed_result in H3.
  subst R.
  set (HT := H3 a (x a) (in_eq (a, x a) _)).
  inversion HT.
  destruct b.
  rewrite <- H4 in H2.
  unfold filter in H2.
  simpl in H2.
  discriminate.
  unfold map in IHT.
  rewrite (IHT ((fix map (l : list nat) : list (nat * value) :=
           match l with
           | [] => []
           | a :: t => (a, x a) :: map t
           end) T)).
  reflexivity.
  reflexivity.
  rewrite <- H4 in H2.
  unfold filter in H2.
  unfold is_true in H2.
  assumption.
  unfold typed_result.
  intros.
  apply (H3 th v).
  apply in_cons.
  assumption.

  split.
  rewrite H4.
  apply map_filter_map_false.
  assumption.
Qed.


Lemma rem_true: forall Sig M S T e R C,
  typed_exp Sig e TBool ->
  typed_state Sig S ->
  eval_exp M S T e R C ->
  [] = map (fun tv => fst tv) (filter is_false R) ->
  T = map (fun tv => fst tv) (filter is_true R) /\
  R = map (fun t : nat => (t, VBool true)) T.
Proof.
  intros.
  assert (typed_result R TBool).
  apply (pres_exp M Sig S T e _ _ C); eauto.
  apply R_eq_exp in H1.
  destruct H1.
  assert (R = map (fun t : nat => (t, VBool true)) T).
  generalize dependent R.
  induction T; intros.
  unfold map in H1.
  subst R.
  reflexivity.  
  unfold map.
  unfold map in H1.
  unfold typed_result in H3.
  subst R.
  set (HT := H3 a (x a) (in_eq (a, x a) _)).
  inversion HT.
  destruct b.
  unfold map in IHT.
  rewrite (IHT ((fix map (l : list nat) : list (nat * value) :=
           match l with
           | [] => []
           | a :: t => (a, x a) :: map t
           end) T)).
  reflexivity.
  reflexivity.
  rewrite <- H4 in H2.
  unfold filter in H2.
  unfold is_false in H2.
  assumption.
  unfold typed_result.
  intros.
  apply (H3 th v).
  apply in_cons.
  assumption.  
  rewrite <- H4 in H2.
  unfold filter in H2.
  simpl in H2.
  discriminate.

  split.
  rewrite H4.
  apply map_filter_map_true.
  assumption.
Qed.

Lemma lengths_TF_TT: forall R,
    (forall x, In x R -> snd x = VBool true \/ snd x = VBool false) ->
    (length (filter is_true R) + length (filter is_false R) = length R)%nat.
Proof.
  intros.
  induction R; intros.
  reflexivity.

  destruct (H a (in_eq _ _)).
  unfold is_true.
  unfold is_false.
  unfold filter.
  rewrite H0.
  simpl.
  apply f_equal.
  apply IHR.
  intros.  
  apply H.
  apply in_cons.
  assumption.

  rewrite add_comm.
  unfold is_true.
  unfold is_false.
  unfold filter.
  rewrite H0.
  simpl.
  apply f_equal.
  rewrite add_comm.
  apply IHR.
  intros.  
  apply H.
  apply in_cons.
  assumption.
Qed.

Lemma unequal_lengths: forall {A} (L1 L2: list A),
    length L1 <> length L2 -> L1 <> L2.
Proof.
  intros.
  intro.
  apply H.
  apply f_equal.
  assumption.
Qed.  
  
Lemma TF_TT_not_T: forall M S T e R C TT TF,
    typed_result R TBool ->
    eval_exp M S T e R C ->
    TT = map (fun tv => fst tv) (filter is_true R) ->
    TF = map (fun tv => fst tv) (filter is_false R) ->
    TF <> [] ->
    TT <> T.
Proof.
  intros.
  apply unequal_lengths.
  assert (length TT + length TF = length R)%nat.
  rewrite H1. rewrite H2.
  rewrite map_length.
  rewrite map_length.
  apply lengths_TF_TT.
  intros.
  unfold typed_result in H.
  rewrite (surjective_pairing x) in H4.
  apply H in H4.
  inversion H4.
  case b.
  left. reflexivity.
  right. reflexivity.
  intro.
  rewrite H5 in H4.
  apply H3.
  apply length_zero_iff_nil.
  apply R_eq_exp in H0.
  destruct H0.
  subst R.
  rewrite map_length in H4.
  lia.
Qed.

(** Lemma 3 - broken into lemmas for work and span **)
Lemma eval_stmt_work_equiv : forall Sig Mw Ms C S T s S' C',
    typed_state Sig S ->
    valid Sig s ->
    T <> [] ->
    par_eval_stmt (ws Mw Ms) C S T s S' C' ->
    exists Cw, eval_stmt Mw S T s S' Cw /\
               work C' <= work C + Cw * (of_nat (length C)).
Proof.
  intros Sig Mw Ms C S T s S' C' H_typed H_valid. intros.
  generalize dependent S.
  remember (ws Mw Ms) as M.
  induction 2; intros; subst M.

  - contradiction.

  - (* Skip *)
    eexists. split; eauto with cost eval cost_block.
    lra.

  - (* VWrite *)
    eqlengths C0 C HL0.
    apply eval_exp_work_equiv in H1. do 2 destruct H1.
    eexists. split. eauto with eval.
    apply (Qle_trans _ (work C + Mw.(MVWrite) * of_nat (length C))).
    eauto with cost_block.
    apply (le_rew _ _ _ _ H4).
    psatz Q 2.

  - (* GWrite *)
    eqlengths C0 C1 HL0.
    eqlengths C1 C2 HL1.
    apply eval_opd_work_equiv in H3. do 2 destruct H3.
    apply eval_exp_work_equiv in H4. do 2 destruct H4.
    eexists. split.
    apply (SCGWrite _ _ _ _ _ _ _ _ _ _ _ H1 H2 H3 H4).
    assumption.
    apply (Qle_trans _ (work C2 + Mw.(MGWrite)
                                       (memreads
                                          (map (fun tn : nat * nat => (fst tn, VNat (snd tn))) inds))
           * of_nat (length C2))).
    eauto with cost_block.
    apply (le_rew _ _ _ _ H9).
    rewrite <- Qplus_assoc.
    apply (le_rew _ _ _ _ H8).
    psatz Q 2.

  - (* SWrite *)
    eqlengths C0 C1 HL0.
    eqlengths C1 C2 HL1.
    apply eval_opd_work_equiv in H3. do 2 destruct H3.
    apply eval_exp_work_equiv in H4. do 2 destruct H4.
    eexists. split.
    apply (SCSWrite _ _ _ _ _ _ _ _ _ _ _ H1 H2 H3 H4).
    assumption.
    apply (Qle_trans _ (work C2 + Mw.(MSWrite)
                                       (conflicts
                                          (map (fun tn : nat * nat => (fst tn, VNat (snd tn))) inds))
           * of_nat (length C2))).
    eauto with cost_block.
    apply (le_rew _ _ _ _ H9).
    rewrite <- Qplus_assoc.
    apply (le_rew _ _ _ _ H8).
    psatz Q 2.

  - (* If *)
    eqlengths C0 C HL0.
    eqlengths C C1 HL1.
    eqlengths C1 C2 HL2.
    rename H4 into HL0'.
    rename H5 into HL1'.
    rename H6 into HL2'.
    apply eval_exp_work_equiv in H1. do 2 destruct H1.
    inversion H_valid.
    set (H_T := logic.TF_TT_T _ _ _ _ _ _ _ _ _ H9 H_typed H1 H2 H3).
    case TT, TF.
    + (* [], [] *)
      destruct H.
      induction T.
      reflexivity.
      destruct (H_T a).
      destruct (H12 (in_eq a T)).
      apply in_nil in H13. contradiction.
      apply in_nil in H13. contradiction.
    + (* [], T *)
      inversion H0_; try contradiction.
      subst S1. subst C1.
      destruct (IHpar_eval_stmt2); eauto.
      discriminate.      
      destruct H15.
      destruct (rem_false Sig Mw S T e R x); eauto.
      rewrite <- H3 in H18.
      eexists. split.
      rewrite <- H18 in H15.
      subst R.
      apply (SCIfF); eauto with eval cost cost_block.
      rewrite work_plus_div_false.
      apply (Qle_trans _ (work C2 + Mw.(MIf) * of_nat (length C2))).
      eauto with cost_block.
      apply (le_rew _ _ _ _ H17).
      rewrite <- Qplus_assoc.
      apply (le_rew _ _ _ _ H4).
      psatz Q 2.
    + (* T, [] *)
      inversion H0_0; try contradiction.
      subst S2. subst C2.
      destruct (IHpar_eval_stmt1); eauto.
      discriminate.      
      destruct H15.
      destruct (rem_true Sig Mw S T e R x); eauto.
      rewrite <- H2 in H18.
      eexists. split.
      rewrite <- H18 in H15.
      subst R.
      apply (SCIfT); eauto with eval cost cost_block.
      rewrite work_plus_div_true.
      apply (Qle_trans _ (work C1 + Mw.(MIf) * of_nat (length C1))).
      eauto with cost_block.
      apply (le_rew _ _ _ _ H17).
      rewrite <- Qplus_assoc.
      apply (le_rew _ _ _ _ H4).
      psatz Q 2.
    + (* TT, TF *)      
      destruct (IHpar_eval_stmt1); eauto. discriminate. destruct H12.
      destruct (IHpar_eval_stmt2); eauto. discriminate.
      apply (preservation Mw _ S (n::TT) s1 _ x0); eauto.
      destruct H14.
      eexists. split.
      apply (SCIfD _ _ _ R (n::TT) (n0::TF) _ _ _ x x0 x1 S1); eauto.
      discriminate.
      discriminate.
      apply (Qle_trans _ (work (C2 o+(T) WSMIf (ws Mw Ms)) +
                          Mw.(MDiv) * of_nat (length (C2 o+(T) WSMIf (ws Mw Ms))))).
      eauto with cost_block.
      apply (Qle_trans _ (work C2 + Mw.(MIf) * of_nat (length C2)
                          + MDiv Mw * of_nat (length (C2 o+( T) WSMIf (ws Mw Ms))))).
      apply Qplus_le_compat.
      eauto with cost_block.
      apply Qle_refl.
      rewrite <- Qplus_assoc.
      apply (le_rew _ _ _ _ H15).
      rewrite <- Qplus_assoc.
      apply (le_rew _ _ _ _ H13).
      rewrite <- Qplus_assoc.
      apply (le_rew _ _ _ _ H4).
      assert (length (C2 o+( T) WSMIf (ws Mw Ms)) = length C2).
      eauto with cost_block.
      rewrite H16.
      rewrite HL0. rewrite <- HL2. rewrite <- HL1.
      psatz Q 2.

  - (* While *)
    eqlengths C0 C HL0.
    eqlengths C C1 HL1.
    eqlengths C1 C2 HL2.
    rename H4 into HL0'.
    rename H5 into HL1'.
    rename H6 into HL2'.
    apply eval_exp_work_equiv in H1. do 2 destruct H1.
    inversion H_valid.
    set (H_T := logic.TF_TT_T _ _ _ _ _ _ _ _ _ H8 H_typed H1 H2 H3).
    case TT, TF.
    + (* [], [] *)
      destruct H.
      induction T.
      reflexivity.
      destruct (H_T a).
      destruct (H10 (in_eq a T)).
      apply in_nil in H11. contradiction.
      apply in_nil in H11. contradiction.
    + (* [], T *)
      inversion H0_0; try contradiction.
      inversion H0_; try contradiction.
      subst S2.
      subst S1.
      subst S3.
      subst S4.
      subst C3.
      subst C2.
      subst C1.
      destruct (rem_false Sig Mw S T e R x); eauto.
      subst R.
      eexists. split.
      apply (SCWhileNone); eauto.
      rewrite work_plus_div_false.
      apply (Qle_trans _ (work C + Mw.(MIf) * of_nat (length C))).
      eauto with cost_block.
      apply (le_rew _ _ _ _ H4).
      psatz Q 2.
    + (* [], T *)
      destruct (rem_true Sig Mw S T e R x); eauto.
      destruct (IHpar_eval_stmt1); eauto. discriminate. destruct H12.
      destruct (IHpar_eval_stmt2); eauto. discriminate.
      apply (preservation Mw _ S (n::TT) s _ x0); eauto.
      destruct H14.
      subst R.
      eexists. split.
      rewrite <- H2 in H10.
      rewrite <- H10 in H12.
      rewrite <- H10 in H14.
      apply (SCWhileAll _ _ _ _ _ S1); eauto.
      rewrite work_plus_div_true.
      apply (Qle_trans _ (work C2 + Mw.(MIf) * of_nat (length C2))).
      eauto with cost_block.
      apply (le_rew _ _ _ _ H15).
      rewrite <- Qplus_assoc.
      apply (le_rew _ _ _ _ H13).
      rewrite <- Qplus_assoc.
      apply (le_rew _ _ _ _ H4).
      rewrite HL0. rewrite <- HL2. rewrite <- HL1.
      psatz Q 2.
    + (* TT, TF *)
      destruct (IHpar_eval_stmt1); eauto. discriminate. destruct H10.
      destruct (IHpar_eval_stmt2); eauto. discriminate.
      apply (preservation Mw _ S (n::TT) s _ x0); eauto.
      destruct H12.
      eexists. split.
      apply (SCWhileSome _ _ _ R (n::TT) _ _ S1); eauto.
      discriminate.
      apply (TF_TT_not_T Mw S _ e R x _ (n0::TF)); eauto.
      apply (pres_exp Mw Sig S T e _ _ x); eauto.
      discriminate.

      apply (Qle_trans _ (work (C2 o+(T) WSMIf (ws Mw Ms))
                          + Mw.(MDiv) * of_nat (length (C2 o+(T) WSMIf (ws Mw Ms))))).
      eauto with cost_block.
      apply (Qle_trans _ (work C2 + Mw.(MIf) * of_nat (length C2)
                          + MDiv Mw * of_nat (length (C2 o+( T) WSMIf (ws Mw Ms))))).
      apply Qplus_le_compat.
      eauto with cost_block.
      apply Qle_refl.
      rewrite <- Qplus_assoc.
      apply (le_rew _ _ _ _ H13).
      rewrite <- Qplus_assoc.
      apply (le_rew _ _ _ _ H11).
      rewrite <- Qplus_assoc.
      apply (le_rew _ _ _ _ H4).
      assert (length (C2 o+( T) WSMIf (ws Mw Ms)) = length C2).
      eauto with cost_block.
      rewrite H14.
      rewrite HL0. rewrite <- HL2. rewrite <- HL1.
      psatz Q 2.

  - (* Seq *)
    eqlengths C0 C1 HL0.
    eqlengths C1 C2 HL1.
    inversion H_valid.
    destruct (IHpar_eval_stmt1); eauto. destruct H8.
    destruct (IHpar_eval_stmt2); eauto.
    apply (preservation Mw _ S T s1 _ x); eauto.
    destruct H10.
    eexists. split. eauto with eval.
    apply (Qle_trans _ _ _ H11).
    apply (le_rew _ _ _ _ H9).
    psatz Q 2.

  - (* Sync *)
    eexists. split. eauto with eval.
    assert (forall C0, work (map (fun ws0 : Q * Q => (fst ws0, span C)) C0) = work C0).
    intros.
    induction C0.
    reflexivity.
    unfold map.
    fold map.
    unfold work.
    fold work.
    simpl.
    unfold map in IHC0.
    rewrite IHC0.
    reflexivity.
    apply (Qle_trans _ (work (map (fun ws0 : Cost * Cost => (fst ws0, span C)) C)
                        + Mw.(MSync) * of_nat (length (map (fun ws0 : Cost * Cost => (fst ws0, span C)) C)))).
    eauto with cost_block.
    rewrite H1.
    rewrite map_length.
    apply Qle_refl.

  - (* Switch *)
    inversion H_valid.
  - inversion H_valid.
Qed.

Lemma eval_stmt_span_equiv : forall Sig Mw Ms C S T s S' C',
    typed_state Sig S ->
    valid Sig s ->
    T <> [] ->
    par_eval_stmt (ws Mw Ms) C S T s S' C' ->
    exists Cs, eval_stmt Ms S T s S' Cs /\
               span C' <= span C + Cs.
Proof.
  intros Sig Mw Ms C S T s S' C' H_typed H_valid. intros.
  generalize dependent S.
  remember (ws Mw Ms) as M.
  induction 2; intros; subst M.

  - contradiction.

  - (* Skip *)
    eexists. split; eauto with cost eval cost_block.
    lra.

  - (* VWrite *)
    apply eval_exp_span_equiv in H1. do 2 destruct H1.
    eexists. split. eauto with eval.
    apply (Qle_trans _ (span C + Ms.(MVWrite))).
    eauto with cost_block.
    apply (le_rew _ _ _ _ H3).
    psatz Q 2.

  - (* GWrite *)
    apply eval_opd_span_equiv in H3. do 2 destruct H3.
    apply eval_exp_span_equiv in H4. do 2 destruct H4.
    eexists. split.
    apply (SCGWrite _ _ _ _ _ _ _ _ _ _ _ H1 H2 H3 H4).
    assumption.
    apply (Qle_trans _ (span C2 + Ms.(MGWrite)
                                       (memreads
                                          (map (fun tn : nat * nat => (fst tn, VNat (snd tn))) inds)))).
    eauto with cost_block.
    apply (le_rew _ _ _ _ H7).
    rewrite <- Qplus_assoc.
    apply (le_rew _ _ _ _ H6).
    psatz Q 2.

  - (* SWrite *)
    apply eval_opd_span_equiv in H3. do 2 destruct H3.
    apply eval_exp_span_equiv in H4. do 2 destruct H4.
    eexists. split.
    apply (SCSWrite _ _ _ _ _ _ _ _ _ _ _ H1 H2 H3 H4).
    assumption.
    apply (Qle_trans _ (span C2 + Ms.(MSWrite)
                                       (conflicts
                                          (map (fun tn : nat * nat => (fst tn, VNat (snd tn))) inds)))).
    eauto with cost_block.
    apply (le_rew _ _ _ _ H7).
    rewrite <- Qplus_assoc.
    apply (le_rew _ _ _ _ H6).
    psatz Q 2.

  - (* If *)
    apply eval_exp_span_equiv in H1. do 2 destruct H1.
    inversion H_valid.
    set (H_T := logic.TF_TT_T _ _ _ _ _ _ _ _ _ H9 H_typed H1 H2 H3).
    case TT, TF.
    + (* [], [] *)
      destruct H.
      induction T.
      reflexivity.
      destruct (H_T a).
      destruct (H12 (in_eq a T)).
      apply in_nil in H13. contradiction.
      apply in_nil in H13. contradiction.
    + (* [], T *)
      inversion H0_; try contradiction.
      subst S1. subst C1.
      destruct (IHpar_eval_stmt2); eauto.
      discriminate.      
      destruct H15.
      destruct (rem_false Sig Ms S T e R x); eauto.
      rewrite <- H3 in H18.
      eexists. split.
      rewrite <- H18 in H15.
      subst R.
      apply (SCIfF); eauto with eval cost cost_block.
      rewrite span_plus_div_false.
      apply (Qle_trans _ (span C2 + Ms.(MIf))).
      eauto with cost_block.
      apply (le_rew _ _ _ _ H17).
      rewrite <- Qplus_assoc.
      apply (le_rew _ _ _ _ H4).
      psatz Q 2.
    + (* T, [] *)
      inversion H0_0; try contradiction.
      subst S2. subst C2.
      destruct (IHpar_eval_stmt1); eauto.
      discriminate.      
      destruct H15.
      destruct (rem_true Sig Ms S T e R x); eauto.
      rewrite <- H2 in H18.
      eexists. split.
      rewrite <- H18 in H15.
      subst R.
      apply (SCIfT); eauto with eval cost cost_block.
      rewrite span_plus_div_true.
      apply (Qle_trans _ (span C1 + Ms.(MIf))).
      eauto with cost_block.
      apply (le_rew _ _ _ _ H17).
      rewrite <- Qplus_assoc.
      apply (le_rew _ _ _ _ H4).
      psatz Q 2.
    + (* TT, TF *)      
      destruct (IHpar_eval_stmt1); eauto. discriminate. destruct H12.
      destruct (IHpar_eval_stmt2); eauto. discriminate.
      apply (preservation Ms _ S (n::TT) s1 _ x0); eauto.
      destruct H14.
      eexists. split.
      apply (SCIfD _ _ _ R (n::TT) (n0::TF) _ _ _ x x0 x1 S1); eauto.
      discriminate.
      discriminate.
      apply (Qle_trans _ (span (C2 o+(T) WSMIf (ws Mw Ms)) +
                          Ms.(MDiv))).
      apply span_plus_div.
      apply (Qle_trans _ (span C2 + Ms.(MIf) + MDiv Ms)).
      apply Qplus_le_compat.
      eauto with cost_block.
      apply Qle_refl.
      rewrite <- Qplus_assoc.
      apply (le_rew _ _ _ _ H15).
      rewrite <- Qplus_assoc.
      apply (le_rew _ _ _ _ H13).
      rewrite <- Qplus_assoc.
      apply (le_rew _ _ _ _ H4).
      psatz Q 2.

  - (* While *)
    apply eval_exp_span_equiv in H1. do 2 destruct H1.
    inversion H_valid.
    set (H_T := logic.TF_TT_T _ _ _ _ _ _ _ _ _ H8 H_typed H1 H2 H3).
    case TT, TF.
    + (* [], [] *)
      destruct H.
      induction T.
      reflexivity.
      destruct (H_T a).
      destruct (H10 (in_eq a T)).
      apply in_nil in H11. contradiction.
      apply in_nil in H11. contradiction.
    + (* [], T *)
      inversion H0_0; try contradiction.
      inversion H0_; try contradiction.
      subst S2.
      subst S1.
      subst S3.
      subst S4.
      subst C3.
      subst C2.
      subst C1.
      destruct (rem_false Sig Ms S T e R x); eauto.
      subst R.
      eexists. split.
      apply (SCWhileNone); eauto.
      rewrite span_plus_div_false.
      apply (Qle_trans _ (span C + Ms.(MIf))).
      eauto with cost_block.
      apply (le_rew _ _ _ _ H4).
      psatz Q 2.
    + (* [], T *)
      destruct (rem_true Sig Ms S T e R x); eauto.
      destruct (IHpar_eval_stmt1); eauto. discriminate. destruct H12.
      destruct (IHpar_eval_stmt2); eauto. discriminate.
      apply (preservation Ms _ S (n::TT) s _ x0); eauto.
      destruct H14.
      subst R.
      eexists. split.
      rewrite <- H2 in H10.
      rewrite <- H10 in H12.
      rewrite <- H10 in H14.
      apply (SCWhileAll _ _ _ _ _ S1); eauto.
      rewrite span_plus_div_true.
      apply (Qle_trans _ (span C2 + Ms.(MIf))).
      eauto with cost_block.
      apply (le_rew _ _ _ _ H15).
      rewrite <- Qplus_assoc.
      apply (le_rew _ _ _ _ H13).
      rewrite <- Qplus_assoc.
      apply (le_rew _ _ _ _ H4).
      psatz Q 2.
    + (* TT, TF *)
      destruct (IHpar_eval_stmt1); eauto. discriminate. destruct H10.
      destruct (IHpar_eval_stmt2); eauto. discriminate.
      apply (preservation Ms _ S (n::TT) s _ x0); eauto.
      destruct H12.
      eexists. split.
      apply (SCWhileSome _ _ _ R (n::TT) _ _ S1); eauto.
      discriminate.
      apply (TF_TT_not_T Ms S _ e R x _ (n0::TF)); eauto.
      apply (pres_exp Ms Sig S T e _ _ x); eauto.
      discriminate.

      apply (Qle_trans _ (span (C2 o+(T) WSMIf (ws Mw Ms)) + Ms.(MDiv))).
      apply span_plus_div.
      apply (Qle_trans _ (span C2 + Ms.(MIf) + MDiv Ms)).
      apply Qplus_le_compat.
      eauto with cost_block.
      apply Qle_refl.
      rewrite <- Qplus_assoc.
      apply (le_rew _ _ _ _ H13).
      rewrite <- Qplus_assoc.
      apply (le_rew _ _ _ _ H11).
      rewrite <- Qplus_assoc.
      apply (le_rew _ _ _ _ H4).
      psatz Q 2.

  - (* Seq *)
    inversion H_valid.
    destruct (IHpar_eval_stmt1); eauto. destruct H6.
    destruct (IHpar_eval_stmt2); eauto.
    apply (preservation Ms _ S T s1 _ x); eauto.
    destruct H8.
    eexists. split. eauto with eval.
    apply (Qle_trans _ _ _ H9).
    apply (le_rew _ _ _ _ H7).
    psatz Q 2.

  - (* Sync *)
    eexists. split. eauto with eval.
    apply (Qle_trans _ (span (map (fun ws0 : Cost * Cost => (fst ws0, span C)) C)
                        + Ms.(MSync))).
    eauto with cost_block.
    assert (forall x C,
               x >= 0 ->
               (C = [] /\
                span (map (fun ws0 : Cost * Q => (fst ws0, x)) C) == 0) \/
               (C <> [] /\
                span (map (fun ws0 : Cost * Q => (fst ws0, x)) C) == x)).
    intros. induction C0.
    left.
    split; reflexivity.

    right.
    split. discriminate.
    unfold span.
    unfold map.
    fold map.
    fold span.
    simpl.
    destruct IHC0; destruct H2.
    unfold max.
    assert (x >= (span
         ((fix map (l : list (Cost * Q)) : list (Cost * Q) :=
             match l with
             | [] => []
             | a0 :: t => (fst a0, x) :: map t
             end) C0))).
    unfold map in H3.
    lra.
    apply Qle_lteq in H4.
    destruct H4.
    assert (Qle_bool x
      (span
         ((fix map (l : list (Cost * Q)) : list (Cost * Q) :=
             match l with
             | [] => []
             | a0 :: t => (fst a0, x) :: map t
             end) C0)) = false).
    remember (Qle_bool x
    (span
       ((fix map (l : list (Cost * Q)) : list (Cost * Q) :=
           match l with
           | [] => []
           | a0 :: t => (fst a0, x) :: map t
           end) C0))) as b.
    destruct b.
    apply sym_eq in Heqb.
    apply Qle_bool_iff in Heqb.
    apply Qlt_not_le in H4.
    contradiction.
    reflexivity.
    rewrite H5.
    reflexivity.
    
    assert (Qle_bool x
      (span
         ((fix map (l : list (Cost * Q)) : list (Cost * Q) :=
             match l with
             | [] => []
             | a0 :: t => (fst a0, x) :: map t
             end) C0)) = true).
    apply Qeq_sym in H4.
    apply Qeq_le_weak in H4.
    apply Qle_bool_iff in H4.
    assumption.
    rewrite H5.
    assumption.
    
    unfold max.
    assert (Qle_bool x
      (span
         ((fix map (l : list (Cost * Q)) : list (Cost * Q) :=
             match l with
             | [] => []
             | a0 :: t => (fst a0, x) :: map t
             end) C0)) = true).
    apply Qle_bool_iff.
    unfold map in H3.
    lra.
    rewrite H4.
    trivial.

    induction C.
    unfold map.
    apply Qle_refl.
    destruct (H1 (span (a::C)) (a::C)).
    apply span_pos.
    destruct H2.
    discriminate.
    destruct H2.
    apply Qplus_le_compat.
    apply Qeq_le_weak.
    assumption.
    apply Qle_refl.

  - (* Switch *)
    inversion H_valid.
  - inversion H_valid.
Qed.

Fixpoint empty_parcost (T: threads) :=
  map (fun (_ : nat) => ((0, 0) : Cost * Cost))
      (List.nodup eq_nat_dec (map warp_of_thread T)).

Lemma empty_all_zero: forall T x, In x (empty_parcost T) -> x = (0, 0).
Proof.
  intros.
  induction T.
  simpl in H.
  contradiction.
  unfold empty_parcost in H.  
  apply in_map_iff in H.
  destruct H.
  destruct H.
  apply sym_eq.
  assumption.
Qed.  

Lemma empty_work : forall T, work (empty_parcost T) = 0.
Proof.
  assert (forall L, (forall x, In x L -> x = (0, 0)) -> work L = 0).
  intros.
  induction L.
  reflexivity.
  unfold work.
  unfold work in IHL.
  rewrite IHL.
  rewrite (H a).
  simpl.
  reflexivity.
  apply in_eq.
  intros.
  apply H.
  apply in_cons.
  assumption.
  
  intros.
  apply H.
  apply empty_all_zero.
Qed.

Lemma empty_span : forall T, span (empty_parcost T) = 0.
  assert (forall L, (forall x, In x L -> x = (0, 0)) -> span L = 0).
  intros.
  induction L.
  reflexivity.
  unfold span.
  unfold span in IHL.
  rewrite IHL.
  rewrite (H a).
  simpl.
  reflexivity.
  apply in_eq.
  intros.
  apply H.
  apply in_cons.
  assumption.
  
  intros.
  apply H.
  apply empty_all_zero.
Qed.

Require Import logic.

(** Theorem 3 **)
Theorem par_equiv: forall Sig Mw Ms cw cs s cw' cs' S T S' C' Cw Cs,
  valid Sig s ->
  typed_state Sig S ->
  cstmt Mw cw s cw' ->
  cstmt Ms cs s cs' ->
  (S ;; T |= cw) ->
  (S ;; T |= cs) ->
  T <> [] ->
  uniq_t T ->
  par_eval_stmt (ws Mw Ms) (empty_parcost T) S T s S' C' ->
  Qap (Q cw) S (X cw) Cw ->
  Qap (Q cs) S (X cs) Cs ->
  exists Cw' Cs',
    Qap (Q cw') S' (X cw') Cw' /\
    Qap (Q cs') S' (X cs') Cs' /\    
    work C' <= (Cw - Cw') * (of_nat (length C')) /\
    span C' <= Cs - Cs'.
Proof.
  intros.
  assert (par_eval_stmt (ws Mw Ms) (empty_parcost T) S T s S' C').
  assumption.
  assert (length (empty_parcost T) = length C') as HL.
  eauto with lengths.
  apply (eval_stmt_work_equiv Sig) in H7; eauto. do 2 destruct H7.
  apply (eval_stmt_span_equiv Sig) in H10; eauto. do 2 destruct H10.
  apply (cstmt_sound Sig _ cw _ cw' _ _ _ Cw) in H7; eauto.
  apply (cstmt_sound Sig _ cs _ cs' _ _ _ Cs) in H10; eauto.
  do 2 destruct H7. destruct H13.
  do 2 destruct H10. destruct H15.
  exists x1. exists x2.
  split. eauto.
  split. eauto.
  split.
  rewrite <- HL.
  rewrite empty_work in H11.
  rewrite Qplus_0_l in H11.
  apply (Qle_trans _ (x * of_nat (length (empty_parcost T)))).
  assumption.  
  apply Qmult_le_compat_r.
  lra.
  unfold of_nat.
  assert (0 = inject_Z 0).
  eauto.
  rewrite H17.
  rewrite <- Zle_Qle.
  apply Nat2Z.is_nonneg.
  rewrite empty_span in H12.
  rewrite Qplus_0_l in H12.
  lra.
Qed.

(** Some additional consistency checks that lock-step evaluation implies
 ** parallel evaluation is possible **)

Lemma eval_imp_par_eval_opd: forall M PM S T o R C C0,
    eval_opd M S T o R C ->
    exists C1, par_eval_opd PM C0 S T o R C1.
Proof.
  intros.
  induction H; eexists; eauto with cost_block.
Qed.

Hint Resolve eval_imp_par_eval_opd : cost_block.

Lemma eval_imp_par_eval_exp: forall M PM S T e R C C0,
    eval_exp M S T e R C ->
    exists C1, par_eval_exp PM C0 S T e R C1.
Proof.
  intros.
  induction H;
    try (apply (eval_imp_par_eval_opd _ PM _ _ _ _ _ C0) in H; destruct H);
    try (apply (eval_imp_par_eval_opd _ PM _ _ _ _ _ x) in H0; destruct H0);
    eexists;
    eauto with cost_block.
Qed.

Lemma eval_imp_par_eval_stmt: forall M PM S T s R C C0,
    T <> [] ->
    eval_stmt M S T s R C ->
    exists C1, par_eval_stmt PM C0 S T s R C1.
Proof.
  intros.
  generalize dependent C0.
  induction H0; intros;
    try (apply (eval_imp_par_eval_exp _ PM _ _ _ _ _ C0) in H0; destruct H0);
    try (apply (eval_imp_par_eval_opd _ PM _ _ _ _ _ C0) in H0; destruct H0);
    try (apply (eval_imp_par_eval_exp _ PM _ _ _ _ _ x) in H1; destruct H1);
    try (apply (eval_imp_par_eval_opd _ PM _ _ _ _ _ C0) in H2; destruct H2);
    try (apply (eval_imp_par_eval_exp _ PM _ _ _ _ _ x) in H3; destruct H3);
    try solve [eexists; econstructor; eauto with cost_block].

  - destruct (IHeval_stmt H x); eauto.
    eexists.
    apply (PSCIf _ _ _ _ (map (fun t : nat => (t, VBool true)) T) T []
                 _ _ _ x x0 x0 S'); eauto with cost_block.

  - destruct (IHeval_stmt H x); eauto.
    eexists.
    apply (PSCIf _ _ _ _ (map (fun t : nat => (t, VBool false)) T) [] T
                 _ _ _ x x x0 S); eauto with cost_block.

  - destruct (IHeval_stmt1 H3 x); eauto.
    destruct (IHeval_stmt2 H4 x0); eauto.
    eexists. eauto with cost_block.

  - destruct (IHeval_stmt1 H x); eauto.
    destruct (IHeval_stmt2 H x0); eauto.
    eexists.
    apply (PSCWhile _ _ _ _ (map (fun t : nat => (t, VBool true)) T) T [] _ _ S1 _ x x0); eauto with cost_block.

  - destruct (IHeval_stmt1 H2 x); eauto.
    destruct (IHeval_stmt2 H2 x0); eauto.
    eexists.
    apply (PSCWhile _ _ _ _ R TT
                    (map (fun tv : nat * value => fst tv) (filter is_false R))
                    _ _ S1 _ x x0); eauto with cost_block.

  - eexists.
    apply (PSCWhile _ _ _ _ (map (fun t : nat => (t, VBool false)) T) [] T  _ _
                    S _ x x); eauto with cost_block.

  - destruct (IHeval_stmt1 H C0); eauto.
    destruct (IHeval_stmt2 H x); eauto.
    eexists. econstructor; eauto with cost_block.
Qed.
