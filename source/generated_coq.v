Require Import pastis.Pastis.

Inductive proc: Type :=
  P_f.

Definition var_global (v: id): bool :=
  match v with
  | _ => false
  end.

Notation V_f_x := 1%positive.
Notation V_f_y := 2%positive.
Notation V_f_n := 3%positive.
Notation V_f_z := 4%positive.
Definition Pedges_f: list (edge proc) :=
  (EA 1 (AGuard (fun s => (~ ((eval (EAdd (EVar V_f_x) (ENum (2))) s) <=
  (eval (EVar V_f_n) s))%Z))) 13)::(EA 1 (AGuard
  (fun s => ((eval (EAdd (EVar V_f_x) (ENum (2))) s) <= (eval (EVar V_f_n)
  s))%Z)) 2)::(EA 2 AWeaken 3)::(EA 3 (AGuard
  (fun s => (~ ((eval (EVar V_f_y) s) > (eval (EVar V_f_x) s))%Z))) 8)::
  (EA 3 (AGuard (fun s => ((eval (EVar V_f_y) s) > (eval (EVar V_f_x)
  s))%Z)) 4)::(EA 4 AWeaken 5)::(EA 5 (AAssign V_f_z (Some (EAdd (EVar V_f_z)
  (ENum (1))))) 6)::(EA 6 (AAssign V_f_x (Some (EAdd (EVar V_f_x)
  (ENum (1))))) 7)::(EA 7 ANone 11)::(EA 8 AWeaken 9)::(EA 9 (AAssign V_f_y
  (Some (EAdd (EVar V_f_y) (ENum (1))))) 10)::(EA 10 ANone 11)::
  (EA 11 ANone 12)::(EA 12 AWeaken 1)::(EA 13 AWeaken 14)::(EA 14 ANone 15)::
  (EA 15 AWeaken 16)::nil.

Instance PROG: Program proc := {
  proc_edges := fun p =>
    match p with
    | P_f => Pedges_f
    end;
  proc_start := fun p => 1%positive;
  proc_end := fun p =>
    (match p with
     | P_f => 16
     end)%positive;
  var_global := var_global
}.

Definition ai_f (p: node) (s: state): Prop := 
  (match p with
   | 1 => (True)%Z
   | 2 => (-1 * s V_f_n+ 1 * s V_f_x + 2 <= 0)%Z
   | 3 => (-1 * s V_f_n+ 1 * s V_f_x + 2 <= 0)%Z
   | 4 => (-1 * s V_f_n+ 1 * s V_f_x + 2 <= 0 /\ 1 * s V_f_x+ -1 * s V_f_y + 1 <= 0)%Z
   | 5 => (1 * s V_f_x+ -1 * s V_f_y + 1 <= 0 /\ -1 * s V_f_n+ 1 * s V_f_x + 2 <= 0)%Z
   | 6 => (-1 * s V_f_n+ 1 * s V_f_x + 2 <= 0 /\ 1 * s V_f_x+ -1 * s V_f_y + 1 <= 0)%Z
   | 7 => (-1 * s V_f_n+ 1 * s V_f_x + 1 <= 0 /\ 1 * s V_f_x+ -1 * s V_f_y <= 0)%Z
   | 8 => (-1 * s V_f_n+ 1 * s V_f_x + 2 <= 0 /\ -1 * s V_f_x+ 1 * s V_f_y <= 0)%Z
   | 9 => (-1 * s V_f_x+ 1 * s V_f_y <= 0 /\ -1 * s V_f_n+ 1 * s V_f_x + 2 <= 0)%Z
   | 10 => (-1 * s V_f_n+ 1 * s V_f_x + 2 <= 0 /\ -1 * s V_f_x+ 1 * s V_f_y + -1 <= 0)%Z
   | 11 => (-1 * s V_f_n+ 1 * s V_f_x + 1 <= 0)%Z
   | 12 => (-1 * s V_f_n+ 1 * s V_f_x + 1 <= 0)%Z
   | 13 => (1 * s V_f_n+ -1 * s V_f_x + -1 <= 0)%Z
   | 14 => (1 * s V_f_n+ -1 * s V_f_x + -1 <= 0)%Z
   | 15 => (1 * s V_f_n+ -1 * s V_f_x + -1 <= 0)%Z
   | 16 => (1 * s V_f_n+ -1 * s V_f_x + -1 <= 0)%Z
   | _ => False
   end)%positive.

Definition annot0_f (p: node) (z: Q) (s: state): Prop := 
  (match p with
   | 1 => (s V_f_z + max0(-1 + s V_f_n - s V_f_x) <= z)%Q
   | 2 => (s V_f_z + max0(-1 + s V_f_n - s V_f_x) <= z)%Q
   | 3 => (s V_f_z + max0(-1 + s V_f_n - s V_f_x) <= z)%Q
   | 4 => hints
     [(*-1 0*) F_max0_pre_decrement 1 (-1 + s V_f_n - s V_f_x) (1)]
     (s V_f_z + max0(-1 + s V_f_n - s V_f_x) <= z)%Q
   | 5 => ((1 # 1) + s V_f_z + max0(-2 + s V_f_n - s V_f_x) <= z)%Q
   | 6 => (s V_f_z + max0(-2 + s V_f_n - s V_f_x) <= z)%Q
   | 7 => (s V_f_z + max0(-1 + s V_f_n - s V_f_x) <= z)%Q
   | 8 => (s V_f_z + max0(-1 + s V_f_n - s V_f_x) <= z)%Q
   | 9 => (s V_f_z + max0(-1 + s V_f_n - s V_f_x) <= z)%Q
   | 10 => (s V_f_z + max0(-1 + s V_f_n - s V_f_x) <= z)%Q
   | 11 => (s V_f_z + max0(-1 + s V_f_n - s V_f_x) <= z)%Q
   | 12 => (s V_f_z + max0(-1 + s V_f_n - s V_f_x) <= z)%Q
   | 13 => (s V_f_z + max0(-1 + s V_f_n - s V_f_x) <= z)%Q
   | 14 => (s V_f_z + max0(-1 + s V_f_n - s V_f_x) <= z)%Q
   | 15 => hints
     [(*-1 0*) F_max0_monotonic (F_check_ge (-1 + s V_f_n - s V_f_x) (-2
                                                                    + s V_f_n
                                                                    - s V_f_x));
      (*-1 0*) F_max0_ge_0 (-2 + s V_f_n - s V_f_x)]
     (s V_f_z + max0(-1 + s V_f_n - s V_f_x) <= z)%Q
   | 16 => (s V_f_z <= z)%Q
   | _ => False
   end)%positive.

Definition ipa: IPA := fun p =>
  match p with
  | P_f =>
    [mkPA Q (fun n z s => ai_f n s /\ annot0_f n z s)]
  end.

Theorem admissible_ipa: IPA_VC ipa.
Proof.
  prove_ipa_vc.
Qed.

Theorem bound_valid:
  forall s1 s2, steps P_f (proc_start P_f) s1 (proc_end P_f) s2 ->
    (s2 V_f_z <= s1 V_f_z + max0(-1 + s1 V_f_n - s1 V_f_x))%Q.
Proof.
  prove_bound ipa admissible_ipa P_f.
Qed.
