(* The local-cleanup rules that rewrite adjacent instructions: from the same
   state, the original sequence and its rewrite run to the same configuration
   under SpecTec's Wasm 2.0 semantics.

   1. `local.set x; local.get x` → `local.tee x`
   2. `local.tee x; drop` → `local.set x`
   5. `local.get`, `global.get` or a constant, then `drop` → nothing

   `steps_in_context` lifts a run under any operand stack below it and before
   any instructions after it. *)
From Stdlib Require Import List NArith Lia.
From mathcomp Require Import ssreflect ssrbool eqtype seq.
Require Import Wasm2.

(* Composing runs. *)

Lemma steps_trans : forall a b c, Steps a b -> Steps b c -> Steps a c.
Proof.
  intros a b c Hab Hbc. induction Hab; [exact Hbc | ].
  inversion Hbc; subst; eapply trans; eauto.
Qed.

Lemma steps_one : forall a b, Step a b -> wf_config a -> wf_config b -> Steps a b.
Proof.
  move=> [z code] [z' code'] Hstep Ha Hb.
  exact: (trans _ _ _ _ _ _ Hstep (Steps__refl _ _ Hb) Ha Hb Hb).
Qed.

(* Well-formedness of the configurations the rules pass through. *)

Lemma val_admin_wf : forall v, wf_val v -> wf_admininstr (admininstr_val v).
Proof. by move=> v; case=> *; constructor. Qed.

Lemma vals_admin_wf : forall vs,
  List.Forall wf_val vs -> List.Forall wf_admininstr (map admininstr_val vs).
Proof.
  elim=> [ | v vs IH] Hvs //=. inversion Hvs; subst.
  constructor; [exact: val_admin_wf | exact: IH].
Qed.

Lemma Forall_cat : forall (A : Type) (P : A -> Prop) (s1 s2 : seq A),
  List.Forall P s1 -> List.Forall P s2 -> List.Forall P (s1 ++ s2).
Proof.
  move=> A P. elim=> [ | x s1 IH] s2 H1 H2 //=. inversion H1; subst.
  constructor; [done | exact: IH].
Qed.

Lemma config_wf_app : forall z vs code rest,
  List.Forall wf_val vs -> List.Forall wf_admininstr rest ->
  wf_config (mk_config z code) ->
  wf_config (mk_config z (map admininstr_val vs ++ (code ++ rest))).
Proof.
  move=> z vs code rest Hvs Hrest Hwf. inversion Hwf as [? ? Hz Hcode]; subst.
  apply: config_case_0 => //.
  apply: Forall_cat; first exact: vals_admin_wf.
  exact: Forall_cat.
Qed.

(* Lifting runs under an operand stack and before further instructions. *)

Lemma step_in_context : forall z code z' code' vs rest,
  Step (mk_config z code) (mk_config z' code') ->
  List.Forall wf_val vs -> List.Forall wf_admininstr rest ->
  wf_config (mk_config z code) -> wf_config (mk_config z' code') ->
  Step (mk_config z (map admininstr_val vs ++ (code ++ rest)))
       (mk_config z' (map admininstr_val vs ++ (code' ++ rest))).
Proof.
  move=> z code z' code' vs rest Hstep Hvs Hrest Hb Ha.
  case: vs Hvs => [ | v vs] Hvs; case: rest Hrest => [ | i rest] Hrest.
  - by rewrite /= !cats0.
  - by apply: ctxt_instrs.
  - by apply: ctxt_instrs.
  - by apply: ctxt_instrs.
Qed.

Lemma steps_in_context : forall c c' vs rest,
  Steps c c' -> List.Forall wf_val vs -> List.Forall wf_admininstr rest ->
  match c, c' with
  | mk_config z code, mk_config z' code' =>
      Steps (mk_config z (map admininstr_val vs ++ (code ++ rest)))
            (mk_config z' (map admininstr_val vs ++ (code' ++ rest)))
  end.
Proof.
  move=> c c' vs rest Hsteps Hvs Hrest.
  elim: Hsteps => [z code Hwf | z code z'' code'' z' code' Hstep _ IH Hb Ha Hm].
  - apply: Steps__refl. exact: config_wf_app.
  - apply: (trans _ _ _ _ _ _ _ IH); try exact: config_wf_app.
    exact: step_in_context.
Qed.

(* Reading a local right after setting it. *)

Lemma nth_list_update : forall (A : Type) (d : A) (l : seq A) (n : N) (g : A -> A),
  (N.to_nat n < size l)%nat ->
  seq.nth d (list_update_func l n g) (N.to_nat n) = g (seq.nth d l (N.to_nat n)).
Proof.
  move=> A d l. elim: l => [ | x l IH] n g /= Hsize; first lia.
  case: n Hsize => [ | p] Hsize; first by rewrite /=.
  have Hpred : N.to_nat (N.pos p) = S (N.to_nat (N.pred (N.pos p))) by lia.
  rewrite Hpred. cbn [list_update_func seq.nth].
  apply: IH. move: Hsize. rewrite Hpred /=. lia.
Qed.

Lemma local_after_set : forall s f x v,
  (N.to_nat (x :> N) < size (LOCALS f))%nat ->
  fun_local (with_local (mk_state s f) x v) x = v.
Proof.
  move=> s f x v Hrange. rewrite /fun_local /with_local /lookup_total /=.
  by rewrite nth_list_update.
Qed.

(* The rules, from a well-formed state whose frame has local `x`. *)

Lemma steps_wf : forall a b, Steps a b -> wf_config a /\ wf_config b.
Proof. by move=> a b; case. Qed.

Lemma steps_cons : forall a b c, Step a b -> Steps b c -> wf_config a -> Steps a c.
Proof.
  move=> [za ca] [zb cb] [zc cc] Hstep Hsteps Ha.
  have [Hb Hc] := steps_wf _ _ Hsteps.
  exact: (trans _ _ _ _ _ _ Hstep Hsteps Ha Hc Hb).
Qed.

Section Rules.

Variables (s : store) (f : frame) (x : idx).
Hypotheses (Hstate : wf_state (mk_state s f)) (Hx : wf_uN 32%N x)
  (Hrange : (N.to_nat (x :> N) < size (LOCALS f))%nat).

Let z := mk_state s f.

Ltac wf_code := repeat (apply: List.Forall_cons || apply: List.Forall_nil);
  solve [ by apply: val_admin_wf | by constructor ].
Ltac wf := apply: config_case_0; [done | wf_code].

Lemma set_local_wf : forall v, wf_val v -> wf_state (with_local z x v).
Proof. move=> v Hv. exact: (with_local_is_wf z x v _ Hstate Hx Hv (eqxx _)). Qed.

(* `v; local.set x` with `rest` after it, as one step. *)
Lemma set_in_context : forall vs v rest,
  List.Forall wf_val vs -> wf_val v -> List.Forall wf_admininstr rest ->
  Step (mk_config z (map admininstr_val vs ++ [:: admininstr_val v; admininstr_LOCAL_SET x] ++ rest))
       (mk_config (with_local z x v) (map admininstr_val vs ++ rest)).
Proof.
  move=> vs v rest Hvs Hv Hrest. have Hz' := set_local_wf v Hv.
  exact: (step_in_context _ _ _ [::] _ _ (Step__local_set z v x) Hvs Hrest
    ltac:(wf) ltac:(wf)).
Qed.

(* 1. `local.set x; local.get x` → `local.tee x` *)
Lemma set_get_tee : forall v, wf_val v ->
  Steps (mk_config z [:: admininstr_val v; admininstr_LOCAL_SET x; admininstr_LOCAL_GET x])
        (mk_config (with_local z x v) [:: admininstr_val v]) /\
  Steps (mk_config z [:: admininstr_val v; admininstr_LOCAL_TEE x])
        (mk_config (with_local z x v) [:: admininstr_val v]).
Proof.
  move=> v Hv. have Hz' := set_local_wf v Hv.
  have Hlocal : fun_local (with_local z x v) x = v := local_after_set s f x v Hrange.
  have Hget : List.Forall wf_admininstr [:: admininstr_LOCAL_GET x] by wf_code.
  have Hv1 : List.Forall wf_val [:: v] by constructor.
  have S1 : Step
      (mk_config z [:: admininstr_val v; admininstr_LOCAL_SET x; admininstr_LOCAL_GET x])
      (mk_config (with_local z x v) [:: admininstr_LOCAL_GET x])
    := set_in_context [::] v _ (List.Forall_nil _) Hv Hget.
  have S2 := read _ _ _ (Step_read__local_get (with_local z x v) x).
  rewrite Hlocal in S2.
  have T1 : Step
      (mk_config z [:: admininstr_val v; admininstr_LOCAL_TEE x])
      (mk_config z [:: admininstr_val v; admininstr_val v; admininstr_LOCAL_SET x])
    := pure _ _ _ (Step_pure__local_tee v x).
  have T2 : Step
      (mk_config z [:: admininstr_val v; admininstr_val v; admininstr_LOCAL_SET x])
      (mk_config (with_local z x v) [:: admininstr_val v])
    := set_in_context [:: v] v [::] Hv1 Hv (List.Forall_nil _).
  split.
  - apply: (steps_cons _ _ _ S1); last by wf.
    by apply: (steps_one _ _ S2); wf.
  - apply: (steps_cons _ _ _ T1); last by wf.
    by apply: (steps_one _ _ T2); wf.
Qed.

(* 2. `local.tee x; drop` → `local.set x` *)
Lemma tee_drop_set : forall v, wf_val v ->
  Steps (mk_config z [:: admininstr_val v; admininstr_LOCAL_TEE x; admininstr_DROP])
        (mk_config (with_local z x v) [::]) /\
  Steps (mk_config z [:: admininstr_val v; admininstr_LOCAL_SET x])
        (mk_config (with_local z x v) [::]).
Proof.
  move=> v Hv. have Hz' := set_local_wf v Hv.
  have Hdrop : List.Forall wf_admininstr [:: admininstr_DROP] by wf_code.
  have Hv1 : List.Forall wf_val [:: v] by constructor.
  have S1 : Step
      (mk_config z [:: admininstr_val v; admininstr_LOCAL_TEE x; admininstr_DROP])
      (mk_config z [:: admininstr_val v; admininstr_val v; admininstr_LOCAL_SET x; admininstr_DROP])
    := step_in_context z _ z _ [::] _ (pure _ _ _ (Step_pure__local_tee v x))
         (List.Forall_nil _) Hdrop ltac:(wf) ltac:(wf).
  have S2 : Step
      (mk_config z [:: admininstr_val v; admininstr_val v; admininstr_LOCAL_SET x; admininstr_DROP])
      (mk_config (with_local z x v) [:: admininstr_val v; admininstr_DROP])
    := set_in_context [:: v] v _ Hv1 Hv Hdrop.
  have S3 : Step
      (mk_config (with_local z x v) [:: admininstr_val v; admininstr_DROP])
      (mk_config (with_local z x v) [::])
    := pure _ _ _ (Step_pure__drop v).
  have T1 : Step
      (mk_config z [:: admininstr_val v; admininstr_LOCAL_SET x])
      (mk_config (with_local z x v) [::])
    := set_in_context [::] v [::] (List.Forall_nil _) Hv (List.Forall_nil _).
  split.
  - apply: (steps_cons _ _ _ S1); last by wf.
    apply: (steps_cons _ _ _ S2); last by wf.
    by apply: (steps_one _ _ S3); wf.
  - by apply: (steps_one _ _ T1); wf.
Qed.

(* 5. A push, then `drop`, → nothing. *)

Lemma push_drop : forall code v, wf_val v ->
  Step (mk_config z code) (mk_config z [:: admininstr_val v]) ->
  wf_config (mk_config z code) ->
  Steps (mk_config z (code ++ [:: admininstr_DROP])) (mk_config z [::]).
Proof.
  move=> code v Hv Hpush Hwf.
  have Hdrop : List.Forall wf_admininstr [:: admininstr_DROP] by wf_code.
  have S1 : Step
      (mk_config z (code ++ [:: admininstr_DROP]))
      (mk_config z [:: admininstr_val v; admininstr_DROP])
    := step_in_context z code z _ [::] _ Hpush (List.Forall_nil _) Hdrop Hwf ltac:(wf).
  have S2 : Step (mk_config z [:: admininstr_val v; admininstr_DROP]) (mk_config z [::])
    := pure _ _ _ (Step_pure__drop v).
  apply: (steps_cons _ _ _ S1);
    last exact: (config_wf_app _ [::] _ _ (List.Forall_nil _) Hdrop Hwf).
  by apply: (steps_one _ _ S2); wf.
Qed.

Lemma get_drop : forall y, wf_uN 32%N y ->
  Steps (mk_config z [:: admininstr_LOCAL_GET y; admininstr_DROP]) (mk_config z [::]).
Proof.
  move=> y Hy. have Hl := local_is_wf z y _ Hstate Hy (eqxx _).
  apply: (push_drop [:: admininstr_LOCAL_GET y] _ Hl); last by wf.
  exact: read (Step_read__local_get z y).
Qed.

Lemma global_get_drop : forall y, wf_uN 32%N y ->
  Steps (mk_config z [:: admininstr_GLOBAL_GET y; admininstr_DROP]) (mk_config z [::]).
Proof.
  move=> y Hy.
  have Hg : wf_val (VALUE (fun_global z y)).
  { by case: (global_is_wf z y _ Hstate Hy (eqxx _)). }
  apply: (push_drop [:: admininstr_GLOBAL_GET y] _ Hg); last by wf.
  exact: read (Step_read__global_get z y).
Qed.

Lemma const_drop : forall t c, wf_num_ t c ->
  Steps (mk_config z [:: admininstr_CONST t c; admininstr_DROP]) (mk_config z [::]).
Proof.
  move=> t c Hc.
  have Hv : wf_val (val_CONST t c) by constructor.
  by apply: (steps_one _ _ (pure _ _ _ (Step_pure__drop (val_CONST t c)))); wf.
Qed.

End Rules.
