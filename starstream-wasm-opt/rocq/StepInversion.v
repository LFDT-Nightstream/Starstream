(* Which steps a configuration can take.

   Every SpecTec rule rewrites a list made of values followed by one more
   instruction (or by a trap and anything after it), so a configuration steps
   at its first non-value instruction, which `split_vals` finds. *)
From Stdlib Require Import List NArith ZArith QArith QArith.Qround Lia.
From mathcomp Require Import ssreflect ssrfun ssrbool eqtype seq.
From RecordUpdate Require Import RecordSet.
Require Import Wasm2 LocalRewrites.

Definition to_val (e : admininstr) : option val :=
  match e with
  | admininstr_CONST t c => Some (val_CONST t c)
  | admininstr_VCONST t c => Some (val_VCONST t c)
  | admininstr_REF_NULL t => Some (val_REF_NULL t)
  | admininstr_REF_FUNC_ADDR a => Some (val_REF_FUNC_ADDR a)
  | admininstr_REF_HOST_ADDR a => Some (val_REF_HOST_ADDR a)
  | _ => None
  end.

Lemma to_val_admin : forall v, to_val (admininstr_val v) = Some v.
Proof. by case. Qed.

Lemma to_val_ref : forall r, to_val (admininstr_ref r) = Some (val_ref r).
Proof. by case. Qed.

Lemma to_val_some : forall e v, to_val e = Some v -> e = admininstr_val v.
Proof. by case=> //= > [<-]. Qed.

(* The values at the front of a list, and the rest. *)
Fixpoint split_vals (es : seq admininstr) : seq val * seq admininstr :=
  match es with
  | [::] => ([::], [::])
  | e :: es' =>
      match to_val e with
      | Some v => (v :: (split_vals es').1, (split_vals es').2)
      | None => ([::], es)
      end
  end.

Lemma split_vals_map_cat : forall vs es,
  split_vals (map admininstr_val vs ++ es) = (vs ++ (split_vals es).1, (split_vals es).2).
Proof. by elim=> [ | v vs IH] es; [case: (split_vals es) | rewrite /= to_val_admin IH]. Qed.

Lemma split_vals_spec : forall es,
  es = map admininstr_val (split_vals es).1 ++ (split_vals es).2.
Proof.
  elim=> [ | e es IH] //=. case Hv: (to_val e) => [v | ] //=.
  by rewrite (to_val_some _ _ Hv) {1}IH.
Qed.

Lemma split_vals_head : forall es h r,
  (split_vals es).2 = h :: r -> to_val h = None.
Proof.
  elim=> [ | e es IH] h r //=. case Hv: (to_val e) => [v | ] /=.
  - exact: IH.
  - by case=> <-.
Qed.

Lemma split_vals_nonval : forall vs h r, to_val h = None ->
  split_vals (map admininstr_val vs ++ h :: r) = (vs, h :: r).
Proof. by move=> vs h r Hh; rewrite split_vals_map_cat /= Hh cats0. Qed.

Lemma split_vals_cat : forall es es', (split_vals es).2 <> [::] ->
  split_vals (es ++ es') = ((split_vals es).1, (split_vals es).2 ++ es').
Proof.
  elim=> [ | e es IH] es' //=. case: (to_val e) => [v | ] //= Hne.
  by rewrite IH.
Qed.

Lemma split_vals_all : forall es, (split_vals es).2 = [::] ->
  forall es', split_vals (es ++ es') = ((split_vals es).1 ++ (split_vals es').1, (split_vals es').2).
Proof.
  move=> es Hall es'. rewrite {1}(split_vals_spec es) Hall cats0.
  by rewrite split_vals_map_cat.
Qed.

(* Rule left-hand sides. *)

Definition redex_lhs (es : seq admininstr) : Prop :=
  match (split_vals es).2 with
  | h :: r => r = [::] \/ h = admininstr_TRAP
  | [::] => False
  end.

Ltac split_vals_simpl :=
  repeat progress (rewrite /= ?to_val_admin ?to_val_ref ?split_vals_map_cat).

Ltac lhs_done := split_vals_simpl; by [left | right].

Lemma pure_lhs : forall es es', Step_pure es es' -> redex_lhs es.
Proof. move=> es es'; case=> *; rewrite /redex_lhs; lhs_done. Qed.

Lemma read_lhs : forall z es es', Step_read (mk_config z es) es' -> redex_lhs es.
Proof.
  move=> z es es' H. remember (mk_config z es) as c eqn:Hc.
  case: H Hc => *; match goal with H : mk_config _ _ = mk_config _ _ |- _ => case: H => *; subst end;
    rewrite /redex_lhs; lhs_done.
Qed.

(* SpecTec's rules that change the state, copied from `Step` so they can be
   told apart from its context rules. `state_step_step` checks the copy. *)
Local Open Scope wasm_scope.
Inductive state_step : config -> config -> Prop :=
	| ss_Step__local_set : forall (z : state) (v_val : val) (x : idx), state_step (mk_config z [::(admininstr_val v_val); (admininstr_LOCAL_SET x)]) (mk_config (with_local z x v_val) [:: ])
	| ss_Step__global_set : forall (z : state) (v_val : val) (x : idx), state_step (mk_config z [::(admininstr_val v_val); (admininstr_GLOBAL_SET x)]) (mk_config (with_global z x v_val) [:: ])
	| ss_table_set_trap : forall (z : state) (i : num_) (v_ref : ref) (x : idx), 
		((proj_num__0 i) != None) ->
		(((!((proj_num__0 i))) :> N) >=? (|(REFS (fun_table z x))|))%BN ->
		state_step (mk_config z [::(admininstr_CONST I32 i); (admininstr_ref v_ref); (admininstr_TABLE_SET x)]) (mk_config z [::admininstr_TRAP])
	| ss_table_set_val : forall (z : state) (i : num_) (v_ref : ref) (x : idx), 
		((proj_num__0 i) != None) ->
		(((!((proj_num__0 i))) :> N) <? (|(REFS (fun_table z x))|))%BN ->
		state_step (mk_config z [::(admininstr_CONST I32 i); (admininstr_ref v_ref); (admininstr_TABLE_SET x)]) (mk_config (with_table z x ((!((proj_num__0 i))) :> N) v_ref) [:: ])
	| ss_table_grow_succeed : forall (z : state) (v_ref : ref) (v_n : n) (x : idx) (ti : tableinst) (var_0 : (option tableinst)), 
		(fun_growtable (fun_table z x) v_n v_ref var_0) ->
		(var_0 != None) ->
		((!(var_0)) == ti) ->
		state_step (mk_config z [::(admininstr_ref v_ref); (admininstr_CONST I32 (mk_num__0 Inn_I32 (mk_uN v_n))); (admininstr_TABLE_GROW x)]) (mk_config (with_tableinst z x ti) [::(admininstr_CONST I32 (mk_num__0 Inn_I32 (mk_uN (|(REFS (fun_table z x))|))))])
	| ss_table_grow_fail : forall (z : state) (v_ref : ref) (v_n : n) (x : idx) (var_0 : N), 
		(fun_inv_signed_ 32%N (0 - (1%N : Z))%Z var_0) ->
		state_step (mk_config z [::(admininstr_ref v_ref); (admininstr_CONST I32 (mk_num__0 Inn_I32 (mk_uN v_n))); (admininstr_TABLE_GROW x)]) (mk_config z [::(admininstr_CONST I32 (mk_num__0 Inn_I32 (mk_uN var_0)))])
	| ss_Step__elem_drop : forall (z : state) (x : idx), state_step (mk_config z [::(admininstr_ELEM_DROP x)]) (mk_config (with_elem z x [:: ]) [:: ])
	| ss_store_num_trap : forall (z : state) (i : num_) (nt : numtype) (c : num_) (ao : memarg), 
		((proj_num__0 i) != None) ->
		((res_size (valtype_numtype nt)) != None) ->
		(((((!((proj_num__0 i))) :> N) + ((OFFSET ao) :> N))%BN + ((((!((res_size (valtype_numtype nt)))) : Q) / (8%N : Q))%Q : N))%BN >? (|(BYTES (fun_mem z (mk_uN 0%N)))|))%BN ->
		(wf_uN 32%N (mk_uN 0%N)) ->
		state_step (mk_config z [::(admininstr_CONST I32 i); (admininstr_CONST nt c); (admininstr_STORE nt None ao)]) (mk_config z [::admininstr_TRAP])
	| ss_store_num_val : forall (z : state) (i : num_) (nt : numtype) (c : num_) (ao : memarg) (b_lst : (seq byte)), 
		((proj_num__0 i) != None) ->
		((res_size (valtype_numtype nt)) != None) ->
		(b_lst == (nbytes_ nt c)) ->
		state_step (mk_config z [::(admininstr_CONST I32 i); (admininstr_CONST nt c); (admininstr_STORE nt None ao)]) (mk_config (with_mem z (mk_uN 0%N) (((!((proj_num__0 i))) :> N) + ((OFFSET ao) :> N))%BN ((((!((res_size (valtype_numtype nt)))) : Q) / (8%N : Q))%Q : N) b_lst) [:: ])
	| ss_store_pack_trap : forall (z : state) (i : num_) (v_Inn : Inn) (c : num_) (v_n : n) (ao : memarg), 
		((proj_num__0 i) != None) ->
		(((((!((proj_num__0 i))) :> N) + ((OFFSET ao) :> N))%BN + (((v_n : Q) / (8%N : Q))%Q : N))%BN >? (|(BYTES (fun_mem z (mk_uN 0%N)))|))%BN ->
		(wf_uN 32%N (mk_uN 0%N)) ->
		state_step (mk_config z [::(admininstr_CONST I32 i); (admininstr_CONST (numtype_Inn v_Inn) c); (admininstr_STORE (numtype_Inn v_Inn) (Some (mk_sz v_n)) ao)]) (mk_config z [::admininstr_TRAP])
	| ss_store_pack_val : forall (z : state) (i : num_) (v_Inn : Inn) (c : num_) (v_n : n) (ao : memarg) (b_lst : (seq byte)), 
		((proj_num__0 i) != None) ->
		((res_size (valtype_Inn v_Inn)) != None) ->
		((proj_num__0 c) != None) ->
		(b_lst == (ibytes_ v_n (wrap__ (!((res_size (valtype_Inn v_Inn)))) v_n (!((proj_num__0 c)))))) ->
		state_step (mk_config z [::(admininstr_CONST I32 i); (admininstr_CONST (numtype_Inn v_Inn) c); (admininstr_STORE (numtype_Inn v_Inn) (Some (mk_sz v_n)) ao)]) (mk_config (with_mem z (mk_uN 0%N) (((!((proj_num__0 i))) :> N) + ((OFFSET ao) :> N))%BN (((v_n : Q) / (8%N : Q))%Q : N) b_lst) [:: ])
	| ss_vstore_oob : forall (z : state) (i : num_) (c : vec_) (ao : memarg), 
		((proj_num__0 i) != None) ->
		((res_size valtype_V128) != None) ->
		(((((!((proj_num__0 i))) :> N) + ((OFFSET ao) :> N))%BN + ((((!((res_size valtype_V128))) : Q) / (8%N : Q))%Q : N))%BN >? (|(BYTES (fun_mem z (mk_uN 0%N)))|))%BN ->
		(wf_uN 32%N (mk_uN 0%N)) ->
		state_step (mk_config z [::(admininstr_CONST I32 i); (admininstr_VCONST V128 c); (admininstr_VSTORE V128 ao)]) (mk_config z [::admininstr_TRAP])
	| ss_vstore_val : forall (z : state) (i : num_) (c : vec_) (ao : memarg) (b_lst : (seq byte)), 
		((proj_num__0 i) != None) ->
		((res_size valtype_V128) != None) ->
		(b_lst == (vbytes_ V128 c)) ->
		state_step (mk_config z [::(admininstr_CONST I32 i); (admininstr_VCONST V128 c); (admininstr_VSTORE V128 ao)]) (mk_config (with_mem z (mk_uN 0%N) (((!((proj_num__0 i))) :> N) + ((OFFSET ao) :> N))%BN ((((!((res_size valtype_V128))) : Q) / (8%N : Q))%Q : N) b_lst) [:: ])
	| ss_vstore_lane_oob : forall (z : state) (i : num_) (c : vec_) (v_N : res_N) (ao : memarg) (j : laneidx), 
		((proj_num__0 i) != None) ->
		(((((!((proj_num__0 i))) :> N) + ((OFFSET ao) :> N))%BN + v_N)%BN >? (|(BYTES (fun_mem z (mk_uN 0%N)))|))%BN ->
		(wf_uN 32%N (mk_uN 0%N)) ->
		state_step (mk_config z [::(admininstr_CONST I32 i); (admininstr_VCONST V128 c); (admininstr_VSTORE_LANE V128 (mk_sz v_N) ao j)]) (mk_config z [::admininstr_TRAP])
	| ss_vstore_lane_val : forall (z : state) (i : num_) (c : vec_) (v_N : res_N) (ao : memarg) (j : laneidx) (b_lst : (seq byte)) (v_Jnn : Jnn) (v_M : M), 
		((proj_num__0 i) != None) ->
		(v_N == (jsize v_Jnn)) ->
		((v_M : Q) == ((128%N : Q) / (v_N : Q))%Q)%Q ->
		((proj_lane__0 ((lanes_ (X (lanetype_Jnn v_Jnn) (mk_dim v_M)) c)[| (j :> N) |])) != None) ->
		((j :> N) <? (|(lanes_ (X (lanetype_Jnn v_Jnn) (mk_dim v_M)) c)|))%BN ->
		(b_lst == (ibytes_ v_N (mk_uN ((!((proj_lane__0 ((lanes_ (X (lanetype_Jnn v_Jnn) (mk_dim v_M)) c)[| (j :> N) |])))) :> (N))))) ->
		(wf_uN v_N (mk_uN ((!((proj_lane__0 ((lanes_ (X (lanetype_Jnn v_Jnn) (mk_dim v_M)) c)[| (j :> N) |])))) :> (N)))) ->
		state_step (mk_config z [::(admininstr_CONST I32 i); (admininstr_VCONST V128 c); (admininstr_VSTORE_LANE V128 (mk_sz v_N) ao j)]) (mk_config (with_mem z (mk_uN 0%N) (((!((proj_num__0 i))) :> N) + ((OFFSET ao) :> N))%BN (((v_N : Q) / (8%N : Q))%Q : N) b_lst) [:: ])
	| ss_memory_grow_succeed : forall (z : state) (v_n : n) (mi : meminst) (var_0 : (option meminst)), 
		(fun_growmemory (fun_mem z (mk_uN 0%N)) v_n var_0) ->
		(var_0 != None) ->
		((!(var_0)) == mi) ->
		(wf_uN 32%N (mk_uN 0%N)) ->
		state_step (mk_config z [::(admininstr_CONST I32 (mk_num__0 Inn_I32 (mk_uN v_n))); admininstr_MEMORY_GROW]) (mk_config (with_meminst z (mk_uN 0%N) mi) [::(admininstr_CONST I32 (mk_num__0 Inn_I32 (mk_uN ((((|(BYTES (fun_mem z (mk_uN 0%N)))|) : Q) / ((64%N * (Ki ))%BN : Q))%Q : N))))])
	| ss_memory_grow_fail : forall (z : state) (v_n : n) (var_0 : N), 
		(fun_inv_signed_ 32%N (0 - (1%N : Z))%Z var_0) ->
		state_step (mk_config z [::(admininstr_CONST I32 (mk_num__0 Inn_I32 (mk_uN v_n))); admininstr_MEMORY_GROW]) (mk_config z [::(admininstr_CONST I32 (mk_num__0 Inn_I32 (mk_uN var_0)))])
	| ss_Step__data_drop : forall (z : state) (x : idx), state_step (mk_config z [::(admininstr_DATA_DROP x)]) (mk_config (with_data z x [:: ]) [:: ]).

Lemma state_step_step : forall c c', state_step c c' -> Step c c'.
Proof. move=> c c'; case=> *; first [ eapply Step__local_set; eassumption | eapply Step__global_set; eassumption | eapply table_set_trap; eassumption | eapply table_set_val; eassumption | eapply table_grow_succeed; eassumption | eapply table_grow_fail; eassumption | eapply Step__elem_drop; eassumption | eapply store_num_trap; eassumption | eapply store_num_val; eassumption | eapply store_pack_trap; eassumption | eapply store_pack_val; eassumption | eapply vstore_oob; eassumption | eapply vstore_val; eassumption | eapply vstore_lane_oob; eassumption | eapply vstore_lane_val; eassumption | eapply memory_grow_succeed; eassumption | eapply memory_grow_fail; eassumption | eapply Step__data_drop; eassumption ]. Qed.

Ltac to_state_step := first [ eapply ss_Step__local_set; eassumption | eapply ss_Step__global_set; eassumption | eapply ss_table_set_trap; eassumption | eapply ss_table_set_val; eassumption | eapply ss_table_grow_succeed; eassumption | eapply ss_table_grow_fail; eassumption | eapply ss_Step__elem_drop; eassumption | eapply ss_store_num_trap; eassumption | eapply ss_store_num_val; eassumption | eapply ss_store_pack_trap; eassumption | eapply ss_store_pack_val; eassumption | eapply ss_vstore_oob; eassumption | eapply ss_vstore_val; eassumption | eapply ss_vstore_lane_oob; eassumption | eapply ss_vstore_lane_val; eassumption | eapply ss_memory_grow_succeed; eassumption | eapply ss_memory_grow_fail; eassumption | eapply ss_Step__data_drop; eassumption ].

Lemma state_lhs : forall z es c', state_step (mk_config z es) c' -> redex_lhs es.
Proof.
  move=> z es c' H. remember (mk_config z es) as c eqn:Hc.
  case: H Hc => *; match goal with H : mk_config _ _ = mk_config _ _ |- _ => case: H => *; subst end;
    rewrite /redex_lhs; lhs_done.
Qed.

(* A step of a rule's left-hand side itself, rather than of part of a list. *)
Inductive base_step : config -> config -> Prop :=
  | base_pure : forall z es es', Step_pure es es' -> base_step (mk_config z es) (mk_config z es')
  | base_read : forall z es es', Step_read (mk_config z es) es' ->
      base_step (mk_config z es) (mk_config z es')
  | base_label : forall z n l es z' es', Step (mk_config z es) (mk_config z' es') ->
      wf_config (mk_config z es) -> wf_config (mk_config z' es') ->
      base_step (mk_config z [:: LABEL_ n l es]) (mk_config z' [:: LABEL_ n l es'])
  | base_frame : forall s f n f' es s' f'' es',
      Step (mk_config (mk_state s f') es) (mk_config (mk_state s' f'') es') ->
      wf_config (mk_config (mk_state s f') es) -> wf_config (mk_config (mk_state s' f'') es') ->
      base_step (mk_config (mk_state s f) [:: FRAME_ n f' es])
                (mk_config (mk_state s' f) [:: FRAME_ n f'' es'])
  | base_state : forall c c', state_step c c' -> base_step c c'.

Lemma base_step_step : forall c c', base_step c c' -> Step c c'.
Proof.
  move=> c c'; case=> *.
  - exact: pure.
  - exact: read.
  - exact: ctxt_label.
  - exact: ctxt_frame.
  - exact: state_step_step.
Qed.

(* Every step happens at the first non-value instruction `h`: it rewrites `h`
   and some of the values right before it. A trap is the exception, since it
   can discard instructions after it. *)
Definition at_head (z : state) (es : seq admininstr) (z' : state) (es' : seq admininstr) : Prop :=
  match (split_vals es).2 with
  | [::] => False
  | h :: r => h <> admininstr_TRAP ->
      exists vs1 vs2 H', (split_vals es).1 = vs1 ++ vs2 /\
        es' = map admininstr_val vs1 ++ H' ++ r /\
        base_step (mk_config z (map admininstr_val vs2 ++ [:: h])) (mk_config z' H')
  end.

Lemma at_head_base : forall z es z' es', redex_lhs es ->
  base_step (mk_config z es) (mk_config z' es') -> at_head z es z' es'.
Proof.
  move=> z es z' es'. rewrite /redex_lhs /at_head.
  case Hs: (split_vals es) => [vs r] /=. case: r Hs => [ | h r] Hs //= Hcase Hstep Hh.
  case: Hcase => [Hr | Ht]; last by case: Hh.
  subst r. exists [::], vs, es'. split; [done | split; [by rewrite cats0 | ]].
  have Hes : es = map admininstr_val vs ++ [:: h] by rewrite {1}(split_vals_spec es) Hs.
  by rewrite -Hes.
Qed.

Definition at_head_config (c c' : config) : Prop :=
  match c, c' with mk_config z es, mk_config z' es' => at_head z es z' es' end.

Lemma step_at_head_config : forall c c', Step c c' -> at_head_config c c'.
Proof.
  move=> c c'. elim.
  - move=> z es es' Hp. exact: at_head_base (pure_lhs _ _ Hp) (base_pure _ _ _ Hp).
  - move=> z es es' Hr. exact: at_head_base (read_lhs _ _ _ Hr) (base_read _ _ _ Hr).
  - move=> z n l es z' es' Hin _ Hwf Hwf'. rewrite /at_head_config /at_head /= => _.
    exists [::], [::], [:: LABEL_ n l es']. split; [done | split; [done | ]].
    exact: base_label.
  - move=> s f n f' es s' f'' es' Hin _ Hwf Hwf'. rewrite /at_head_config /at_head /= => _.
    exists [::], [::], [:: FRAME_ n f'' es']. split; [done | split; [done | ]].
    exact: base_frame.
  - move=> z vals es_in es_suf z' es_in' Hin IH _ _ _. rewrite /at_head_config in IH.
    rewrite /at_head_config /at_head /at_head in IH *.
    case Hsp: (split_vals es_in) IH => [ws r_in] /=.
    case: r_in Hsp => [ | h r_in] Hsp // IH.
    have Hne : (split_vals es_in).2 <> [::] by rewrite Hsp.
    rewrite split_vals_map_cat (split_vals_cat _ _ Hne) Hsp /= => Hh.
    have [vs1 [vs2 [H' [Hws [Hes' Hstep]]]]] := IH Hh.
    exists (vals ++ vs1), vs2, H'. split; [by rewrite Hws catA | split; [ | done]].
    by rewrite Hes' map_cat -!catA.
  all: move=> *; apply: at_head_base; [rewrite /redex_lhs; lhs_done | apply: base_state; to_state_step].
Qed.

Lemma step_at_head : forall z es z' es',
  Step (mk_config z es) (mk_config z' es') -> at_head z es z' es'.
Proof. move=> z es z' es' H. exact: (step_at_head_config _ _ H). Qed.

Lemma vals_no_step : forall z vs c', ~ Step (mk_config z (map admininstr_val vs)) c'.
Proof.
  move=> z vs [z' es'] H. have := step_at_head _ _ _ _ H.
  by rewrite /at_head -(cats0 (map admininstr_val vs)) split_vals_map_cat.
Qed.

(* Inverting a step of one particular instruction. *)

Ltac kill_lhs :=
  match goal with
  | H : _ = map admininstr_val _ ++ [:: _] |- _ =>
      move/(f_equal split_vals): H; rewrite split_vals_nonval //; split_vals_simpl; try congruence
  | H : map admininstr_val _ ++ [:: _] = _ |- _ =>
      move/(f_equal split_vals): H; rewrite split_vals_nonval //; split_vals_simpl; try congruence
  end.

Lemma base_tee : forall z vs x z' es',
  base_step (mk_config z (map admininstr_val vs ++ [:: admininstr_LOCAL_TEE x])) (mk_config z' es') ->
  exists v, vs = [:: v] /\ z' = z /\
    es' = [:: admininstr_val v; admininstr_val v; admininstr_LOCAL_SET x].
Proof.
  move=> z vs x z' es' H. inversion H as [? ? ? Hp | ? ? ? Hr | | | ? ? Hs]; subst.
  - inversion Hp; subst; kill_lhs. by case=> <- <-; exists v_val.
  - inversion Hr; subst; kill_lhs.
  - kill_lhs.
  - kill_lhs.
  - inversion Hs; subst; kill_lhs.
Qed.

Lemma base_set : forall z vs x z' es',
  base_step (mk_config z (map admininstr_val vs ++ [:: admininstr_LOCAL_SET x])) (mk_config z' es') ->
  exists v, vs = [:: v] /\ z' = with_local z x v /\ es' = [::].
Proof.
  move=> z vs x z' es' H. inversion H as [? ? ? Hp | ? ? ? Hr | | | ? ? Hs]; subst.
  - inversion Hp; subst; kill_lhs.
  - inversion Hr; subst; kill_lhs.
  - kill_lhs.
  - kill_lhs.
  - inversion Hs; subst; kill_lhs. by case=> <- <-; exists v_val.
Qed.

Lemma step_tee : forall z us x r z' es',
  Step (mk_config z (map admininstr_val us ++ admininstr_LOCAL_TEE x :: r)) (mk_config z' es') ->
  exists us' v, us = us' ++ [:: v] /\ z' = z /\
    es' = map admininstr_val us' ++ [:: admininstr_val v; admininstr_val v; admininstr_LOCAL_SET x] ++ r.
Proof.
  move=> z us x r z' es' H. have := step_at_head _ _ _ _ H.
  rewrite /at_head split_vals_nonval //= => /(_ ltac:(done)) [vs1 [vs2 [H' [Hus [Hes' Hb]]]]].
  have [v [Hvs2 [Hz HH']]] := base_tee _ _ _ _ _ Hb. subst.
  by exists vs1, v.
Qed.

Lemma step_set : forall z us x r z' es',
  Step (mk_config z (map admininstr_val us ++ admininstr_LOCAL_SET x :: r)) (mk_config z' es') ->
  exists us' v, us = us' ++ [:: v] /\ z' = with_local z x v /\
    es' = map admininstr_val us' ++ r.
Proof.
  move=> z us x r z' es' H. have := step_at_head _ _ _ _ H.
  rewrite /at_head split_vals_nonval //= => /(_ ltac:(done)) [vs1 [vs2 [H' [Hus [Hes' Hb]]]]].
  have [v [Hvs2 [Hz HH']]] := base_set _ _ _ _ _ Hb. subst.
  by exists vs1, v.
Qed.

(* Once a trap reaches the front, steps only discard what is around it. *)

Definition trap_headed (es : seq admininstr) : Prop :=
  exists r, (split_vals es).2 = admininstr_TRAP :: r.

Definition trap_step_config (c c' : config) : Prop :=
  match c, c' with
  | mk_config z es, mk_config z' es' => trap_headed es -> z' = z /\ trap_headed es'
  end.

Lemma step_trap_config : forall c c', Step c c' -> trap_step_config c c'.
Proof.
  move=> c c'; elim.
  - move=> z es es' Hp [r Hr]. split; first done.
    inversion Hp; subst; move: Hr; split_vals_simpl; move=> Hr; try congruence.
    by exists [::].
  - move=> z es es' Hrd [r Hr]. inversion Hrd; subst; move: Hr; split_vals_simpl; congruence.
  - by move=> z n l es z' es' _ _ _ _ [r Hr].
  - by move=> s f n f' es s' f'' es' _ _ _ _ [r Hr].
  - move=> z vals es_in es_suf z' es_in' Hin IH _ _ _ [r Hr].
    case Hsp: (split_vals es_in) => [ws r_in].
    case: r_in Hsp => [ | h r_in] Hsp.
    + exfalso. move: Hin. rewrite (split_vals_spec es_in) Hsp cats0 /=. exact: vals_no_step.
    + have Hne : (split_vals es_in).2 <> [::] by rewrite Hsp.
      move: Hr. rewrite split_vals_map_cat (split_vals_cat _ _ Hne) Hsp /= => [[Hh Hr]]. subst h.
      have Ht : trap_headed es_in by exists r_in; rewrite Hsp.
      have [-> [r' Hr']] := IH Ht.
      split; first done. exists (r' ++ es_suf).
      have Hne' : (split_vals es_in').2 <> [::] by rewrite Hr'.
      by rewrite split_vals_map_cat (split_vals_cat _ _ Hne') Hr'.
  all: intros; rewrite /trap_step_config; case=> r Hr; move: Hr; split_vals_simpl; congruence.
Qed.

Lemma step_trap : forall z es z' es', trap_headed es ->
  Step (mk_config z es) (mk_config z' es') -> z' = z /\ trap_headed es'.
Proof. move=> z es z' es' Ht H. exact: (step_trap_config _ _ H Ht). Qed.

(* Steps keep the number of locals in the current frame. *)

Definition frame_of (z : state) : frame := let: mk_state _ f := z in f.

Lemma size_list_update : forall (A : Type) (l : seq A) n g, size (list_update_func l n g) = size l.
Proof.
  move=> A. elim=> [ | x l IH] n g //=. by case: n => [ | p] //=; rewrite IH.
Qed.

Definition locals_size_config (c c' : config) : Prop :=
  match c, c' with
  | mk_config z _, mk_config z' _ => size (LOCALS (frame_of z')) = size (LOCALS (frame_of z))
  end.

Lemma step_locals_size_config : forall c c', Step c c' -> locals_size_config c c'.
Proof.
  move=> c c'; elim=> //=.
  all: intros; repeat match goal with z : state |- _ => destruct z end;
    rewrite /= ?size_list_update //.
Qed.

Lemma step_locals_size : forall z es z' es', Step (mk_config z es) (mk_config z' es') ->
  size (LOCALS (frame_of z')) = size (LOCALS (frame_of z)).
Proof. move=> z es z' es' H. exact: (step_locals_size_config _ _ H). Qed.
