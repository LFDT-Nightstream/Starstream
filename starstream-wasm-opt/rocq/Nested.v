(* The local-cleanup rules applied anywhere in a program: in function bodies,
   inside blocks, loops and ifs, and so at every depth of labels and frames
   at run time. The original and rewritten programs have different stores,
   since their function bodies differ. *)
From Stdlib Require Import List NArith ZArith QArith QArith.Qround Lia.
From mathcomp Require Import ssreflect ssrfun ssrbool eqtype seq.
From RecordUpdate Require Import RecordSet.
Require Import Wasm2 LocalRewrites StepInversion.

Local Notation vals vs := (map admininstr_val vs).
Local Notation instrs b := (map admininstr_instr b).

(* `ilr k P Q`: a rule replaces `P` with `Q`, in a function with `k` locals
   counting its parameters. *)
Inductive ilr (k : nat) : seq instr -> seq instr -> Prop :=
  | ilr_set_get : forall x, (N.to_nat (x :> N) < k)%nat ->
      ilr k [:: LOCAL_SET x; LOCAL_GET x] [:: LOCAL_TEE x]
  | ilr_tee_drop : forall x, ilr k [:: LOCAL_TEE x; DROP] [:: LOCAL_SET x]
  | ilr_get_drop : forall y, ilr k [:: LOCAL_GET y; DROP] [::]
  | ilr_global_get_drop : forall y, ilr k [:: GLOBAL_GET y; DROP] [::]
  | ilr_const_drop : forall t c, ilr k [:: CONST t c; DROP] [::].

(* Function bodies, equal apart from places where a rule replaced `P` with
   `Q`, at any depth. *)
Inductive icong (k : nat) : seq instr -> seq instr -> Prop :=
  | icong_nil : icong k [::] [::]
  | icong_cons : forall i1 i2 b1 b2, ielem k i1 i2 -> icong k b1 b2 ->
      icong k (i1 :: b1) (i2 :: b2)
  | icong_rule : forall P Q b1 b2, ilr k P Q -> icong k b1 b2 -> icong k (P ++ b1) (Q ++ b2)
with ielem (k : nat) : instr -> instr -> Prop :=
  | ielem_same : forall i, ielem k i i
  | ielem_block : forall bt b1 b2, icong k b1 b2 -> ielem k (BLOCK bt b1) (BLOCK bt b2)
  | ielem_loop : forall bt b1 b2, icong k b1 b2 -> ielem k (LOOP bt b1) (LOOP bt b2)
  | ielem_if : forall bt b1 b2 c1 c2, icong k b1 b2 -> icong k c1 c2 ->
      ielem k (IFELSE bt b1 c1) (IFELSE bt b2 c2).

(* Running configurations: the same, at any depth of labels and frames,
   with the frame's number of locals as `k`. *)
Inductive acong : nat -> seq admininstr -> seq admininstr -> Prop :=
  | acong_nil : forall k, acong k [::] [::]
  | acong_cons : forall k e1 e2 es1 es2, aelem k e1 e2 -> acong k es1 es2 ->
      acong k (e1 :: es1) (e2 :: es2)
  | acong_rule : forall k P Q es1 es2, ilr k P Q -> acong k es1 es2 ->
      acong k (instrs P ++ es1) (instrs Q ++ es2)
  (* Midway through rule 1: the rewrite has run `local.tee x` and the original
     has yet to run `local.set x; local.get x`. *)
  | acong_tee_mid : forall k x v es1 es2, (N.to_nat (x :> N) < k)%nat -> acong k es1 es2 ->
      acong k [:: admininstr_val v, admininstr_LOCAL_SET x, admininstr_LOCAL_GET x & es1]
              [:: admininstr_val v, admininstr_val v, admininstr_LOCAL_SET x & es2]
with aelem : nat -> admininstr -> admininstr -> Prop :=
  | aelem_same : forall k e, aelem k e e
  | aelem_block : forall k bt b1 b2, icong k b1 b2 ->
      aelem k (admininstr_BLOCK bt b1) (admininstr_BLOCK bt b2)
  | aelem_loop : forall k bt b1 b2, icong k b1 b2 ->
      aelem k (admininstr_LOOP bt b1) (admininstr_LOOP bt b2)
  | aelem_if : forall k bt b1 b2 c1 c2, icong k b1 b2 -> icong k c1 c2 ->
      aelem k (admininstr_IFELSE bt b1 c1) (admininstr_IFELSE bt b2 c2)
  | aelem_label : forall k n c1 c2 es1 es2, icong k c1 c2 -> arel k es1 es2 ->
      aelem k (LABEL_ n c1 es1) (LABEL_ n c2 es2)
  | aelem_frame : forall k n f es1 es2, arel (size (LOCALS f)) es1 es2 ->
      aelem k (FRAME_ n f es1) (FRAME_ n f es2)
(* Related instructions, or a trap at the front of both. *)
with arel : nat -> seq admininstr -> seq admininstr -> Prop :=
  | arel_cong : forall k es1 es2, acong k es1 es2 -> arel k es1 es2
  | arel_trap : forall k es1 es2, trap_headed es1 -> trap_headed es2 -> arel k es1 es2.

Lemma ielem_aelem : forall k i1 i2, ielem k i1 i2 ->
  aelem k (admininstr_instr i1) (admininstr_instr i2).
Proof. move=> k i1 i2; case=> *; by constructor. Qed.

Lemma icong_acong : forall k b1 b2, icong k b1 b2 -> acong k (instrs b1) (instrs b2).
Proof.
  move=> k b1 b2; elim=> {b1 b2} [ | i1 i2 b1 b2 Hi _ IH | P Q b1 b2 Hlr _ IH].
  - exact: acong_nil.
  - exact: acong_cons (ielem_aelem _ _ _ Hi) IH.
  - rewrite !map_cat. exact: acong_rule.
Qed.

Lemma acong_refl : forall k es, acong k es es.
Proof. move=> k; elim=> [ | e es IH]; [exact: acong_nil | exact: acong_cons (aelem_same _ _) IH]. Qed.

Lemma acong_prefix : forall k l es1 es2, acong k es1 es2 -> acong k (l ++ es1) (l ++ es2).
Proof. move=> k; elim=> [ | e l IH] es1 es2 H //=. exact: acong_cons (aelem_same _ _) (IH _ _ H). Qed.

Lemma acong_cat : forall k l1 l2 es1 es2, acong k l1 l2 -> acong k es1 es2 ->
  acong k (l1 ++ es1) (l2 ++ es2).
Proof.
  move=> k l1 l2 es1 es2 H; elim: H es1 es2 => {k l1 l2}.
  - by move=> k es1 es2 H.
  - move=> k e1 e2 l1 l2 He _ IH es1 es2 H /=. exact: acong_cons He (IH _ _ H).
  - move=> k P Q l1 l2 Hlr _ IH es1 es2 H. rewrite -!catA. exact: acong_rule Hlr (IH _ _ H).
  - move=> k x v l1 l2 Hx _ IH es1 es2 H /=. exact: acong_tee_mid Hx (IH _ _ H).
Qed.

(* Stores: the same apart from function bodies, which are related. *)

Definition code_rel (fi1 fi2 : funcinst) : Prop :=
  funcinst_TYPE fi1 = funcinst_TYPE fi2 /\ funcinst_MODULE fi1 = funcinst_MODULE fi2 /\
  exists x ls b1 b2, CODE fi1 = func_FUNC x ls b1 /\ CODE fi2 = func_FUNC x ls b2 /\
    forall t1 t2, funcinst_TYPE fi1 = mk_functype (mk_list _ t1) (mk_list _ t2) ->
      icong (size t1 + size ls) b1 b2.

Definition other_fields (s : store) :=
  (store_GLOBALS s, store_TABLES s, store_MEMS s, store_ELEMS s, store_DATAS s).

(* `s1` is the original store: its functions are well formed, which the
   rewritten store cannot vouch for. *)
Definition srel (s1 s2 : store) : Prop :=
  other_fields s1 = other_fields s2 /\ List.Forall2 code_rel (store_FUNCS s1) (store_FUNCS s2) /\
  List.Forall wf_funcinst (store_FUNCS s1).

Definition zrel (z1 z2 : state) : Prop :=
  match z1, z2 with mk_state s1 f1, mk_state s2 f2 => f1 = f2 /\ srel s1 s2 end.

Lemma zrel_frame : forall z1 z2, zrel z1 z2 -> frame_of z1 = frame_of z2.
Proof. by move=> [s1 f1] [s2 f2] []. Qed.

Ltac zrel_cases Hz :=
  match type of Hz with zrel ?z1 ?z2 =>
    let s1 := fresh "s1" in let f1 := fresh "f1" in
    let s2 := fresh "s2" in let f2 := fresh "f2" in
    let Hf := fresh "Hf" in let Hof := fresh "Hof" in
    let Hfn := fresh "Hfn" in let Hwf := fresh "Hwf" in
    destruct z1 as [s1 f1]; destruct z2 as [s2 f2];
    destruct Hz as [Hf [Hof [Hfn Hwf]]]; subst f2;
    destruct s1, s2; rewrite /other_fields /= in Hof; case: Hof => ? ? ? ? ?; subst
  end.

(* Everything but function bodies reads the same in related states. *)

Lemma zrel_globalinst : forall z1 z2, zrel z1 z2 -> fun_globalinst z2 = fun_globalinst z1.
Proof. move=> z1 z2 Hz; by zrel_cases Hz. Qed.

Lemma zrel_tableinst : forall z1 z2, zrel z1 z2 -> fun_tableinst z2 = fun_tableinst z1.
Proof. move=> z1 z2 Hz; by zrel_cases Hz. Qed.

Lemma zrel_meminst : forall z1 z2, zrel z1 z2 -> fun_meminst z2 = fun_meminst z1.
Proof. move=> z1 z2 Hz; by zrel_cases Hz. Qed.

Lemma zrel_eleminst : forall z1 z2, zrel z1 z2 -> fun_eleminst z2 = fun_eleminst z1.
Proof. move=> z1 z2 Hz; by zrel_cases Hz. Qed.

Lemma zrel_datainst : forall z1 z2, zrel z1 z2 -> fun_datainst z2 = fun_datainst z1.
Proof. move=> z1 z2 Hz; by zrel_cases Hz. Qed.

Lemma zrel_global : forall z1 z2 x, zrel z1 z2 -> fun_global z2 x = fun_global z1 x.
Proof. move=> z1 z2 x Hz; by zrel_cases Hz. Qed.

Lemma zrel_table : forall z1 z2 x, zrel z1 z2 -> fun_table z2 x = fun_table z1 x.
Proof. move=> z1 z2 x Hz; by zrel_cases Hz. Qed.

Lemma zrel_mem : forall z1 z2 x, zrel z1 z2 -> fun_mem z2 x = fun_mem z1 x.
Proof. move=> z1 z2 x Hz; by zrel_cases Hz. Qed.

Lemma zrel_elem : forall z1 z2 x, zrel z1 z2 -> fun_elem z2 x = fun_elem z1 x.
Proof. move=> z1 z2 x Hz; by zrel_cases Hz. Qed.

Lemma zrel_data : forall z1 z2 x, zrel z1 z2 -> fun_data z2 x = fun_data z1 x.
Proof. move=> z1 z2 x Hz; by zrel_cases Hz. Qed.

Lemma zrel_local : forall z1 z2 x, zrel z1 z2 -> fun_local z2 x = fun_local z1 x.
Proof. move=> z1 z2 x Hz; by zrel_cases Hz. Qed.

Lemma zrel_type : forall z1 z2 x, zrel z1 z2 -> fun_type z2 x = fun_type z1 x.
Proof. move=> z1 z2 x Hz; by zrel_cases Hz. Qed.

Lemma zrel_funcaddr : forall z1 z2, zrel z1 z2 -> fun_funcaddr z2 = fun_funcaddr z1.
Proof. move=> z1 z2 Hz; by zrel_cases Hz. Qed.

Lemma zrel_blocktype : forall z1 z2 bt, zrel z1 z2 -> fun_blocktype z2 bt = fun_blocktype z1 bt.
Proof. move=> z1 z2 [[ | ] | x] Hz //=. exact: zrel_type. Qed.


(* Store updates keep states related. *)

Ltac zrel_update := move=> *;
  match goal with Hz : zrel _ _ |- _ => zrel_cases Hz end;
  rewrite /zrel /srel /other_fields /=; by repeat split.

Lemma zrel_with_local : forall z1 z2 x v, zrel z1 z2 -> zrel (with_local z1 x v) (with_local z2 x v).
Proof. zrel_update. Qed.
Lemma zrel_with_global : forall z1 z2 x v, zrel z1 z2 -> zrel (with_global z1 x v) (with_global z2 x v).
Proof. zrel_update. Qed.
Lemma zrel_with_table : forall z1 z2 x i r, zrel z1 z2 -> zrel (with_table z1 x i r) (with_table z2 x i r).
Proof. zrel_update. Qed.
Lemma zrel_with_tableinst : forall z1 z2 x ti, zrel z1 z2 ->
  zrel (with_tableinst z1 x ti) (with_tableinst z2 x ti).
Proof. zrel_update. Qed.
Lemma zrel_with_mem : forall z1 z2 x i j bs, zrel z1 z2 ->
  zrel (with_mem z1 x i j bs) (with_mem z2 x i j bs).
Proof. zrel_update. Qed.
Lemma zrel_with_meminst : forall z1 z2 x mi, zrel z1 z2 ->
  zrel (with_meminst z1 x mi) (with_meminst z2 x mi).
Proof. zrel_update. Qed.
Lemma zrel_with_elem : forall z1 z2 x rs, zrel z1 z2 -> zrel (with_elem z1 x rs) (with_elem z2 x rs).
Proof. zrel_update. Qed.
Lemma zrel_with_data : forall z1 z2 x bs, zrel z1 z2 -> zrel (with_data z1 x bs) (with_data z2 x bs).
Proof. zrel_update. Qed.

Lemma zrel_wf : forall z1 z2, zrel z1 z2 -> wf_state z2 -> wf_state z1.
Proof.
  move=> z1 z2 Hz. zrel_cases Hz. move=> H. inversion H as [? ? Hs Hf]; subst.
  inversion Hs; subst. by constructor; [constructor | ].
Qed.

(* Rules other than calls read only what related states agree on, so a step
   of the rewritten state is a step of the original. *)

Lemma read_transport : forall z1 z2 es es', zrel z1 z2 -> Step_read (mk_config z2 es) es' ->
  Step_read (mk_config z1 es) es' \/
  (exists vs a, es = vals vs ++ [:: CALL_ADDR a]) \/
  (exists i x y, es = [:: admininstr_CONST I32 i; admininstr_CALL_INDIRECT x y]).
Proof.
  move=> z1 z2 es es' Hz H. zrel_cases Hz.
  inversion H; subst;
    try (by right; left; do 2 eexists);
    try (by right; right; do 3 eexists);
    left; cbn [fun_local fun_global fun_table fun_mem fun_elem fun_data fun_globalinst
      fun_tableinst fun_meminst fun_eleminst fun_datainst fun_funcaddr fun_type
      store_GLOBALS store_TABLES store_MEMS store_ELEMS store_DATAS] in *; first [ eapply Step_read__block; eassumption | eapply Step_read__loop; eassumption | eapply Step_read__call; eassumption | eapply Step_read__ref_func; eassumption | eapply Step_read__local_get; eassumption | eapply Step_read__global_get; eassumption | eapply table_get_trap; eassumption | eapply table_get_val; eassumption | eapply Step_read__table_size; eassumption | eapply table_fill_trap; eassumption | eapply table_fill_zero; eassumption | eapply table_fill_succ; eassumption | eapply table_copy_trap; eassumption | eapply table_copy_zero; eassumption | eapply table_copy_le; eassumption | eapply table_copy_gt; eassumption | eapply table_init_trap; eassumption | eapply table_init_zero; eassumption | eapply table_init_succ; eassumption | eapply load_num_trap; eassumption | eapply load_num_val; eassumption | eapply load_pack_trap; eassumption | eapply load_pack_val; eassumption | eapply vload_oob; eassumption | eapply Step_read__vload_val; eassumption | eapply vload_shape_oob; eassumption | eapply vload_shape_val; eassumption | eapply vload_splat_oob; eassumption | eapply vload_splat_val; eassumption | eapply vload_zero_oob; eassumption | eapply vload_zero_val; eassumption | eapply vload_lane_oob; eassumption | eapply vload_lane_val; eassumption | eapply Step_read__memory_size; eassumption | eapply memory_fill_trap; eassumption | eapply memory_fill_zero; eassumption | eapply memory_fill_succ; eassumption | eapply memory_copy_trap; eassumption | eapply memory_copy_zero; eassumption | eapply memory_copy_le; eassumption | eapply memory_copy_gt; eassumption | eapply memory_init_trap; eassumption | eapply memory_init_zero; eassumption | eapply memory_init_succ; eassumption ].
Qed.

Ltac store_cbn := cbn [fun_local fun_global fun_table fun_mem fun_elem fun_data fun_globalinst
  fun_tableinst fun_meminst fun_eleminst fun_datainst fun_funcaddr fun_type
  store_GLOBALS store_TABLES store_MEMS store_ELEMS store_DATAS] in *.

Lemma state_transport : forall z1 z2 es z2' es', zrel z1 z2 ->
  state_step (mk_config z2 es) (mk_config z2' es') ->
  exists z1', state_step (mk_config z1 es) (mk_config z1' es') /\ zrel z1' z2'.
Proof.
  move=> z1 z2 es z2' es' Hz H. zrel_cases Hz.
  inversion H; subst; eexists; (split;
    [ store_cbn; to_state_step
    | try first [ apply: zrel_with_local | apply: zrel_with_global | apply: zrel_with_table
                | apply: zrel_with_tableinst | apply: zrel_with_mem | apply: zrel_with_meminst
                | apply: zrel_with_elem | apply: zrel_with_data ];
      rewrite /zrel /srel /other_fields /=; by repeat split ]).
Qed.

(* Calls read function instances, whose types and modules agree. *)

Lemma Forall2_size : forall (A B : Type) (R : A -> B -> Prop) l1 l2,
  List.Forall2 R l1 l2 -> size l1 = size l2.
Proof. move=> A B R l1 l2; elim=> //= *; congruence. Qed.

Lemma Forall2_nth : forall (A B : Type) (R : A -> B -> Prop) (d1 : A) (d2 : B) l1 l2 n,
  List.Forall2 R l1 l2 -> (n < size l1)%nat -> R (seq.nth d1 l1 n) (seq.nth d2 l2 n).
Proof.
  move=> A B R d1 d2 l1 l2 n H. elim: H n => [ | x y l1' l2' Hxy _ IH] n /= Hn; first lia.
  case: n Hn => [ | n] Hn //=. apply: IH. lia.
Qed.

Lemma Forall2_nth_type : forall l1 l2 n, List.Forall2 code_rel l1 l2 ->
  funcinst_TYPE (seq.nth default_val l1 n) = funcinst_TYPE (seq.nth default_val l2 n).
Proof.
  move=> l1 l2 n H. elim: H n => //= x y l1' l2' [Ht _] _ IH [ | n] //=.
Qed.

Lemma zrel_funcinst_size : forall z1 z2, zrel z1 z2 -> size (fun_funcinst z1) = size (fun_funcinst z2).
Proof. move=> [s1 f1] [s2 f2] [_ [_ [H _]]] /=. exact: Forall2_size H. Qed.

Lemma zrel_funcinst_type : forall z1 z2 a, zrel z1 z2 ->
  funcinst_TYPE ((fun_funcinst z1)[| a |]) = funcinst_TYPE ((fun_funcinst z2)[| a |]).
Proof. move=> [s1 f1] [s2 f2] a [_ [_ [H _]]] /=. exact: Forall2_nth_type. Qed.

Lemma call_indirect_before : forall z1 z2 i x y, zrel z1 z2 ->
  Step_read_before_call_indirect_trap
    (mk_config z1 [:: admininstr_CONST I32 i; admininstr_CALL_INDIRECT x y]) ->
  Step_read_before_call_indirect_trap
    (mk_config z2 [:: admininstr_CONST I32 i; admininstr_CALL_INDIRECT x y]).
Proof.
  move=> z1 z2 i x y Hz H. inversion H; subst.
  apply: call_indirect_call_0; rewrite ?(zrel_table _ _ _ Hz) ?(zrel_type _ _ _ Hz)
    -?(zrel_funcinst_size _ _ Hz) -?(zrel_funcinst_type _ _ _ Hz) //; eassumption.
Qed.

(* Refute an equation between rule left-hand sides with different last
   instructions. *)
Ltac lhs_clash := match goal with
  | H : @eq (seq admininstr) _ _ |- _ => move/(f_equal split_vals): H; split_vals_simpl; congruence
  end.

Lemma call_indirect_transport : forall z1 z2 i x y es', zrel z1 z2 ->
  Step_read (mk_config z2 [:: admininstr_CONST I32 i; admininstr_CALL_INDIRECT x y]) es' ->
  Step_read (mk_config z1 [:: admininstr_CONST I32 i; admininstr_CALL_INDIRECT x y]) es'.
Proof.
  move=> z1 z2 i x y es' Hz H. inversion H; subst; try lhs_clash.
  - apply: call_indirect_call; rewrite -?(zrel_table _ _ _ Hz) -?(zrel_type _ _ _ Hz)
      ?(zrel_funcinst_size _ _ Hz) ?(zrel_funcinst_type _ _ _ Hz) //; eassumption.
  - apply: call_indirect_trap => Hb. by apply: H5; exact: call_indirect_before Hb.
Qed.

(* Entering a function: the original enters its own body, related to the
   rewritten one. *)

Lemma call_addr_inv : forall z vs a es', Step_read (mk_config z (vals vs ++ [:: CALL_ADDR a])) es' ->
  exists n f t1 t2 mm x t_lst body,
    (a <? (| fun_funcinst z |))%BN /\
    (fun_funcinst z)[| a |] = {| funcinst_TYPE := mk_functype (mk_list _ t1) (mk_list _ t2);
                                funcinst_MODULE := mm;
                                CODE := func_FUNC x (map LOCAL t_lst) body |} /\
    List.Forall (fun t => default_ t != None) t_lst /\
    f = {| LOCALS := vs ++ map (fun t => !(default_ t)) t_lst; frame_MODULE := mm |} /\
    wf_frame f /\ size vs = size t1 /\ n = (| t2 |) /\
    es' = [:: FRAME_ n f [:: LABEL_ n [::] (instrs body)]].
Proof.
  move=> z vs a es' H. inversion H; subst; try lhs_clash.
  match goal with Heq : @eq _ (cat (seq.map _ _) [:: CALL_ADDR _]) _ |- _ =>
    move/(f_equal split_vals): Heq; split_vals_simpl; case=> Hvs Ha; subst end.
  rewrite !cats0 in Hvs. subst val_lst.
  exists v_n, f, t_1_lst, t_2_lst, mm, x, t_lst, instr_lst.
  move/eqP: H3 => H3. move/eqP: H4 => H4. move/eqP: H6 => H6.
  move/eqP: H10 => H10. move/eqP: H11 => H11. move/eqP: H13 => H13.
  subst f v_func. do 5 (split; first done). split; first lia. by split.
Qed.

Lemma zrel_funcinst_rel : forall z1 z2 a, zrel z1 z2 ->
  (N.to_nat a < size (fun_funcinst z2))%nat ->
  code_rel ((fun_funcinst z1)[| a |]) ((fun_funcinst z2)[| a |]).
Proof.
  move=> [s1 f1] [s2 f2] a [_ [_ [Hfn _]]] /= Hin. rewrite /lookup_total.
  apply: (Forall2_nth _ _ _ _ _ _ _ _ Hfn). by rewrite (Forall2_size _ _ _ _ _ Hfn).
Qed.

Lemma zrel_funcinst_wf : forall z1 z2 a, zrel z1 z2 ->
  (N.to_nat a < size (fun_funcinst z1))%nat -> wf_funcinst ((fun_funcinst z1)[| a |]).
Proof.
  move=> [s1 f1] [s2 f2] a [_ [_ [_ Hwf]]] /= Hin. rewrite /lookup_total.
  elim: Hwf (N.to_nat a) Hin => [ | fi l Hfi _ IH] [ | m] //= Hin; try lia. apply: IH. lia.
Qed.

Lemma instr_admin_wf : forall i, wf_instr i -> wf_admininstr (admininstr_instr i).
Proof. move=> i H; destruct H; cbn; solve [constructor | econstructor; eassumption]. Qed.

Lemma instrs_wf : forall b, List.Forall wf_instr b -> List.Forall wf_admininstr (instrs b).
Proof. elim=> [ | i b IH] H //=. inversion H; subst. constructor; [exact: instr_admin_wf | exact: IH]. Qed.

Lemma call_addr_sim : forall z1 z2 vs a es2', zrel z1 z2 -> wf_state z1 ->
  Step_read (mk_config z2 (vals vs ++ [:: CALL_ADDR a])) es2' ->
  exists es1', Step_read (mk_config z1 (vals vs ++ [:: CALL_ADDR a])) es1' /\
    wf_config (mk_config z1 es1') /\ forall k, acong k es1' es2'.
Proof.
  move=> z1 z2 vs a es2' Hz Hz1 H.
  have [n [f [t1 [t2 [mm [x [t_lst [body2 [Ha [Hfi2 [Hdef [Hf [Hwff [Hsize [Hn ->]]]]]]]]]]]]]]] :=
    call_addr_inv _ _ _ _ H.
  have Hin2 : (N.to_nat a < size (fun_funcinst z2))%nat by move: Ha => /N.ltb_lt; lia.
  have Hin1 : (N.to_nat a < size (fun_funcinst z1))%nat by rewrite (zrel_funcinst_size _ _ Hz).
  have Hrel := zrel_funcinst_rel _ _ _ Hz Hin2.
  have Hwfi := zrel_funcinst_wf _ _ _ Hz Hin1.
  rewrite Hfi2 in Hrel.
  case Efi1: ((fun_funcinst z1)[| a |]) Hrel Hwfi => [ty md cd] /=.
  case=> Hty [Hmd [x' [ls [b1 [b2 [Hc1 [[Hx Hls Hb] Hicong]]]]]]] Hwfi.
  rewrite /= in Hty Hmd Hc1 Hicong. subst ty md cd x' ls b2.
  have Hicong' := Hicong _ _ erefl.
  exists [:: FRAME_ n f [:: LABEL_ n [::] (instrs b1)]]. split; [ | split].
  - rewrite Hf. apply: (call_addr z1 (N.of_nat (size vs)) vs a n _ b1 t1 t2 mm
      (func_FUNC x (map LOCAL t_lst) b1) x t_lst) => //.
    + by rewrite (zrel_funcinst_size _ _ Hz).
    + by rewrite Efi1.
    + by inversion Hwfi.
    + by rewrite -Hf.
    + by apply/eqP; rewrite Hsize.
    + by apply/eqP.
  - apply: config_case_0 => //. constructor; last constructor. apply: admininstr_case_72 => //.
    constructor; last constructor. apply: admininstr_case_71; first constructor. apply: instrs_wf.
    inversion Hwfi as [? ? ? _ Hfunc]; subst. by inversion Hfunc.
  - move=> k. apply: acong_cons (acong_nil _). apply: aelem_frame. apply: arel_cong.
    apply: acong_cons (acong_nil _). apply: aelem_label; first exact: icong_nil.
    apply: arel_cong. apply: icong_acong. rewrite size_map in Hicong'.
    by rewrite Hf /= size_cat size_map Hsize.
Qed.

(* Well-formedness bookkeeping. *)

Lemma admin_val_wf_inv : forall v, wf_admininstr (admininstr_val v) -> wf_val v.
Proof. by case=> > H; inversion H; subst; constructor. Qed.

Lemma Forall_cat_inv : forall (A : Type) (P : A -> Prop) (s1 s2 : seq A),
  List.Forall P (s1 ++ s2) -> List.Forall P s1 /\ List.Forall P s2.
Proof.
  move=> A P. elim=> [ | x s1 IH] s2 H //=. inversion H as [ | ? ? Hx Hs]; subst.
  have [Hs1 Hs2] := IH _ Hs. by split; first constructor.
Qed.

Lemma vals_wf_inv : forall vs, List.Forall wf_admininstr (vals vs) -> List.Forall wf_val vs.
Proof.
  elim=> [ | v vs IH] H //=. inversion H; subst.
  constructor; [exact: admin_val_wf_inv | exact: IH].
Qed.

Lemma wf_code : forall z es, wf_config (mk_config z es) -> List.Forall wf_admininstr es.
Proof. by move=> z es H; inversion H. Qed.

Lemma wf_state_of : forall z es, wf_config (mk_config z es) -> wf_state z.
Proof. by move=> z es H; inversion H. Qed.

Lemma wf_with_code : forall z es es', wf_config (mk_config z es) ->
  List.Forall wf_admininstr es' -> wf_config (mk_config z es').
Proof. move=> z es es' H Hes'. apply: config_case_0 => //. exact: wf_state_of H. Qed.

Lemma lift : forall z code z' code' vs rest,
  Steps (mk_config z code) (mk_config z' code') ->
  List.Forall wf_val vs -> List.Forall wf_admininstr rest ->
  Steps (mk_config z (vals vs ++ code ++ rest)) (mk_config z' (vals vs ++ code' ++ rest)).
Proof. move=> z code z' code' vs rest H. exact: steps_in_context H. Qed.

Lemma trap_collapse : forall z es, wf_config (mk_config z es) -> trap_headed es ->
  Steps (mk_config z es) (mk_config z [:: admininstr_TRAP]).
Proof.
  move=> z es Hwf [r Hr].
  have Hes : es = vals (split_vals es).1 ++ [:: admininstr_TRAP] ++ r.
  { by rewrite {1}(split_vals_spec es) Hr. }
  have Htrap : wf_config (mk_config z [:: admininstr_TRAP]).
  { apply: (wf_with_code _ _ _ Hwf). by repeat constructor. }
  case Hempty: (((split_vals es).1 != [::]) || (r != [::])).
  - rewrite Hes. apply: steps_one; [ | by rewrite -Hes | done].
    exact: pure (trap_vals _ _ Hempty).
  - move/negbT: Hempty. rewrite negb_or !negbK => /andP [/eqP Hvs /eqP Hr0].
    rewrite Hes Hvs Hr0 /=. exact: Steps__refl.
Qed.

(* Runs inside a label or a frame. *)

Lemma steps_in_label : forall c c', Steps c c' ->
  forall n l, List.Forall wf_instr l ->
  match c, c' with
  | mk_config z es, mk_config z' es' =>
      Steps (mk_config z [:: LABEL_ n l es]) (mk_config z' [:: LABEL_ n l es'])
  end.
Proof.
  have Hwf : forall z es n l, List.Forall wf_instr l -> wf_config (mk_config z es) ->
      wf_config (mk_config z [:: LABEL_ n l es]).
  { move=> z es n l Hl H. apply: (wf_with_code _ _ _ H). repeat constructor => //. exact: wf_code H. }
  move=> c c'; elim=> [z es H | z es z'' es'' z' es' Hstep _ IH Ha Hc Hb] n l Hl.
  - apply: Steps__refl. exact: Hwf.
  - apply: (trans _ _ _ _ _ _ (ctxt_label _ _ _ _ _ _ Hstep Ha Hb)); try exact: Hwf.
    exact: IH.
Qed.

Lemma steps_in_frame : forall c c', Steps c c' ->
  forall n f, wf_frame f ->
  match c, c' with
  | mk_config (mk_state s f') es, mk_config (mk_state s' f'') es' =>
      Steps (mk_config (mk_state s f) [:: FRAME_ n f' es])
            (mk_config (mk_state s' f) [:: FRAME_ n f'' es'])
  end.
Proof.
  have Hwf : forall s f' es n f, wf_frame f -> wf_config (mk_config (mk_state s f') es) ->
      wf_config (mk_config (mk_state s f) [:: FRAME_ n f' es]).
  { move=> s f' es n f Hf H. inversion H as [? ? Hz Hes]; subst. inversion Hz; subst.
    apply: config_case_0; first by constructor. by repeat constructor. }
  move=> c c'; elim=> [[s f'] es H | [s f'] es [s'' f'''] es'' [s' f''] es' Hstep _ IH Ha Hc Hb] n f Hf.
  - apply: Steps__refl. exact: Hwf.
  - apply: (trans _ _ _ _ _ _ (ctxt_frame _ _ _ _ _ _ _ _ Hstep Ha Hb)); try exact: Hwf.
    exact: IH.
Qed.

(* Related elements, by the rewritten one's shape. *)

Lemma icong_refl : forall k b, icong k b b.
Proof. move=> k; elim=> [ | i b IH]; [exact: icong_nil | exact: icong_cons (ielem_same _ _) IH]. Qed.

Lemma arel_refl : forall k es, arel k es es.
Proof. move=> k es. exact: arel_cong (acong_refl _ _). Qed.

Lemma aelem_label_inv : forall k e1 n c2 es2, aelem k e1 (LABEL_ n c2 es2) ->
  exists c1 es1, e1 = LABEL_ n c1 es1 /\ icong k c1 c2 /\ arel k es1 es2.
Proof.
  move=> k e1 n c2 es2 H. inversion H; subst.
  - exists c2, es2. by split; [ | split; [exact: icong_refl | exact: arel_refl]].
  - by exists c1, es1.
Qed.

Lemma aelem_frame_inv : forall k e1 n f es2, aelem k e1 (FRAME_ n f es2) ->
  exists es1, e1 = FRAME_ n f es1 /\ arel (size (LOCALS f)) es1 es2.
Proof.
  move=> k e1 n f es2 H. inversion H; subst.
  - exists es2. by split; last exact: arel_refl.
  - by exists es1.
Qed.

Lemma aelem_block_inv : forall k e1 bt b2, aelem k e1 (admininstr_BLOCK bt b2) ->
  exists b1, e1 = admininstr_BLOCK bt b1 /\ icong k b1 b2.
Proof.
  move=> k e1 bt b2 H. inversion H; subst; [exists b2; split; [done | exact: icong_refl] | by exists b1].
Qed.

Lemma aelem_loop_inv : forall k e1 bt b2, aelem k e1 (admininstr_LOOP bt b2) ->
  exists b1, e1 = admininstr_LOOP bt b1 /\ icong k b1 b2.
Proof.
  move=> k e1 bt b2 H. inversion H; subst; [exists b2; split; [done | exact: icong_refl] | by exists b1].
Qed.

Lemma aelem_if_inv : forall k e1 bt b2 c2, aelem k e1 (admininstr_IFELSE bt b2 c2) ->
  exists b1 c1, e1 = admininstr_IFELSE bt b1 c1 /\ icong k b1 b2 /\ icong k c1 c2.
Proof.
  move=> k e1 bt b2 c2 H. inversion H; subst.
  - exists b2, c2. by split; [ | split; exact: icong_refl].
  - by exists b1, c1.
Qed.

(* Elements with no code in them are related only to themselves. *)
Definition has_code (e : admininstr) : bool :=
  match e with
  | admininstr_BLOCK _ _ | admininstr_LOOP _ _ | admininstr_IFELSE _ _ _
  | LABEL_ _ _ _ | FRAME_ _ _ _ => true
  | _ => false
  end.

Lemma aelem_plain : forall k e1 e2, aelem k e1 e2 -> ~~ has_code e2 -> e1 = e2.
Proof. move=> k e1 e2; by case. Qed.

(* Junk the rewrite deleted: the original runs it away without changing the
   state. *)
Lemma junk_steps : forall k P z, ilr k P [::] -> wf_config (mk_config z (instrs P)) ->
  Steps (mk_config z (instrs P)) (mk_config z [::]).
Proof.
  move=> k P [s f] Hlr Hwf. have Hz := wf_state_of _ _ Hwf. have HP := wf_code _ _ Hwf.
  inversion Hlr; subst; inversion HP as [ | ? ? Hi]; subst.
  - inversion Hi; subst. exact: get_drop.
  - inversion Hi; subst. exact: global_get_drop.
  - inversion Hi; subst. exact: const_drop.
Qed.

(* Before the rewrite's next step, the original runs any junk ahead of it.
   Then it reaches an instruction related to the rewrite's next one, or the
   start of a rule. *)
Definition caught (k : nat) (us : seq val) (h2 : admininstr) (r2 es1c : seq admininstr) : Prop :=
  (exists h1 r1, es1c = vals us ++ h1 :: r1 /\ aelem k h1 h2 /\ acong k r1 r2)
  \/ (exists x r1, h2 = admininstr_LOCAL_TEE x /\ (N.to_nat (x :> N) < k)%nat /\
        es1c = vals us ++ [:: admininstr_LOCAL_SET x, admininstr_LOCAL_GET x & r1] /\ acong k r1 r2)
  \/ (exists x r1, h2 = admininstr_LOCAL_SET x /\
        es1c = vals us ++ [:: admininstr_LOCAL_TEE x, admininstr_DROP & r1] /\ acong k r1 r2)
  \/ (exists x us0 v r1, h2 = admininstr_LOCAL_SET x /\ (N.to_nat (x :> N) < k)%nat /\
        us = us0 ++ [:: v; v] /\
        es1c = vals (us0 ++ [:: v]) ++ [:: admininstr_LOCAL_SET x, admininstr_LOCAL_GET x & r1] /\
        acong k r1 r2).

Lemma caught_cons : forall k u us h r2 es1c,
  caught k us h r2 es1c -> caught k (u :: us) h r2 (admininstr_val u :: es1c).
Proof.
  move=> k u us h r2 es1c [[h1 [r1 [-> [Hh Hc]]]] | [[x [r1 [Hh [Hx [-> Hc]]]]] |
    [[x [r1 [Hh [-> Hc]]]] | [x [us0 [v [r1 [Hh [Hx [Hus [-> Hc]]]]]]]]]]].
  - left. by exists h1, r1.
  - right; left. by exists x, r1.
  - right; right; left. by exists x, r1.
  - right; right; right. exists x, (u :: us0), v, r1. by rewrite Hus.
Qed.

Lemma catch_up : forall k es1 es2, acong k es1 es2 ->
  forall z us h r2, wf_config (mk_config z es1) ->
  es2 = vals us ++ h :: r2 -> to_val h = None ->
  exists es1c, Steps (mk_config z es1) (mk_config z es1c) /\ caught k us h r2 es1c.
Proof.
  move=> k es1 es2; elim.
  - by move=> k0 z [ | u us] h r2.
  - move=> k0 e1 e2 es1' es2' He Hc IH z [ | u us] h r2 Hwf /= [He2 Hes2] Hh.
    + subst e2 es2'. exists (e1 :: es1'). split; first exact: Steps__refl.
      left. by exists e1, es1'.
    + subst e2. have He1 : e1 = admininstr_val u by apply: (aelem_plain _ _ _ He); destruct u.
      subst e1. have /List.Forall_cons_iff [Hu Hrest] := wf_code _ _ Hwf.
      have [es1c [Hs Hcaught]] := IH z us h r2 (wf_with_code _ _ _ Hwf Hrest) Hes2 Hh.
      exists (admininstr_val u :: es1c). split; last exact: caught_cons.
      have Hu' : List.Forall wf_val [:: u] by constructor; [exact: admin_val_wf_inv | ].
      have := lift _ _ _ _ [:: u] [::] Hs Hu' (List.Forall_nil _).
      by rewrite /= !cats0.
  - move=> k0 P Q es1' es2' Hlr Hc IH z us h r2 Hwf Hes2 Hh.
    have Hsplit := split_vals_nonval us _ r2 Hh. rewrite -Hes2 in Hsplit.
    have [HP Hrest] := Forall_cat_inv _ _ _ _ (wf_code _ _ Hwf).
    have Hlr' := Hlr. move: Hes2 Hsplit. inversion Hlr'; subst; move=> Hes2 Hsplit.
    3-5: have Hjunk := junk_steps _ _ _ Hlr (wf_with_code _ _ _ Hwf HP);
      have [es1c [Hs Hcaught]] := IH z us h r2 (wf_with_code _ _ _ Hwf Hrest) Hes2 Hh;
      exists es1c; split; last done;
      apply: (steps_trans _ _ _ _ Hs);
      by have := lift _ _ _ _ [::] es1' Hjunk (List.Forall_nil _) Hrest.
    + move: Hsplit => /= [<- <- <-].
      exists ([:: admininstr_LOCAL_SET x; admininstr_LOCAL_GET x] ++ es1').
      split; first exact: Steps__refl. right; left. by exists x, es1'.
    + move: Hsplit => /= [<- <- <-].
      exists ([:: admininstr_LOCAL_TEE x; admininstr_DROP] ++ es1').
      split; first exact: Steps__refl. right; right; left. by exists x, es1'.
  - move=> k0 x v es1' es2' Hx Hc IH z us h r2 Hwf Hes2 Hh.
    have := split_vals_nonval us _ r2 Hh. rewrite -Hes2. split_vals_simpl. case=> <- <- <-.
    exists [:: admininstr_val v, admininstr_LOCAL_SET x, admininstr_LOCAL_GET x & es1'].
    split; first exact: Steps__refl.
    right; right; right. by exists x, [::], v, es1'.
Qed.

Lemma catch_up_vals : forall k es1 es2, acong k es1 es2 ->
  forall z vs, wf_config (mk_config z es1) -> es2 = vals vs ->
  Steps (mk_config z es1) (mk_config z (vals vs)).
Proof.
  move=> k es1 es2; elim.
  - move=> k0 z [ | v vs] Hwf // _. exact: Steps__refl.
  - move=> k0 e1 e2 es1' es2' He Hc IH z [ | v vs] Hwf //= [He2 Hes2]. subst e2.
    have He1 : e1 = admininstr_val v by apply: (aelem_plain _ _ _ He); destruct v.
    subst e1. have /List.Forall_cons_iff [Hv Hrest] := wf_code _ _ Hwf.
    have Hs := IH z vs (wf_with_code _ _ _ Hwf Hrest) Hes2.
    have Hv' : List.Forall wf_val [:: v] by constructor; [exact: admin_val_wf_inv | ].
    have := lift _ _ _ _ [:: v] [::] Hs Hv' (List.Forall_nil _).
    by rewrite /= !cats0.
  - move=> k0 P Q es1' es2' Hlr Hc IH z vs Hwf Hes2.
    have [HP Hrest] := Forall_cat_inv _ _ _ _ (wf_code _ _ Hwf).
    have Hlr' := Hlr. move: Hes2. inversion Hlr'; subst; move=> Hes2.
    1-2: move/(f_equal split_vals): Hes2; rewrite -(cats0 (vals vs)) split_vals_map_cat /=;
      congruence.
    1-3: apply: (steps_trans _ _ _
           (lift _ _ _ _ [::] es1' (junk_steps _ _ _ Hlr (wf_with_code _ _ _ Hwf HP))
              (List.Forall_nil _) Hrest));
         exact: (IH z vs (wf_with_code _ _ _ Hwf Hrest) Hes2).
  - move=> k0 x v es1' es2' _ _ _ z vs _ Hes2.
    move/(f_equal split_vals): Hes2. rewrite -(cats0 (vals vs)) split_vals_map_cat.
    split_vals_simpl. congruence.
Qed.

(* Steps of instructions holding code, which the original takes with its own
   code. *)

Ltac lhs_match H := move/(f_equal split_vals): H; split_vals_simpl; rewrite ?cats0.

Lemma base_block_inv : forall z ws bt b z' H',
  base_step (mk_config z (vals ws ++ [:: admininstr_BLOCK bt b])) (mk_config z' H') ->
  z' = z /\ exists n, H' = [:: LABEL_ n [::] (vals ws ++ instrs b)] /\
    forall b', Step_read (mk_config z (vals ws ++ [:: admininstr_BLOCK bt b']))
                         [:: LABEL_ n [::] (vals ws ++ instrs b')].
Proof.
  move=> z ws bt b z' H' H. inversion H as [? ? ? Hp | ? ? ? Hr | | | ? ? Hs]; subst.
  - inversion Hp; subst; lhs_clash.
  - inversion Hr; subst; try lhs_clash.
    match goal with Heq : @eq _ (cat (seq.map _ _) [:: admininstr_BLOCK _ _]) _ |- _ =>
      lhs_match Heq; case=> ? ? ?; subst end.
    split; first done. exists v_n. split; first done. move=> b'. by econstructor; eassumption.
  - lhs_clash.
  - lhs_clash.
  - inversion Hs; subst; lhs_clash.
Qed.

Lemma base_loop_inv : forall z ws bt b z' H',
  base_step (mk_config z (vals ws ++ [:: admininstr_LOOP bt b])) (mk_config z' H') ->
  z' = z /\ exists n, H' = [:: LABEL_ n [:: LOOP bt b] (vals ws ++ instrs b)] /\
    forall b', Step_read (mk_config z (vals ws ++ [:: admininstr_LOOP bt b']))
                         [:: LABEL_ n [:: LOOP bt b'] (vals ws ++ instrs b')].
Proof.
  move=> z ws bt b z' H' H. inversion H as [? ? ? Hp | ? ? ? Hr | | | ? ? Hs]; subst.
  - inversion Hp; subst; lhs_clash.
  - inversion Hr; subst; try lhs_clash.
    match goal with Heq : @eq _ (cat (seq.map _ _) [:: admininstr_LOOP _ _]) _ |- _ =>
      lhs_match Heq; case=> ? ? ?; subst end.
    split; first done. exists k. split; first done. move=> b'. by econstructor; eassumption.
  - lhs_clash.
  - lhs_clash.
  - inversion Hs; subst; lhs_clash.
Qed.

Lemma base_if_inv : forall z ws bt b c z' H',
  base_step (mk_config z (vals ws ++ [:: admininstr_IFELSE bt b c])) (mk_config z' H') ->
  z' = z /\ exists cnd, ws = [:: val_CONST I32 cnd] /\
    ((H' = [:: admininstr_BLOCK bt b] /\ forall b' c',
        Step_pure [:: admininstr_CONST I32 cnd; admininstr_IFELSE bt b' c'] [:: admininstr_BLOCK bt b'])
     \/ (H' = [:: admininstr_BLOCK bt c] /\ forall b' c',
        Step_pure [:: admininstr_CONST I32 cnd; admininstr_IFELSE bt b' c'] [:: admininstr_BLOCK bt c'])).
Proof.
  move=> z ws bt b c z' H' H. inversion H as [? ? ? Hp | ? ? ? Hr | | | ? ? Hs]; subst.
  - inversion Hp; subst; try lhs_clash.
    + match goal with Heq : @eq _ [:: admininstr_CONST _ _; admininstr_IFELSE _ _ _] _ |- _ =>
        lhs_match Heq; case=> ? ? ? ?; subst end.
      split; first done. eexists. split; first done. left. split; first done.
      by move=> b' c'; apply: if_true.
    + match goal with Heq : @eq _ [:: admininstr_CONST _ _; admininstr_IFELSE _ _ _] _ |- _ =>
        lhs_match Heq; case=> ? ? ? ?; subst end.
      split; first done. eexists. split; first done. right. split; first done.
      by move=> b' c'; apply: if_false.
  - inversion Hr; subst; lhs_clash.
  - lhs_clash.
  - lhs_clash.
  - inversion Hs; subst; lhs_clash.
Qed.

Lemma base_label_inv : forall z ws n c es z' H',
  base_step (mk_config z (vals ws ++ [:: LABEL_ n c es])) (mk_config z' H') ->
  ws = [::] /\
  ( (exists es', Step (mk_config z es) (mk_config z' es') /\ wf_config (mk_config z es) /\
       wf_config (mk_config z' es') /\ H' = [:: LABEL_ n c es'])
  \/ (z' = z /\ exists vs, es = vals vs /\ H' = vals vs)
  \/ (z' = z /\ exists vs' vs rest,
        es = ((vals vs' ++ vals vs) ++ [:: admininstr_BR (mk_uN 0)]) ++ rest /\
        (n == (| vs |)) /\ H' = vals vs ++ instrs c)
  \/ (z' = z /\ exists vs l rest,
        es = (vals vs ++ [:: admininstr_BR (mk_uN ((l :> N) + 1)%BN)]) ++ rest /\
        H' = vals vs ++ [:: admininstr_BR l])
  \/ (z' = z /\ exists vs rest,
        es = (vals vs ++ [:: admininstr_RETURN]) ++ rest /\ H' = vals vs ++ [:: admininstr_RETURN])
  \/ (z' = z /\ es = [:: admininstr_TRAP] /\ H' = [:: admininstr_TRAP])).
Proof.
  move=> z ws n c es z' H' H. inversion H as [? ? ? Hp | ? ? ? Hr | | | ? ? Hs]; subst.
  - inversion Hp; subst; try lhs_clash;
      match goal with Heq : @eq _ [:: LABEL_ _ _ _] _ |- _ =>
        lhs_match Heq; case=> ? ? ? ?; subst end; split=> //.
    + right; left. split=> //. by eexists.
    + right; right; left. split=> //. by do 3 eexists.
    + right; right; right; left. split=> //. by do 3 eexists.
    + right; right; right; right; left. split=> //. by do 2 eexists.
    + by right; right; right; right; right.
  - inversion Hr; subst; lhs_clash.
  - match goal with Heq : @eq _ [:: LABEL_ _ _ _] _ |- _ =>
      lhs_match Heq; case=> ? ? ? ?; subst end.
    split=> //. left. eexists.
    split; [eassumption | split; [eassumption | split; [eassumption | reflexivity]]].
  - lhs_clash.
  - inversion Hs; subst; lhs_clash.
Qed.

Lemma base_frame_inv : forall s f ws n f' es z' H',
  base_step (mk_config (mk_state s f) (vals ws ++ [:: FRAME_ n f' es])) (mk_config z' H') ->
  ws = [::] /\
  ( (exists s' f'' es', Step (mk_config (mk_state s f') es) (mk_config (mk_state s' f'') es') /\
       wf_config (mk_config (mk_state s f') es) /\ wf_config (mk_config (mk_state s' f'') es') /\
       z' = mk_state s' f /\ H' = [:: FRAME_ n f'' es'])
  \/ (z' = mk_state s f /\ exists vs, es = vals vs /\ (n == (| vs |)) /\ H' = vals vs)
  \/ (z' = mk_state s f /\ exists vs' vs rest,
        es = ((vals vs' ++ vals vs) ++ [:: admininstr_RETURN]) ++ rest /\
        (n == (| vs |)) /\ H' = vals vs)
  \/ (z' = mk_state s f /\ es = [:: admininstr_TRAP] /\ H' = [:: admininstr_TRAP])).
Proof.
  move=> s f ws n f' es z' H' H. inversion H as [? ? ? Hp | ? ? ? Hr | | | ? ? Hs]; subst.
  - inversion Hp; subst; try lhs_clash;
      match goal with Heq : @eq _ [:: FRAME_ _ _ _] _ |- _ =>
        lhs_match Heq; case=> ? ? ? ?; subst end; split=> //.
    + right; left. split=> //. by eexists.
    + right; right; left. split=> //. by do 3 eexists.
    + by right; right; right.
  - inversion Hr; subst; lhs_clash.
  - lhs_clash.
  - match goal with Heq : @eq _ [:: FRAME_ _ _ _] _ |- _ =>
      lhs_match Heq; case=> ? ? ? ?; subst end.
    split=> //. left. do 3 eexists.
    split; [eassumption | split; [eassumption | split; [eassumption | split; reflexivity]]].
  - inversion Hs; subst; lhs_clash.
Qed.

(* How deeply labels and frames nest, for induction. *)
Fixpoint adepth (e : admininstr) : nat :=
  match e with
  | LABEL_ _ _ es | FRAME_ _ _ es => S (foldr (fun e' m => Nat.max (adepth e') m) 0%nat es)
  | _ => 0%nat
  end.

Definition ldepth (es : seq admininstr) : nat := foldr (fun e m => Nat.max (adepth e) m) 0%nat es.

Lemma ldepth_cat_cons : forall l1 e l2, (adepth e <= ldepth (l1 ++ e :: l2))%nat.
Proof. elim=> [ | a l1 IH] e l2 /=; rewrite /ldepth /=; [lia | ]. move: (IH e l2). rewrite /ldepth. lia. Qed.

(* Catching up to an instruction with no code in it, or to a trap. *)

Lemma catch_up_plain : forall k z es1 us h r2, arel k es1 (vals us ++ h :: r2) ->
  wf_config (mk_config z es1) -> to_val h = None -> ~~ has_code h -> h <> admininstr_TRAP ->
  (forall x, h <> admininstr_LOCAL_TEE x) -> (forall x, h <> admininstr_LOCAL_SET x) ->
  exists r1, Steps (mk_config z es1) (mk_config z (vals us ++ h :: r1)) /\ acong k r1 r2.
Proof.
  move=> k z es1 us h r2 Hr Hwf Hh Hcode Htrap Htee Hset. inversion Hr as [? ? ? Hc | ? ? ? _ [r Ht]]; subst.
  - have [es1c [Hs Hcaught]] := catch_up _ _ _ Hc z us h r2 Hwf erefl Hh.
    case: Hcaught => [[h1 [r1 [Hes1c [He Hc1]]]] | [[x [? [Hx _]]] | [[x [? [Hx _]]] | [x [? [? [? [Hx _]]]]]]]].
    + rewrite Hes1c (aelem_plain _ _ _ He Hcode) in Hs. by exists r1.
    + by case: (Htee x).
    + by case: (Hset x).
    + by case: (Hset x).
  - move: Ht. rewrite split_vals_nonval //=. case=> Hh' _. by case: Htrap.
Qed.

Lemma catch_up_trap : forall k z es1 es2, arel k es1 es2 -> trap_headed es2 ->
  wf_config (mk_config z es1) ->
  exists es1c, Steps (mk_config z es1) (mk_config z es1c) /\ trap_headed es1c.
Proof.
  move=> k z es1 es2 Hr [r Ht] Hwf. inversion Hr as [? ? ? Hc | ? ? ? Ht1 _]; subst.
  - have Hes2 : es2 = vals (split_vals es2).1 ++ admininstr_TRAP :: r.
    { by rewrite {1}(split_vals_spec es2) Ht. }
    have [es1c [Hs Hcaught]] := catch_up _ _ _ Hc z _ _ _ Hwf Hes2 erefl.
    exists es1c. split; first done.
    case: Hcaught => [[h1 [r1 [-> [He _]]]] | [[x [? [? _]]] | [[x [? [? _]]] | [x [? [? [? [? _]]]]]]]] //.
    rewrite (aelem_plain _ _ _ He erefl). exists r1. by rewrite split_vals_nonval.
  - exists es1. split; [exact: Steps__refl | done].
Qed.

(* A step at an instruction with no code in it: the original takes the same
   step in its own state. *)
Lemma plain_head_sim : forall z1 z2 ws h z2' H2', zrel z1 z2 ->
  wf_config (mk_config z1 (vals ws ++ [:: h])) -> wf_config (mk_config z2' H2') -> ~~ has_code h ->
  base_step (mk_config z2 (vals ws ++ [:: h])) (mk_config z2' H2') ->
  exists z1' H1', Steps (mk_config z1 (vals ws ++ [:: h])) (mk_config z1' H1') /\ zrel z1' z2' /\
    forall k, acong k H1' H2'.
Proof.
  move=> z1 z2 ws h z2' H2' Hz Hwf1 Hwf2' Hcode Hb.
  have Hz1 := wf_state_of _ _ Hwf1.
  have Hwf_same : forall z1', zrel z1' z2' -> wf_config (mk_config z1' H2').
  { move=> z1' Hz'. apply: config_case_0; [exact: zrel_wf Hz' (wf_state_of _ _ Hwf2') | exact: wf_code Hwf2']. }
  inversion Hb as [? ? ? Hp | ? ? ? Hr | | | ? ? Hs]; subst.
  - exists z1, H2'. split; last by split; [done | move=> k; exact: acong_refl].
    apply: steps_one; [exact: pure Hp | done | exact: Hwf_same].
  - case: (read_transport _ _ _ _ Hz Hr) => [Hr1 | [[vs [a Hes]] | [i [x [y Hes]]]]].
    + exists z1, H2'. split; last by split; [done | move=> k; exact: acong_refl].
      apply: steps_one; [exact: read Hr1 | done | exact: Hwf_same].
    + rewrite Hes in Hr Hwf1 *.
      have [es1' [Hr1 [Hwf1' Hc]]] := call_addr_sim _ _ _ _ _ Hz Hz1 Hr.
      exists z1, es1'. split; last by split.
      apply: steps_one; [exact: read Hr1 | done | done].
    + rewrite Hes in Hr Hwf1 *.
      have Hr1 := call_indirect_transport _ _ _ _ _ _ Hz Hr.
      exists z1, H2'. split; last by split; [done | move=> k; exact: acong_refl].
      apply: steps_one; [exact: read Hr1 | done | exact: Hwf_same].
  - match goal with Heq : @eq _ [:: LABEL_ _ _ _] _ |- _ =>
      move/(f_equal (last admininstr_NOP)): Heq; rewrite last_cat /= => Hh end.
    by move: Hcode; rewrite -Hh.
  - match goal with Heq : @eq _ [:: FRAME_ _ _ _] _ |- _ =>
      move/(f_equal (last admininstr_NOP)): Heq; rewrite last_cat /= => Hh end.
    by move: Hcode; rewrite -Hh.
  - have [z1' [Hs1 Hz']] := state_transport _ _ _ _ _ Hz Hs.
    exists z1', H2'. split; last by split; [done | move=> k; exact: acong_refl].
    apply: steps_one; [exact: state_step_step Hs1 | done | exact: Hwf_same].
Qed.

(* Blocks, loops and ifs: the original enters its own body. *)

Lemma wf_parts : forall z vs e rest, wf_config (mk_config z (vals vs ++ e :: rest)) ->
  wf_state z /\ List.Forall wf_val vs /\ wf_admininstr e /\ List.Forall wf_admininstr rest.
Proof.
  move=> z vs e rest H. have Hz := wf_state_of _ _ H. have := wf_code _ _ H.
  move/Forall_cat_inv => [Hvs /List.Forall_cons_iff [He Hr]]. by split; [ | split; [exact: vals_wf_inv | ]].
Qed.

Lemma block_head_sim : forall z1 z2 ws bt b1 b2 z2' H2' k, zrel z1 z2 -> icong k b1 b2 ->
  wf_config (mk_config z1 (vals ws ++ [:: admininstr_BLOCK bt b1])) ->
  base_step (mk_config z2 (vals ws ++ [:: admininstr_BLOCK bt b2])) (mk_config z2' H2') ->
  exists H1', Steps (mk_config z1 (vals ws ++ [:: admininstr_BLOCK bt b1])) (mk_config z1 H1') /\
    z2' = z2 /\ acong k H1' H2'.
Proof.
  move=> z1 z2 ws bt b1 b2 z2' H2' k Hz Hb Hwf Hstep.
  have [-> [n [-> Hrd]]] := base_block_inv _ _ _ _ _ _ Hstep.
  have [Hz1 [Hws [Hblock _]]] := wf_parts _ _ _ _ Hwf.
  exists [:: LABEL_ n [::] (vals ws ++ instrs b1)]. split; last split => //.
  - apply: steps_one => //.
    + case: (read_transport _ _ _ _ Hz (Hrd b1)) => [? | [[vs [a Hes]] | [i [x [y Hes]]]]].
      * exact: read.
      * by move: Hes; move/(f_equal (last admininstr_NOP)); rewrite !last_cat.
      * by move: Hes; move/(f_equal (last admininstr_NOP)); rewrite !last_cat.
    + apply: config_case_0 => //. constructor; last constructor. inversion Hblock; subst.
      constructor; first constructor. apply: Forall_cat; [exact: vals_admin_wf | exact: instrs_wf].
  - apply: acong_cons (acong_nil _). apply: aelem_label; first exact: icong_nil.
    apply: arel_cong. apply: acong_prefix. exact: icong_acong.
Qed.

Lemma loop_head_sim : forall z1 z2 ws bt b1 b2 z2' H2' k, zrel z1 z2 -> icong k b1 b2 ->
  wf_config (mk_config z1 (vals ws ++ [:: admininstr_LOOP bt b1])) ->
  base_step (mk_config z2 (vals ws ++ [:: admininstr_LOOP bt b2])) (mk_config z2' H2') ->
  exists H1', Steps (mk_config z1 (vals ws ++ [:: admininstr_LOOP bt b1])) (mk_config z1 H1') /\
    z2' = z2 /\ acong k H1' H2'.
Proof.
  move=> z1 z2 ws bt b1 b2 z2' H2' k Hz Hb Hwf Hstep.
  have [-> [n [-> Hrd]]] := base_loop_inv _ _ _ _ _ _ Hstep.
  have [Hz1 [Hws [Hloop _]]] := wf_parts _ _ _ _ Hwf.
  exists [:: LABEL_ n [:: LOOP bt b1] (vals ws ++ instrs b1)]. split; last split => //.
  - apply: steps_one => //.
    + case: (read_transport _ _ _ _ Hz (Hrd b1)) => [? | [[vs [a Hes]] | [i [x [y Hes]]]]].
      * exact: read.
      * by move: Hes; move/(f_equal (last admininstr_NOP)); rewrite !last_cat.
      * by move: Hes; move/(f_equal (last admininstr_NOP)); rewrite !last_cat.
    + apply: config_case_0 => //. constructor; last constructor. inversion Hloop; subst.
      constructor; first by repeat constructor.
      apply: Forall_cat; [exact: vals_admin_wf | exact: instrs_wf].
  - apply: acong_cons (acong_nil _). apply: aelem_label.
    + apply: icong_cons (icong_nil _). exact: ielem_loop.
    + apply: arel_cong. apply: acong_prefix. exact: icong_acong.
Qed.

Lemma if_head_sim : forall z1 z2 ws bt b1 b2 c1 c2 z2' H2' k, icong k b1 b2 -> icong k c1 c2 ->
  wf_config (mk_config z1 (vals ws ++ [:: admininstr_IFELSE bt b1 c1])) ->
  base_step (mk_config z2 (vals ws ++ [:: admininstr_IFELSE bt b2 c2])) (mk_config z2' H2') ->
  exists H1', Steps (mk_config z1 (vals ws ++ [:: admininstr_IFELSE bt b1 c1])) (mk_config z1 H1') /\
    z2' = z2 /\ acong k H1' H2'.
Proof.
  move=> z1 z2 ws bt b1 b2 c1 c2 z2' H2' k Hb Hc Hwf Hstep.
  have [-> [cnd [Hws [[-> Hp] | [-> Hp]]]]] := base_if_inv _ _ _ _ _ _ _ Hstep.
  all: subst ws; have [Hz1 [_ [Hif _]]] := wf_parts _ _ _ _ Hwf; inversion Hif; subst.
  - exists [:: admininstr_BLOCK bt b1]. split; last split => //.
    + apply: steps_one; [exact: pure (Hp b1 c1) | exact: Hwf | ].
      apply: config_case_0 => //. by repeat constructor.
    + apply: acong_cons (acong_nil _). exact: aelem_block.
  - exists [:: admininstr_BLOCK bt c1]. split; last split => //.
    + apply: steps_one; [exact: pure (Hp b1 c1) | exact: Hwf | ].
      apply: config_case_0 => //. by repeat constructor.
    + apply: acong_cons (acong_nil _). exact: aelem_block.
Qed.

Lemma steps_locals_size : forall c c', Steps c c' ->
  match c, c' with
  | mk_config z _, mk_config z' _ => size (LOCALS (frame_of z')) = size (LOCALS (frame_of z))
  end.
Proof.
  move=> c c'; elim=> //= z es z'' es'' z' es' Hstep _ IH _ _ _.
  by rewrite IH (step_locals_size _ _ _ _ Hstep).
Qed.

Lemma wf_label_inv : forall n c es, wf_admininstr (LABEL_ n c es) ->
  List.Forall wf_instr c /\ List.Forall wf_admininstr es.
Proof. by move=> n c es H; inversion H. Qed.

Lemma wf_frame_inv : forall n f es, wf_admininstr (FRAME_ n f es) ->
  wf_frame f /\ List.Forall wf_admininstr es.
Proof. by move=> n f es H; inversion H. Qed.

Lemma no_trap_vals : forall vs, ~ trap_headed (vals vs).
Proof. move=> vs [r]. by rewrite -(cats0 (vals vs)) split_vals_map_cat. Qed.

(* The simulation, for configurations nested at most `d` deep. *)
Definition sim_goal (d : nat) : Prop :=
  forall z1 z2 es1 es2 z2' es2', (ldepth es2 <= d)%nat -> zrel z1 z2 ->
  wf_config (mk_config z1 es1) -> wf_config (mk_config z2 es2) ->
  arel (size (LOCALS (frame_of z1))) es1 es2 ->
  Step (mk_config z2 es2) (mk_config z2' es2') -> wf_config (mk_config z2' es2') ->
  exists z1' es1', Steps (mk_config z1 es1) (mk_config z1' es1') /\ zrel z1' z2' /\
    arel (size (LOCALS (frame_of z1'))) es1' es2'.

(* Labels: steps inside follow by induction; leaving a label follows once the
   original catches up inside it. *)
Lemma label_head_sim : forall d, (forall d', (d' < d)%nat -> sim_goal d') ->
  forall z1 z2 ws n c1 c2 es1 es2 z2' H2', (ldepth es2 < d)%nat -> zrel z1 z2 ->
  icong (size (LOCALS (frame_of z1))) c1 c2 -> arel (size (LOCALS (frame_of z1))) es1 es2 ->
  wf_config (mk_config z1 (vals ws ++ [:: LABEL_ n c1 es1])) ->
  wf_config (mk_config z2 (vals ws ++ [:: LABEL_ n c2 es2])) ->
  base_step (mk_config z2 (vals ws ++ [:: LABEL_ n c2 es2])) (mk_config z2' H2') ->
  wf_config (mk_config z2' H2') ->
  exists z1' H1', Steps (mk_config z1 (vals ws ++ [:: LABEL_ n c1 es1])) (mk_config z1' H1') /\
    zrel z1' z2' /\ acong (size (LOCALS (frame_of z1'))) H1' H2'.
Proof.
  move=> d IH z1 z2 ws n c1 c2 es1 es2 z2' H2' Hd Hz Hc Hr Hwf1 Hwf2 Hb Hwf2'.
  have [Hws Hcases] := base_label_inv _ _ _ _ _ _ _ Hb. subst ws.
  have [Hz1 [_ [Hl1 _]]] := wf_parts _ [::] _ [::] Hwf1.
  have [Hc1 Hes1] := wf_label_inv _ _ _ Hl1.
  have Hwf1in : wf_config (mk_config z1 es1) by apply: config_case_0.
  (* Run the original inside the label, then take one step out of it. *)
  have Hexit : forall es1c H1, Steps (mk_config z1 es1) (mk_config z1 es1c) ->
      Step_pure [:: LABEL_ n c1 es1c] H1 -> List.Forall wf_admininstr H1 ->
      Steps (mk_config z1 [:: LABEL_ n c1 es1]) (mk_config z1 H1).
  { move=> es1c H1 Hs Hp HH1. apply: (steps_trans _ _ _ (steps_in_label _ _ Hs n c1 Hc1)).
    have [_ Hwfc] := steps_wf _ _ Hs.
    apply: steps_one; [exact: pure Hp | | by apply: config_case_0].
    apply: config_case_0 => //. repeat constructor => //. exact: wf_code Hwfc. }
  have Hplain : forall us h rest, es2 = vals us ++ h :: rest ->
      to_val h = None -> ~~ has_code h -> h <> admininstr_TRAP ->
      (forall x, h <> admininstr_LOCAL_TEE x) -> (forall x, h <> admininstr_LOCAL_SET x) ->
      exists r1, Steps (mk_config z1 es1) (mk_config z1 (vals us ++ h :: r1)).
  { move=> us h rest Hes2 Hh Hcode Htrap Htee Hset. rewrite Hes2 in Hr.
    have [r1 [Hs _]] := catch_up_plain _ _ _ _ _ _ Hr Hwf1in Hh Hcode Htrap Htee Hset.
    by exists r1. }
  case: Hcases => [[es2' [Hin [Hwf2in [Hwf2in' HH]]]] | [[Hz2 [vs [Hes2 HH]]] |
    [[Hz2 [vs' [vs [rest [Hes2 [Hn HH]]]]]] | [[Hz2 [vs [l [rest [Hes2 HH]]]]] |
    [[Hz2 [vs [rest [Hes2 HH]]]] | [Hz2 [Hes2 HH]]]]]]];
    subst H2'; try subst z2'; have Hcode2' := wf_code _ _ Hwf2'.
  - have [z1' [es1' [Hs [Hz' Hr']]]] :=
      IH (ldepth es2) Hd z1 z2 es1 es2 z2' es2' (le_n _) Hz Hwf1in Hwf2in Hr Hin Hwf2in'.
    exists z1', [:: LABEL_ n c1 es1']. split; [ | split; [done | ]].
    + exact: (steps_in_label _ _ Hs n c1 Hc1).
    + have Hk := steps_locals_size _ _ Hs. apply: acong_cons (acong_nil _).
      apply: aelem_label; last done. by rewrite Hk.
  - subst es2. inversion Hr as [? ? ? Hc' | ? ? ? _ Ht]; subst; last by case: (no_trap_vals vs).
    have Hs := catch_up_vals _ _ _ Hc' z1 vs Hwf1in erefl.
    exists z1, (vals vs). split; [ | split; [done | exact: acong_refl]].
    exact: (Hexit _ _ Hs (label_vals _ _ _) Hcode2').
  - have Hes2' : es2 = vals (vs' ++ vs) ++ admininstr_BR (mk_uN 0) :: rest.
    { by rewrite Hes2 map_cat -!catA. }
    have [r1 Hs] := Hplain _ _ _ Hes2' erefl erefl ltac:(done) ltac:(done) ltac:(done).
    exists z1, (vals vs ++ instrs c1). split; [ | split; [done | ]].
    + apply: (Hexit _ _ Hs).
      * have := br_zero n c1 vs' vs r1 Hn. by rewrite map_cat -!catA.
      * apply: Forall_cat; last exact: instrs_wf.
        by have [] := Forall_cat_inv _ _ _ _ Hcode2'.
    + apply: acong_prefix. exact: icong_acong.
  - have Hes2' : es2 = vals vs ++ admininstr_BR (mk_uN ((l :> N) + 1)%BN) :: rest.
    { by rewrite Hes2 -!catA. }
    have [r1 Hs] := Hplain _ _ _ Hes2' erefl erefl ltac:(done) ltac:(done) ltac:(done).
    exists z1, (vals vs ++ [:: admininstr_BR l]). split; [ | split; [done | exact: acong_refl]].
    apply: (Hexit _ _ Hs); last done.
    have := br_succ n c1 vs l r1. by rewrite -!catA.
  - have Hes2' : es2 = vals vs ++ admininstr_RETURN :: rest by rewrite Hes2 -!catA.
    have [r1 Hs] := Hplain _ _ _ Hes2' erefl erefl ltac:(done) ltac:(done) ltac:(done).
    exists z1, (vals vs ++ [:: admininstr_RETURN]). split; [ | split; [done | exact: acong_refl]].
    apply: (Hexit _ _ Hs); last done.
    have := return_label n c1 vs r1. by rewrite -!catA.
  - have Ht2 : trap_headed es2 by rewrite Hes2; exists [::].
    have [es1c [Hs Ht1]] := catch_up_trap _ _ _ _ Hr Ht2 Hwf1in.
    have [_ Hwfc] := steps_wf _ _ Hs.
    have Hcol := trap_collapse _ _ Hwfc Ht1.
    exists z1, [:: admininstr_TRAP]. split; [ | split; [done | exact: acong_refl]].
    exact: (Hexit _ _ (steps_trans _ _ _ Hs Hcol) (trap_label _ _) Hcode2').
Qed.

Lemma zrel_reframe : forall s1 s2 f g, zrel (mk_state s1 f) (mk_state s2 f) ->
  zrel (mk_state s1 g) (mk_state s2 g).
Proof. by move=> s1 s2 f g [_ H]. Qed.

(* Frames: the same, with the inner configuration running in the frame's own
   state. *)
Lemma frame_head_sim : forall d, (forall d', (d' < d)%nat -> sim_goal d') ->
  forall s1 s2 f ws n f' es1 es2 z2' H2', (ldepth es2 < d)%nat ->
  zrel (mk_state s1 f) (mk_state s2 f) -> arel (size (LOCALS f')) es1 es2 ->
  wf_config (mk_config (mk_state s1 f) (vals ws ++ [:: FRAME_ n f' es1])) ->
  wf_config (mk_config (mk_state s2 f) (vals ws ++ [:: FRAME_ n f' es2])) ->
  base_step (mk_config (mk_state s2 f) (vals ws ++ [:: FRAME_ n f' es2])) (mk_config z2' H2') ->
  wf_config (mk_config z2' H2') ->
  exists z1' H1', Steps (mk_config (mk_state s1 f) (vals ws ++ [:: FRAME_ n f' es1]))
                        (mk_config z1' H1') /\
    zrel z1' z2' /\ forall k, acong k H1' H2'.
Proof.
  move=> d IH s1 s2 f ws n f' es1 es2 z2' H2' Hd Hz Hr Hwf1 Hwf2 Hb Hwf2'.
  have [Hws Hcases] := base_frame_inv _ _ _ _ _ _ _ _ Hb. subst ws.
  have [Hz1 [_ [Hfr1 _]]] := wf_parts _ [::] _ [::] Hwf1.
  have [Hf' Hes1] := wf_frame_inv _ _ _ Hfr1.
  have Hs1wf : wf_store s1 by inversion Hz1.
  have Hf : wf_frame f by inversion Hz1.
  have Hwf1in : wf_config (mk_config (mk_state s1 f') es1).
  { apply: config_case_0; [by constructor | done]. }
  have Hzin := zrel_reframe _ _ _ f' Hz.
  have Hexit : forall es1c H1, Steps (mk_config (mk_state s1 f') es1) (mk_config (mk_state s1 f') es1c) ->
      Step_pure [:: FRAME_ n f' es1c] H1 -> List.Forall wf_admininstr H1 ->
      Steps (mk_config (mk_state s1 f) [:: FRAME_ n f' es1]) (mk_config (mk_state s1 f) H1).
  { move=> es1c H1 Hs Hp HH1. apply: (steps_trans _ _ _ (steps_in_frame _ _ Hs n f Hf)).
    have [_ Hwfc] := steps_wf _ _ Hs.
    apply: steps_one; [exact: pure Hp | | by apply: config_case_0].
    apply: config_case_0 => //. repeat constructor => //. exact: wf_code Hwfc. }
  case: Hcases => [[s2' [f'' [es2' [Hin [Hwf2in [Hwf2in' [Hz2 HH]]]]]]] | [[Hz2 [vs [Hes2 [Hn HH]]]] |
    [[Hz2 [vs' [vs [rest [Hes2 [Hn HH]]]]]] | [Hz2 [Hes2 HH]]]]];
    subst H2' z2'; have Hcode2' := wf_code _ _ Hwf2'.
  - have [[s1' f1'] [es1' [Hs [Hz' Hr']]]] :=
      IH (ldepth es2) Hd _ _ es1 es2 _ es2' (le_n _) Hzin Hwf1in Hwf2in Hr Hin Hwf2in'.
    case: Hz' => Hf1' Hsrel. subst f1'.
    exists (mk_state s1' f), [:: FRAME_ n f'' es1']. split; [ | split].
    + exact: (steps_in_frame _ _ Hs n f Hf).
    + by split.
    + move=> k. apply: acong_cons (acong_nil _). exact: aelem_frame.
  - subst es2. inversion Hr as [? ? ? Hc' | ? ? ? _ Ht]; subst; last by case: (no_trap_vals vs).
    have Hs := catch_up_vals _ _ _ Hc' _ vs Hwf1in erefl.
    exists (mk_state s1 f), (vals vs). split; [ | split; [done | move=> k; exact: acong_refl]].
    exact: (Hexit _ _ Hs (frame_vals _ _ _ Hn) Hcode2').
  - have Hes2' : es2 = vals (vs' ++ vs) ++ admininstr_RETURN :: rest.
    { by rewrite Hes2 map_cat -!catA. }
    rewrite Hes2' in Hr.
    have [r1 [Hs _]] := catch_up_plain _ _ _ _ _ _ Hr Hwf1in erefl erefl ltac:(done) ltac:(done) ltac:(done).
    exists (mk_state s1 f), (vals vs). split; [ | split; [done | move=> k; exact: acong_refl]].
    apply: (Hexit _ _ Hs); last done.
    have := return_frame n f' vs' vs r1 Hn. by rewrite map_cat -!catA.
  - have Ht2 : trap_headed es2 by rewrite Hes2; exists [::].
    have [es1c [Hs Ht1]] := catch_up_trap _ _ _ _ Hr Ht2 Hwf1in.
    have [_ Hwfc] := steps_wf _ _ Hs.
    have Hcol := trap_collapse _ _ Hwfc Ht1.
    exists (mk_state s1 f), [:: admininstr_TRAP].
    split; [ | split; [done | move=> k; exact: acong_refl]].
    exact: (Hexit _ _ (steps_trans _ _ _ Hs Hcol) (trap_frame _ _) Hcode2').
Qed.

Lemma size_with_local : forall z x v,
  size (LOCALS (frame_of (with_local z x v))) = size (LOCALS (frame_of z)).
Proof. move=> [s f] x v /=. exact: size_list_update. Qed.

Lemma adepth_label : forall n c es, adepth (LABEL_ n c es) = S (ldepth es).
Proof. done. Qed.

Lemma adepth_frame : forall n f es, adepth (FRAME_ n f es) = S (ldepth es).
Proof. done. Qed.

Lemma sim_step_rec : forall d, (forall d', (d' < d)%nat -> sim_goal d') -> sim_goal d.
Proof.
  move=> d IH z1 z2 es1 es2 z2' es2' Hd Hz Hwf1 Hwf2 Hrel Hstep Hwf2'.
  inversion Hrel as [? ? ? Hc | ? ? ? Ht1 Ht2]; subst; last first.
  { have [Hz2' Ht2'] := step_trap _ _ _ _ Ht2 Hstep. subst z2'.
    exists z1, es1. split; [exact: Steps__refl | split; [done | exact: arel_trap]]. }
  case Hsp: (split_vals es2) => [us r].
  case: r Hsp => [ | h r2] Hsp.
  { exfalso. move: Hstep. rewrite (split_vals_spec es2) Hsp cats0. exact: vals_no_step. }
  have Hh : to_val h = None by apply: (split_vals_head es2 h r2); rewrite Hsp.
  have Hes2 : es2 = vals us ++ h :: r2 by rewrite {1}(split_vals_spec es2) Hsp.
  have [es1c [Hs1 Hcaught]] := catch_up _ _ _ Hc z1 us h r2 Hwf1 Hes2 Hh.
  have [_ Hwf1c] := steps_wf _ _ Hs1.
  have Hz1wf := wf_state_of _ _ Hwf1.
  have Hstep2 : Step (mk_config z2 (vals us ++ h :: r2)) (mk_config z2' es2') by rewrite -Hes2.
  case: (admininstr_eq_dec h admininstr_TRAP) => [Htrap | Hnt].
  { subst h. have Ht2 : trap_headed es2 by exists r2; rewrite Hsp.
    have [Hz2' Ht2'] := step_trap _ _ _ _ Ht2 Hstep. subst z2'.
    exists z1, es1c. split; first done. split; first done. apply: arel_trap => //.
    case: Hcaught => [[h1 [r1 [-> [He _]]]] | [[x [r1 [? _]]] | [[x [r1 [? _]]] | [x [? [? [? [? _]]]]]]]] //.
    rewrite (aelem_plain _ _ _ He erefl). exists r1. by rewrite split_vals_nonval. }
  case: Hcaught => [[h1 [r1 [Hes1c [He Hc1]]]] | [[x [r1 [Hhx [Hx [Hes1c Hc1]]]]] |
    [[x [r1 [Hhx [Hes1c Hc1]]]] | [x [us0 [v [r1 [Hhx [Hx [Hus [Hes1c Hc1]]]]]]]]]]].
  - (* Related instructions: the original takes the corresponding step. *)
    have := step_at_head _ _ _ _ Hstep. rewrite /at_head Hsp /= => /(_ Hnt).
    move=> [vs1 [vs2 [H2' [Hus [Hes2' Hb]]]]]. subst us.
    have Hcode2 := wf_code _ _ Hwf2. rewrite Hes2 map_cat -catA in Hcode2.
    have [Hv1 Hcode2r] := Forall_cat_inv _ _ _ _ Hcode2.
    have [Hredex2 _] := Forall_cat_inv _ wf_admininstr (vals vs2 ++ [:: h]) r2 ltac:(by rewrite -catA).
    have Hcode2' := wf_code _ _ Hwf2'. rewrite Hes2' in Hcode2'.
    have [_ HH'r] := Forall_cat_inv _ _ _ _ Hcode2'.
    have [HH' _] := Forall_cat_inv _ _ _ _ HH'r.
    have Hcode1c := wf_code _ _ Hwf1c. rewrite Hes1c map_cat -catA in Hcode1c.
    have [_ Hrest1] := Forall_cat_inv _ _ _ _ Hcode1c.
    have [Hredex1 Hr1] := Forall_cat_inv _ wf_admininstr (vals vs2 ++ [:: h1]) r1 ltac:(by rewrite -catA).
    have Hwfr1 : wf_config (mk_config z1 (vals vs2 ++ [:: h1])) by apply: config_case_0.
    have Hwfr2 : wf_config (mk_config z2 (vals vs2 ++ [:: h])) by apply: (wf_with_code _ _ _ Hwf2).
    have HwfH2 : wf_config (mk_config z2' H2') by apply: (wf_with_code _ _ _ Hwf2').
    have Hdh : (adepth h <= d)%nat.
    { apply: (Nat.le_trans _ _ _ _ Hd). rewrite Hes2. exact: ldepth_cat_cons. }
    have Hhead : exists z1' H1', Steps (mk_config z1 (vals vs2 ++ [:: h1])) (mk_config z1' H1') /\
        zrel z1' z2' /\ acong (size (LOCALS (frame_of z1'))) H1' H2'.
    { case Hcode: (has_code h); last first.
      { rewrite (aelem_plain _ _ _ He (negbT Hcode)) in Hwfr1 *.
        have [z1' [H1' [Hs [Hz' Hrel']]]] :=
          plain_head_sim _ _ _ _ _ _ Hz Hwfr1 HwfH2 (negbT Hcode) Hb.
        by exists z1', H1'. }
      destruct h; try discriminate Hcode.
      - have [b1 [Hh1 Hb1]] := aelem_block_inv _ _ _ _ He. subst h1.
        have [H1' [Hs [-> HrelH]]] := block_head_sim _ _ _ _ _ _ _ _ _ Hz Hb1 Hwfr1 Hb.
        by exists z1, H1'.
      - have [b1 [Hh1 Hb1]] := aelem_loop_inv _ _ _ _ He. subst h1.
        have [H1' [Hs [-> HrelH]]] := loop_head_sim _ _ _ _ _ _ _ _ _ Hz Hb1 Hwfr1 Hb.
        by exists z1, H1'.
      - have [b1 [c1 [Hh1 [Hb1 Hc1']]]] := aelem_if_inv _ _ _ _ _ He. subst h1.
        have [H1' [Hs [-> HrelH]]] := if_head_sim _ _ _ _ _ _ _ _ _ _ _ Hb1 Hc1' Hwfr1 Hb.
        by exists z1, H1'.
      - have [c1 [es1in [Hh1 [Hc1' Hr1']]]] := aelem_label_inv _ _ _ _ _ He. subst h1.
        rewrite adepth_label in Hdh.
        exact: (label_head_sim d IH _ _ _ _ _ _ _ _ _ _ Hdh Hz Hc1' Hr1' Hwfr1 Hwfr2 Hb HwfH2).
      - have [es1in [Hh1 Hr1']] := aelem_frame_inv _ _ _ _ _ He. subst h1.
        rewrite adepth_frame in Hdh.
        destruct z1 as [s1 g1], z2 as [s2 g2]. have Hg := zrel_frame _ _ Hz. simpl in Hg. subst g2.
        have [z1' [H1' [Hs [Hz' HrelH]]]] :=
          frame_head_sim d IH _ _ _ _ _ _ _ _ _ _ Hdh Hz Hr1' Hwfr1 Hwfr2 Hb HwfH2.
        by exists z1', H1'. }
    have [z1' [H1' [Hs [Hz' HrelH]]]] := Hhead.
    have Hk' := steps_locals_size _ _ Hs. simpl in Hk'.
    exists z1', (vals vs1 ++ H1' ++ r1). split; [ | split; [done | ]].
    + apply: (steps_trans _ _ _ Hs1). rewrite Hes1c map_cat -catA.
      have := lift _ _ _ _ vs1 r1 Hs (vals_wf_inv _ Hv1) Hr1. by rewrite -catA.
    + apply: arel_cong. rewrite Hes2' Hk'. rewrite Hk' in HrelH.
      apply: acong_prefix. exact: acong_cat HrelH Hc1.
  - (* Rule 1 starts: the rewrite runs `local.tee x`, the original waits. *)
    subst h. have [us' [v [Hus [Hz' Hes2']]]] := step_tee _ _ _ _ _ _ Hstep2.
    subst z2'. exists z1, es1c. split; first done. split; first done. apply: arel_cong.
    rewrite Hes1c Hes2' Hus map_cat -!catA /=. apply: acong_prefix. exact: acong_tee_mid.
  - (* Rule 2: the rewrite runs `local.set x`, the original
       `local.tee x; drop`. *)
    subst h. have [us' [v [Hus [Hz' Hes2']]]] := step_set _ _ _ _ _ _ Hstep2.
    subst us z2'. destruct z1 as [s1 f1].
    have Hcode1c := wf_code _ _ Hwf1c. rewrite Hes1c map_cat -catA in Hcode1c.
    have [Hvs' Hrest] := Forall_cat_inv _ _ _ _ Hcode1c.
    move: Hrest => /= /List.Forall_cons_iff [Hv /List.Forall_cons_iff [Htee /List.Forall_cons_iff [_ Hr1]]].
    have Hxwf : wf_uN 32%N x by inversion Htee.
    have [Hrun _] := tee_drop_set s1 f1 x Hz1wf Hxwf v (admin_val_wf_inv _ Hv).
    exists (with_local (mk_state s1 f1) x v), (vals us' ++ r1). split; [ | split].
    + apply: (steps_trans _ _ _ Hs1). rewrite Hes1c map_cat -catA.
      exact: (lift _ _ _ _ us' r1 Hrun (vals_wf_inv _ Hvs') Hr1).
    + exact: zrel_with_local.
    + apply: arel_cong. rewrite size_with_local Hes2'. exact: acong_prefix.
  - (* Rule 1 finishes: the rewrite runs `local.set x`, the original
       `local.set x; local.get x`. *)
    subst h. have [us' [v' [Hus' [Hz' Hes2']]]] := step_set _ _ _ _ _ _ Hstep2.
    have Hr : rcons (us0 ++ [:: v]) v = rcons us' v' by rewrite -!cats1 -catA -Hus' Hus.
    have [Hus0 Hv] := rcons_inj Hr. subst us' v' z2'. destruct z1 as [s1 f1].
    have Hcode1c := wf_code _ _ Hwf1c. rewrite Hes1c map_cat -catA in Hcode1c.
    have [Hvs0 Hrest] := Forall_cat_inv _ _ _ _ Hcode1c.
    move: Hrest => /= /List.Forall_cons_iff [Hv /List.Forall_cons_iff [Hset /List.Forall_cons_iff [_ Hr1]]].
    have Hxwf : wf_uN 32%N x by inversion Hset.
    have [Hrun _] := set_get_tee s1 f1 x Hz1wf Hxwf Hx v (admin_val_wf_inv _ Hv).
    exists (with_local (mk_state s1 f1) x v), (vals (us0 ++ [:: v]) ++ r1). split; [ | split].
    + apply: (steps_trans _ _ _ Hs1). rewrite Hes1c !map_cat -!catA.
      exact: (lift _ _ _ _ us0 r1 Hrun (vals_wf_inv _ Hvs0) Hr1).
    + exact: zrel_with_local.
    + apply: arel_cong. rewrite size_with_local Hes2'. exact: acong_prefix.
Qed.

Lemma sim_step : forall d, sim_goal d.
Proof. move=> d. induction d as [d IH] using Wf_nat.lt_wf_ind. exact: sim_step_rec. Qed.

(* The end of a run: values, or a trap. *)
Definition terminal (es : seq admininstr) : Prop :=
  (exists vs, es = vals vs) \/ es = [:: admininstr_TRAP].

Lemma finish : forall k z es1 es2, wf_config (mk_config z es1) -> arel k es1 es2 -> terminal es2 ->
  Steps (mk_config z es1) (mk_config z es2).
Proof.
  move=> k z es1 es2 Hwf Hr [[vs Hvs] | Htrap].
  - inversion Hr as [? ? ? Hc | ? ? ? _ Ht]; subst; last by case: (no_trap_vals vs).
    exact: (catch_up_vals _ _ _ Hc z vs Hwf erefl).
  - have Ht2 : trap_headed es2 by rewrite Htrap; exists [::].
    have [es1c [Hs Ht1]] := catch_up_trap _ _ _ _ Hr Ht2 Hwf.
    have [_ Hwfc] := steps_wf _ _ Hs.
    rewrite Htrap. exact: (steps_trans _ _ _ Hs (trap_collapse _ _ Hwfc Ht1)).
Qed.

(* Every run of the rewritten program to values or a trap has a run of the
   original to the same values or trap, ending in a related state: the same
   but for function bodies. *)
Theorem nested_refines : forall c2 c2f, Steps c2 c2f ->
  forall z1 z2 es1 es2 z2f res, c2 = mk_config z2 es2 -> c2f = mk_config z2f res -> terminal res ->
  zrel z1 z2 -> wf_config (mk_config z1 es1) -> arel (size (LOCALS (frame_of z1))) es1 es2 ->
  exists z1f, Steps (mk_config z1 es1) (mk_config z1f res) /\ zrel z1f z2f.
Proof.
  move=> c2 c2f; elim.
  - move=> z2' code Hwf z1 z2 es1 es2 z2f res [Hz2 Hes2] [Hz2f Hres] Hterm Hz Hwf1 Hrel.
    subst. exists z1. split; [exact: finish Hwf1 Hrel Hterm | done].
  - move=> z2' code z'' code'' z' code' Hstep Hsteps IHs Hwf2 _ Hwf2'
      z1 z2 es1 es2 z2f res [Hz2 Hes2] Hf Hterm Hz Hwf1 Hrel. subst z2' code.
    have [z1' [es1' [Hs1 [Hz' Hrel']]]] :=
      sim_step (ldepth es2) z1 z2 es1 es2 z' code' (le_n _) Hz Hwf1 Hwf2 Hrel Hstep Hwf2'.
    have [_ Hwf1'] := steps_wf _ _ Hs1.
    have [z1f [Hs2 Hzf]] := IHs _ _ es1' _ _ _ erefl Hf Hterm Hz' Hwf1' Hrel'.
    exists z1f. split; [exact: (steps_trans _ _ _ Hs1 Hs2) | done].
Qed.

(* In particular: invoking a function of the rewritten store gives the same
   result as invoking it in the original store. *)
Corollary invoke_refines : forall s1 s2 f vs a z2f res,
  srel s1 s2 -> wf_config (mk_config (mk_state s1 f) (vals vs ++ [:: CALL_ADDR a])) ->
  Steps (mk_config (mk_state s2 f) (vals vs ++ [:: CALL_ADDR a])) (mk_config z2f res) ->
  terminal res ->
  exists z1f, Steps (mk_config (mk_state s1 f) (vals vs ++ [:: CALL_ADDR a])) (mk_config z1f res) /\
    zrel z1f z2f.
Proof.
  move=> s1 s2 f vs a z2f res Hs Hwf Hrun Hterm.
  exact: (nested_refines _ _ Hrun (mk_state s1 f) (mk_state s2 f) _ _ z2f res erefl erefl Hterm
    (conj erefl Hs) Hwf (arel_refl _ _)).
Qed.
