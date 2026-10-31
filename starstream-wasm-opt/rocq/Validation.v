(* The local-cleanup rules keep code valid: with the same context and the same
   type, the rewritten instructions, functions and modules validate whenever
   the originals do. *)
From Stdlib Require Import List NArith.
From mathcomp Require Import ssreflect ssrfun ssrbool eqtype seq.
Require Import Wasm2 Nested.

Set Implicit Arguments.
Unset Strict Implicit.

Local Notation fn t1 t2 := (mk_functype (mk_list _ t1) (mk_list _ t2)).
Local Notation vsub := (List.Forall2 Valtype_sub).

Lemma vsub_refl : forall ts, vsub ts ts.
Proof. elim=> [ | t ts IH]; constructor=> //. exact: refl. Qed.

Lemma vsub_trans : forall a b c, vsub a b -> vsub b c -> vsub a c.
Proof.
  move=> a b c H; elim: H c => [ | x y a' b' Hxy _ IH] c; first by [].
  case: c => [ | z c'] H; first by inversion H.
  inversion H as [ | ? ? ? ? Hyz Hr]; subst.
  constructor; last exact: IH.
  case: Hxy Hyz => [t | t] Hyz; [exact: Hyz | exact: bot].
Qed.

Lemma vsub_cat : forall a b c d, vsub a b -> vsub c d -> vsub (a ++ c) (b ++ d).
Proof.
  move=> a b c d H Hcd; elim: H => [ | x y a' b' Hxy _ IH] //=.
  by constructor.
Qed.

Lemma vsub_size : forall a b, vsub a b -> size a = size b.
Proof. by move=> a b; elim=> // x y a' b' _ _ /= ->. Qed.

(* Removing what two equal-length lists end with keeps them related. *)
Lemma vsub_prefix : forall a b c d, size a = size b -> vsub (a ++ c) (b ++ d) -> vsub a b.
Proof.
  elim=> [ | x a IH] [ | y b] c d //= Hs H.
  inversion H; subst.
  constructor=> //.
  apply: (IH _ c d) => //.
  by case: Hs.
Qed.

Lemma vsub_rsub : forall a b, vsub a b -> Resulttype_sub (mk_list _ a) (mk_list _ b).
Proof.
  move=> a b H; constructor=> //.
  by rewrite (vsub_size H).
Qed.

Lemma rsub_vsub : forall a b, Resulttype_sub (mk_list _ a) (mk_list _ b) -> vsub a b.
Proof. by move=> a b H; inversion H; subst. Qed.

Lemma instr_ok_ctx : forall C i ft, Instr_ok C i ft -> wf_context C.
Proof. by move=> C i ft []. Qed.

Lemma instr_ok_wf : forall C i ft, Instr_ok C i ft -> wf_instr i.
Proof. by move=> C i ft []. Qed.

Lemma ok_ctx : forall C b ft, Instrs_ok C b ft -> wf_context C.
Proof. by move=> C b ft []. Qed.

Lemma forall_cat : forall (A : Type) (P : A -> Prop) (a b : seq A),
  List.Forall P a -> List.Forall P b -> List.Forall P (a ++ b).
Proof.
  move=> A P a b; elim=> //= x l Hx _ IH Hb.
  constructor=> //.
  exact: IH Hb.
Qed.

Lemma ok_wf : forall C b ft, Instrs_ok C b ft -> List.Forall wf_instr b.
Proof.
  move=> C b ft; case=> //.
  - by move=> ? ? ? ? _ _ Hi; constructor.
  - by move=> ? b1 b2 ? ? ? _ _ _ H1 H2; apply: forall_cat.
Qed.

Lemma ok_sub : forall C b t1 t2 t1' t2', Instrs_ok C b (fn t1 t2) ->
  vsub t1' t1 -> vsub t2 t2' -> Instrs_ok C b (fn t1' t2').
Proof.
  move=> C b t1 t2 t1' t2' H H1 H2.
  apply: (sub _ _ _ _ _ _ H (vsub_rsub H1) (vsub_rsub H2)).
  - exact: ok_ctx H.
  - exact: ok_wf H.
Qed.

Lemma ok_frame : forall C b ts t1 t2, Instrs_ok C b (fn t1 t2) ->
  Instrs_ok C b (fn (ts ++ t1) (ts ++ t2)).
Proof.
  move=> C b ts t1 t2 H.
  apply: (Instrs_ok__frame _ _ _ _ _ H).
  - exact: ok_ctx H.
  - exact: ok_wf H.
Qed.

Lemma ok_seq : forall C b1 b2 t1 t2 t3, Instrs_ok C b1 (fn t1 t2) -> Instrs_ok C b2 (fn t2 t3) ->
  Instrs_ok C (b1 ++ b2) (fn t1 t3).
Proof.
  move=> C b1 b2 t1 t2 t3 H1 H2.
  apply: (res_seq _ _ _ _ _ _ H1 H2).
  - exact: ok_ctx H1.
  - exact: ok_wf H1.
  - exact: ok_wf H2.
Qed.

Lemma ok_nil : forall C t1 t2, wf_context C -> vsub t1 t2 -> Instrs_ok C [::] (fn t1 t2).
Proof.
  move=> C t1 t2 Hc Hs.
  have H := ok_frame t1 (empty _ Hc).
  rewrite cats0 in H.
  exact: ok_sub H (vsub_refl t1) Hs.
Qed.

Lemma ok_one : forall C i ts a r t1 t2, Instr_ok C i (fn a r) ->
  vsub t1 (ts ++ a) -> vsub (ts ++ r) t2 -> Instrs_ok C [:: i] (fn t1 t2).
Proof.
  move=> C i ts a r t1 t2 Hi H1 H2.
  have H := Instrs_ok__instr _ _ _ _ Hi (instr_ok_ctx Hi) (instr_ok_wf Hi).
  exact: ok_sub (ok_frame ts H) H1 H2.
Qed.

Lemma cat_nil : forall (A : Type) (a b : seq A), a ++ b = [::] -> a = [::] /\ b = [::].
Proof. by move=> A [ | x a] [ | y b]. Qed.

Lemma cat_one : forall (A : Type) (a b : seq A) i, a ++ b = [:: i] ->
  (a = [::] /\ b = [:: i]) \/ (a = [:: i] /\ b = [::]).
Proof.
  move=> A [ | x [ | y a]] b i //=.
  - by left.
  - by case=> <- ->; right.
Qed.

Lemma ok_nil_inv : forall C b ft, Instrs_ok C b ft -> b = [::] ->
  forall t1 t2, ft = fn t1 t2 -> vsub t1 t2.
Proof.
  move=> C b ft; elim=> //.
  - by move=> C' _ _ t1 t2 [<- <-]; constructor.
  - move=> C' b1 b2 t1 t3 t2 _ IH1 _ IH2 _ _ _ He t1' t2' [<- <-].
    case: (cat_nil He) => Ha Hb.
    exact: vsub_trans (IH1 Ha _ _ erefl) (IH2 Hb _ _ erefl).
  - move=> C' b' t1' t2' t1 t2 _ IH Hs1 Hs2 _ _ He t1'' t2'' [<- <-].
    exact: vsub_trans (rsub_vsub Hs1) (vsub_trans (IH He _ _ erefl) (rsub_vsub Hs2)).
  - move=> C' b' t t1 t2 _ IH _ _ He t1' t2' [<- <-].
    exact: vsub_cat (vsub_refl t) (IH He _ _ erefl).
Qed.

(* A derivation for one instruction is its typing rule, framed and weakened. *)
Lemma ok_one_inv : forall C b ft, Instrs_ok C b ft -> forall i, b = [:: i] ->
  forall t1 t2, ft = fn t1 t2 ->
  exists ts a r, Instr_ok C i (fn a r) /\ vsub t1 (ts ++ a) /\ vsub (ts ++ r) t2.
Proof.
  move=> C b ft; elim=> //.
  - move=> C' i' t1 t2 Hi _ _ i [<-] _ _ [<- <-].
    exists [::], t1, t2; split=> //; split; exact: vsub_refl.
  - move=> C' b1 b2 t1 t3 t2 H1 IH1 H2 IH2 _ _ _ i He t1' t3' [<- <-].
    case: (cat_one He) => [[Hl Hr] | [Hl Hr]].
    + have [ts [a [r [Hi [Ha Ho]]]]] := IH2 _ Hr _ _ erefl.
      exists ts, a, r; split=> //; split=> //.
      exact: vsub_trans (ok_nil_inv H1 Hl erefl) Ha.
    + have [ts [a [r [Hi [Ha Ho]]]]] := IH1 _ Hl _ _ erefl.
      exists ts, a, r; split=> //; split=> //.
      exact: vsub_trans Ho (ok_nil_inv H2 Hr erefl).
  - move=> C' b' t1' t2' t1 t2 _ IH Hs1 Hs2 _ _ i He t1'' t2'' [<- <-].
    have [ts [a [r [Hi [Ha Ho]]]]] := IH _ He _ _ erefl.
    exists ts, a, r; split=> //; split.
    + exact: vsub_trans (rsub_vsub Hs1) Ha.
    + exact: vsub_trans Ho (rsub_vsub Hs2).
  - move=> C' b' t t1 t2 _ IH _ _ i He t1' t2' [<- <-].
    have [ts [a [r [Hi [Ha Ho]]]]] := IH _ He _ _ erefl.
    exists (t ++ ts), a, r; split=> //; rewrite -!catA; split; exact: vsub_cat (vsub_refl t) _.
Qed.

(* Two ways of splitting one list: one split point is at or before the other. *)
Lemma cat_eq_cat : forall {A : Type} (c1 c2 b1 b2 : seq A), c1 ++ c2 = b1 ++ b2 ->
  (exists m, b1 = c1 ++ m /\ c2 = m ++ b2) \/ (exists m, c1 = b1 ++ m /\ b2 = m ++ c2).
Proof.
  move=> A; elim=> [ | x c1 IH] c2 [ | y b1] b2 //= He.
  - by left; exists [::].
  - by left; exists (y :: b1).
  - by right; exists (x :: c1).
  - case: He => <- He.
    case: (IH _ _ _ He) => [[m [-> ->]] | [m [-> ->]]].
    + by left; exists m.
    + by right; exists m.
Qed.

(* Any derivation for `b1 ++ b2` goes through some stack in the middle. *)
Lemma ok_split : forall C b ft, Instrs_ok C b ft -> forall b1 b2 t1 t3, b = b1 ++ b2 ->
  ft = fn t1 t3 -> exists t2, Instrs_ok C b1 (fn t1 t2) /\ Instrs_ok C b2 (fn t2 t3).
Proof.
  move=> C b ft; elim.
  - move=> C' Hc b1 b2 _ _ He [<- <-].
    case: (cat_nil (esym He)) => -> ->.
    by exists [::]; split; apply: empty.
  - move=> C' i t1 t2 Hi Hc Hw b1 b2 t1' t3' He [<- <-].
    have H := Instrs_ok__instr _ _ _ _ Hi Hc Hw.
    case: (cat_one (esym He)) => [[-> ->] | [-> ->]].
    + by exists t1; split=> //; apply: ok_nil Hc (vsub_refl _).
    + by exists t2; split=> //; apply: ok_nil Hc (vsub_refl _).
  - move=> C' c1 c2 t1 t3 t2 H1 IH1 H2 IH2 Hc _ _ b1 b2 t1' t3' He [<- <-].
    case: (cat_eq_cat He) => [[m [-> Hm]] | [m [Hm ->]]].
    + have [tm [Hl Hr]] := IH2 _ _ _ _ Hm erefl.
      exists tm; split=> //.
      exact: ok_seq H1 Hl.
    + have [tm [Hl Hr]] := IH1 _ _ _ _ Hm erefl.
      exists tm; split=> //.
      exact: ok_seq Hr H2.
  - move=> C' b' t1' t2' t1 t2 _ IH Hs1 Hs2 _ _ b1 b2 t1'' t3 He [<- <-].
    have [tm [Hl Hr]] := IH _ _ _ _ He erefl.
    exists tm; split.
    + exact: ok_sub Hl (rsub_vsub Hs1) (vsub_refl _).
    + exact: ok_sub Hr (vsub_refl _) (rsub_vsub Hs2).
  - move=> C' b' t t1 t2 _ IH _ _ b1 b2 t1' t3 He [<- <-].
    have [tm [Hl Hr]] := IH _ _ _ _ He erefl.
    by exists (t ++ tm); split; apply: ok_frame.
Qed.

(* Derivations of two instructions, with the stack between them left out. *)
Lemma ok_two : forall C i j t1 t2, Instrs_ok C [:: i; j] (fn t1 t2) ->
  exists ts a r ts' a' r', Instr_ok C i (fn a r) /\ Instr_ok C j (fn a' r') /\
    vsub t1 (ts ++ a) /\ vsub (ts ++ r) (ts' ++ a') /\ vsub (ts' ++ r') t2.
Proof.
  move=> C i j t1 t2 H.
  have [tm [Hl Hr]] := @ok_split _ _ _ H [:: i] [:: j] _ _ erefl erefl.
  have [ts [a [r [Hi [Ha Ho]]]]] := ok_one_inv Hl erefl erefl.
  have [ts' [a' [r' [Hj [Ha' Ho']]]]] := ok_one_inv Hr erefl erefl.
  exists ts, a, r, ts', a', r'; do 4?split=> //.
  exact: vsub_trans Ho Ha'.
Qed.

Lemma vsub_snoc : forall a b x y, vsub (a ++ [:: x]) (b ++ [:: y]) -> vsub a b.
Proof.
  move=> a b x y H; apply: (vsub_prefix _ H).
  have := vsub_size H.
  by rewrite !cats1 !size_rcons; case.
Qed.

Local Notation local_of C x := ((context_LOCALS C)[| (x :> N) |]).

Lemma get_inv : forall C x a r, Instr_ok C (LOCAL_GET x) (fn a r) ->
  a = [::] /\ r = [:: local_of C x].
Proof.
  move=> C x a r H; inversion H; subst.
  match goal with Hq : is_true (_ == _) |- _ => by rewrite (eqP Hq) end.
Qed.

Lemma set_inv : forall C x a r, Instr_ok C (LOCAL_SET x) (fn a r) ->
  a = [:: local_of C x] /\ r = [::] /\ Instr_ok C (LOCAL_TEE x) (fn a a).
Proof.
  move=> C x a r H; inversion H; subst.
  match goal with Hq : is_true (_ == _) |- _ => rewrite (eqP Hq) end.
  do 2?split=> //.
  apply: local_tee => //.
  match goal with Hw : wf_instr _ |- _ => inversion Hw; subst end.
  by constructor.
Qed.

Lemma tee_inv : forall C x a r, Instr_ok C (LOCAL_TEE x) (fn a r) ->
  a = [:: local_of C x] /\ r = a /\ Instr_ok C (LOCAL_SET x) (fn a [::]).
Proof.
  move=> C x a r H; inversion H; subst.
  match goal with Hq : is_true (_ == _) |- _ => rewrite (eqP Hq) end.
  do 2?split=> //.
  apply: local_set => //.
  match goal with Hw : wf_instr _ |- _ => inversion Hw; subst end.
  by constructor.
Qed.

Lemma drop_inv : forall C a r, Instr_ok C DROP (fn a r) -> exists t, a = [:: t] /\ r = [::].
Proof. by move=> C a r H; inversion H; subst; eexists. Qed.

Lemma ok_drop : forall C t, wf_context C -> Instr_ok C DROP (fn [:: t] [::]).
Proof. by move=> C t Hc; apply: drop => //; constructor. Qed.

Lemma global_get_inv : forall C x a r, Instr_ok C (GLOBAL_GET x) (fn a r) ->
  a = [::] /\ exists t, r = [:: t].
Proof. by move=> C x a r H; inversion H; subst; split=> //; eexists. Qed.

Lemma const_inv : forall C t c a r, Instr_ok C (CONST t c) (fn a r) ->
  a = [::] /\ exists t', r = [:: t'].
Proof. by move=> C t c a r H; inversion H; subst; split=> //; eexists. Qed.

(* Pushing one value and dropping it does nothing to the stack. *)
Lemma push_drop_ok : forall C i t1 t2,
  (forall a r, Instr_ok C i (fn a r) -> a = [::] /\ exists t, r = [:: t]) ->
  Instrs_ok C [:: i; DROP] (fn t1 t2) -> Instrs_ok C [::] (fn t1 t2).
Proof.
  move=> C i t1 t2 Hp H.
  have [ts [a [r [ts' [a' [r' [Hi [Hd [H1 [H2 H3]]]]]]]]]] := ok_two H.
  have [? [t ?]] := Hp _ _ Hi; subst a r.
  have [t' [? ?]] := drop_inv Hd; subst a' r'.
  rewrite cats0 in H1; rewrite cats0 in H3.
  apply: (ok_nil (ok_ctx H)).
  exact: vsub_trans H1 (vsub_trans (vsub_snoc H2) H3).
Qed.

Lemma ilr_ok : forall k D P Q, ilr k D P Q ->
  forall C t1 t2, Instrs_ok C P (fn t1 t2) -> Instrs_ok C Q (fn t1 t2).
Proof.
  move=> k D P Q; case=> [x _ | x | x _ | x _ | y | y | t c] C t1 t2 H.
  - have [ts [a [r [ts' [a' [r' [Hs [Hg [H1 [H2 H3]]]]]]]]]] := ok_two H.
    have [? [? Ht]] := set_inv Hs; subst a r.
    have [? ?] := get_inv Hg; subst a' r'.
    rewrite !cats0 in H2.
    exact: ok_one Ht H1 (vsub_trans (vsub_cat H2 (vsub_refl _)) H3).
  - have [ts [a [r [ts' [a' [r' [Ht [Hd [H1 [H2 H3]]]]]]]]]] := ok_two H.
    have [? [? Hs]] := tee_inv Ht; subst a r.
    have [t' [? ?]] := drop_inv Hd; subst a' r'.
    rewrite cats0 in H3.
    apply: ok_one Hs H1 _.
    rewrite cats0.
    exact: vsub_trans (vsub_snoc H2) H3.
  - have [ts [a [r [Ht [H1 H2]]]]] := ok_one_inv H erefl erefl.
    have [_ [? _]] := tee_inv Ht; subst r.
    exact: ok_nil (ok_ctx H) (vsub_trans H1 H2).
  - have [ts [a [r [Hs [H1 H2]]]]] := ok_one_inv H erefl erefl.
    have [? [? _]] := set_inv Hs; subst a r.
    exact: ok_one (ok_drop _ (ok_ctx H)) H1 H2.
  - exact: push_drop_ok (fun a r Hi => let: conj Ha Hr := get_inv Hi in conj Ha (ex_intro _ _ Hr)) H.
  - exact: push_drop_ok (fun a r Hi => global_get_inv Hi) H.
  - exact: push_drop_ok (fun a r Hi => const_inv Hi) H.
Qed.

Scheme icong_min := Minimality for icong Sort Prop
  with ielem_min := Minimality for ielem Sort Prop.
Combined Scheme icong_ielem_min from icong_min, ielem_min.

Lemma wf_block : forall bt b1 b2, wf_instr (BLOCK bt b1) -> List.Forall wf_instr b2 ->
  wf_instr (BLOCK bt b2).
Proof. by move=> bt b1 b2 H Hb; inversion H; subst; constructor. Qed.

Lemma wf_loop : forall bt b1 b2, wf_instr (LOOP bt b1) -> List.Forall wf_instr b2 ->
  wf_instr (LOOP bt b2).
Proof. by move=> bt b1 b2 H Hb; inversion H; subst; constructor. Qed.

Lemma wf_if : forall bt b1 c1 b2 c2, wf_instr (IFELSE bt b1 c1) -> List.Forall wf_instr b2 ->
  List.Forall wf_instr c2 -> wf_instr (IFELSE bt b2 c2).
Proof. by move=> bt b1 c1 b2 c2 H Hb Hc; inversion H; subst; constructor. Qed.

Lemma icong_ielem_ok : forall k D,
  (forall b1 b2, icong k D b1 b2 ->
    forall C t1 t2, Instrs_ok C b1 (fn t1 t2) -> Instrs_ok C b2 (fn t1 t2)) /\
  (forall i1 i2, ielem k D i1 i2 ->
    forall C t1 t2, Instr_ok C i1 (fn t1 t2) -> Instr_ok C i2 (fn t1 t2)).
Proof.
  move=> k D; apply: icong_ielem_min.
  - done.
  - move=> i1 i2 b1 b2 _ IHi _ IHb C t1 t2 H.
    have [tm [Hl Hr]] := @ok_split _ _ _ H [:: i1] b1 _ _ erefl erefl.
    have [ts [a [r [Hi [Ha Ho]]]]] := ok_one_inv Hl erefl erefl.
    exact: ok_seq (ok_one (IHi _ _ _ Hi) Ha Ho) (IHb _ _ _ Hr).
  - move=> P Q b1 b2 Hp _ IHb C t1 t2 H.
    have [tm [Hl Hr]] := ok_split H erefl erefl.
    exact: ok_seq (ilr_ok Hp Hl) (IHb _ _ _ Hr).
  - done.
  - move=> bt b1 b2 _ IH C t1 t2 H.
    inversion H; subst.
    match goal with Hb : Instrs_ok _ b1 _ |- _ => have Hb2 := IH _ _ _ Hb end.
    apply: block => //.
    match goal with Hw : wf_instr (BLOCK _ _) |- _ => exact: wf_block Hw (ok_wf Hb2) end.
  - move=> bt b1 b2 _ IH C t1 t2 H.
    inversion H; subst.
    match goal with Hb : Instrs_ok _ b1 _ |- _ => have Hb2 := IH _ _ _ Hb end.
    apply: loop => //.
    match goal with Hw : wf_instr (LOOP _ _) |- _ => exact: wf_loop Hw (ok_wf Hb2) end.
  - move=> bt b1 b2 c1 c2 _ IHb _ IHc C t1 t2 H.
    inversion H; subst.
    match goal with Hb : Instrs_ok _ b1 _ |- _ => have Hb2 := IHb _ _ _ Hb end.
    match goal with Hc : Instrs_ok _ c1 _ |- _ => have Hc2 := IHc _ _ _ Hc end.
    apply: res_if => //.
    match goal with Hw : wf_instr (IFELSE _ _ _) |- _ => exact: wf_if Hw (ok_wf Hb2) (ok_wf Hc2) end.
Qed.

Theorem instrs_ok_rewrite : forall k D b1 b2 C t1 t2, icong k D b1 b2 ->
  Instrs_ok C b1 (fn t1 t2) -> Instrs_ok C b2 (fn t1 t2).
Proof. move=> k D b1 b2 C t1 t2 Hc; exact: (proj1 (icong_ielem_ok k D) _ _ Hc). Qed.

(* A function validates at the same type after the rewrite. *)
Theorem func_ok_rewrite : forall k D C x ls b1 b2 ft, icong k D b1 b2 ->
  Func_ok C (func_FUNC x ls b1) ft -> Func_ok C (func_FUNC x ls b2) ft.
Proof.
  move=> k D C x ls b1 b2 ft Hc H.
  inversion H; subst.
  match goal with He : Expr_ok _ _ _ |- _ => inversion He; subst end.
  match goal with Hb : Instrs_ok _ b1 _ |- _ => have Hb2 := instrs_ok_rewrite Hc Hb end.
  have Hw := ok_wf Hb2.
  apply: mk_Func_ok => //.
  match goal with Hf : wf_func _ |- _ => inversion Hf; subst end.
  by constructor.
Qed.

(* Two functions the same but for a rewritten body. *)
Definition func_rewrite (f1 f2 : func) : Prop :=
  exists x ls b1 b2 k D, f1 = func_FUNC x ls b1 /\ f2 = func_FUNC x ls b2 /\ icong k D b1 b2.

(* Every rewrite `invoke_refines` covers is one of these. *)
Lemma code_rel_rewrite : forall fi1 fi2, code_rel fi1 fi2 -> func_rewrite (CODE fi1) (CODE fi2).
Proof.
  move=> fi1 fi2 [_ [_ [x [ls [b1 [b2 [H1 [H2 Hb]]]]]]]].
  case E: (funcinst_TYPE fi1) => [[t1] [t2]].
  have [D Hc] := Hb _ _ E.
  by exists x, ls, b1, b2, (size t1 + size ls)%nat, D.
Qed.

Lemma func_ok_rewrites : forall C fts fs1 fs2, List.Forall2 func_rewrite fs1 fs2 ->
  List.Forall2 (fun ft f => Func_ok C f ft) fts fs1 ->
  List.Forall2 (fun ft f => Func_ok C f ft) fts fs2.
Proof.
  move=> C fts fs1 fs2 Hr; elim: Hr fts => [ | f1 f2 fs1' fs2' Hf _ IH] fts H.
  - by inversion H.
  - inversion H as [ | ft f ftl fl Hok Hl]; subst.
    case: Hf => [x [ls [b1 [b2 [k [D [? [? Hc]]]]]]]]; subst.
    constructor; first exact: func_ok_rewrite Hc Hok.
    exact: IH Hl.
Qed.

Lemma funcs_wf : forall C fts fs, List.Forall2 (fun ft f => Func_ok C f ft) fts fs ->
  List.Forall wf_func fs.
Proof.
  move=> C fts fs; elim=> // ft f ? ? Hf _ IH.
  constructor=> //.
  by inversion Hf.
Qed.

(* A module validates after the rewrite of any of its function bodies. *)
Theorem module_ok_rewrite : forall ts ims fs1 fs2 gs tbs ms es ds st xs,
  List.Forall2 func_rewrite fs1 fs2 ->
  Module_ok (MODULE ts ims fs1 gs tbs ms es ds st xs) ->
  Module_ok (MODULE ts ims fs2 gs tbs ms es ds st xs).
Proof.
  move=> ts ims fs1 fs2 gs tbs ms es ds st xs Hr H.
  inversion H; subst.
  match goal with Hf : List.Forall2 _ _ fs1 |- _ =>
    have Hf2 := func_ok_rewrites Hr Hf end.
  have Hs : size fs1 = size fs2.
  { by elim: Hr {H Hf2} => //= ? ? ? ? _ _ ->. }
  eapply mk_Module_ok; try eassumption.
  - by rewrite -Hs.
  - match goal with Hm : wf_module _ |- _ => inversion Hm; subst end.
    constructor=> //.
    exact: funcs_wf Hf2.
Qed.
