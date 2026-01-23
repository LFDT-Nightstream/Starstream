use crate::F;
use ark_ff::PrimeField as _;
use ark_r1cs_std::{
    GR1CSVar as _,
    alloc::AllocVar as _,
    eq::EqGadget as _,
    fields::{FieldVar as _, fp::FpVar},
    prelude::{Boolean, ToBitsGadget as _},
};
use ark_relations::gr1cs::{ConstraintSystemRef, SynthesisError};
use std::ops::Not;

pub(crate) fn alloc_rem_one_hot_selectors<const DIVISOR: usize>(
    cs: &ConstraintSystemRef<F>,
    dividend: &FpVar<F>,
    switch: &Boolean<F>,
) -> Result<[Boolean<F>; DIVISOR], SynthesisError> {
    let d_val = dividend.value()?.into_bigint().0[0];

    let divisor = FpVar::new_constant(cs.clone(), F::from(DIVISOR as u64))?;

    let quotient = FpVar::new_witness(cs.clone(), || Ok(F::from(d_val / DIVISOR as u64)))?;
    let remainder = FpVar::new_witness(cs.clone(), || Ok(F::from(d_val % DIVISOR as u64)))?;

    let recomposed = &quotient * &divisor + &remainder;

    recomposed.conditional_enforce_equal(dividend, &switch)?;

    let quotient_nonzero = quotient.is_zero()?.not();

    enforce_32_bit(dividend, &switch)?;
    enforce_32_bit(&quotient, &switch)?;

    let r_eq: [Boolean<F>; DIVISOR] = std::array::from_fn(|i| {
        let c = FpVar::new_constant(cs.clone(), F::from(i as u64)).unwrap();

        (&remainder - c).is_zero().unwrap()
    });

    let r_eq_any = Boolean::kary_or(&r_eq)?;
    r_eq_any.conditional_enforce_equal(&Boolean::TRUE, &switch)?;

    let ref_push_lane_switches = std::array::from_fn(|i| {
        let mut r_gt = Boolean::FALSE;

        // we set index 0 if
        //
        //   r == 1, which is r_eq[1] == true
        //   r == 2, which is r_eq[2] == true
        //
        // and so on
        for k in (i + 1)..DIVISOR {
            r_gt = &r_gt | &r_eq[k];
        }

        // but we only filter when the quotient is nonzero, since otherwise we
        // still have elements
        let in_range = &quotient_nonzero | &r_gt;

        switch & &in_range
    });

    Ok(ref_push_lane_switches)
}

fn enforce_32_bit(var: &FpVar<F>, switch: &Boolean<F>) -> Result<(), SynthesisError> {
    let bits = var.to_bits_le()?;
    for bit in bits.iter().skip(32) {
        bit.conditional_enforce_equal(&Boolean::FALSE, switch)?;
    }
    Ok(())
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    pub fn test_rem_one_hot_5_mod_7() {
        use ark_relations::gr1cs::ConstraintSystem;

        let cs = ConstraintSystem::<F>::new_ref();

        let dividend = FpVar::new_witness(cs.clone(), || Ok(F::from(5))).unwrap();

        let res = alloc_rem_one_hot_selectors::<7>(&cs, &dividend, &Boolean::TRUE).unwrap();

        assert_eq!(res[0].value().unwrap(), true);
        assert_eq!(res[1].value().unwrap(), true);
        assert_eq!(res[2].value().unwrap(), true);
        assert_eq!(res[3].value().unwrap(), true);
        assert_eq!(res[4].value().unwrap(), true);

        assert_eq!(res[5].value().unwrap(), false);
        assert_eq!(res[6].value().unwrap(), false);

        assert_eq!(res.len(), 7);
    }

    #[test]
    pub fn test_rem_one_hot_7_mod_7() {
        use ark_relations::gr1cs::ConstraintSystem;

        let cs = ConstraintSystem::<F>::new_ref();

        let dividend = FpVar::new_witness(cs.clone(), || Ok(F::from(7))).unwrap();

        let res = alloc_rem_one_hot_selectors::<7>(&cs, &dividend, &Boolean::TRUE).unwrap();

        assert_eq!(res[0].value().unwrap(), true);
        assert_eq!(res[1].value().unwrap(), true);
        assert_eq!(res[2].value().unwrap(), true);
        assert_eq!(res[3].value().unwrap(), true);
        assert_eq!(res[4].value().unwrap(), true);
        assert_eq!(res[5].value().unwrap(), true);
        assert_eq!(res[6].value().unwrap(), true);

        assert_eq!(res.len(), 7);
    }

    #[test]
    pub fn test_rem_one_hot_3_mod_5() {
        use ark_relations::gr1cs::ConstraintSystem;

        let cs = ConstraintSystem::<F>::new_ref();

        let dividend = FpVar::new_witness(cs.clone(), || Ok(F::from(3))).unwrap();

        let res = alloc_rem_one_hot_selectors::<5>(&cs, &dividend, &Boolean::TRUE).unwrap();

        assert_eq!(res[0].value().unwrap(), true);
        assert_eq!(res[1].value().unwrap(), true);
        assert_eq!(res[2].value().unwrap(), true);
        assert_eq!(res[3].value().unwrap(), false);
        assert_eq!(res[4].value().unwrap(), false);

        assert_eq!(res.len(), 5);
    }
}
