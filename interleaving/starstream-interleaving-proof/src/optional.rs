use ark_ff::PrimeField;
use ark_r1cs_std::{GR1CSVar, boolean::Boolean, fields::{FieldVar, fp::FpVar}};
use ark_relations::gr1cs::SynthesisError;

#[derive(Copy, Clone, Debug, Default)]
pub struct OptionalF<F: PrimeField>(F);

impl<F: PrimeField> OptionalF<F> {
    pub fn none() -> Self {
        Self(F::ZERO)
    }

    pub fn from_pid(value: F) -> Self {
        Self(value + F::ONE)
    }

    pub fn from_option(value: Option<F>) -> Self {
        value.map(Self::from_pid).unwrap_or_else(Self::none)
    }

    pub fn encoded(self) -> F {
        self.0
    }

    pub fn from_encoded(value: F) -> Self {
        Self(value)
    }

    pub fn to_option(self) -> Option<F> {
        if self.0 == F::ZERO {
            None
        } else {
            Some(self.0 - F::ONE)
        }
    }

    pub fn decode_or_zero(self) -> F {
        self.to_option().unwrap_or(F::ZERO)
    }
}

#[derive(Clone)]
pub struct OptionalFpVar<F: PrimeField>(FpVar<F>);

impl<F: PrimeField> OptionalFpVar<F> {
    pub fn new(value: FpVar<F>) -> Self {
        Self(value)
    }

    pub fn encoded(&self) -> FpVar<F> {
        self.0.clone()
    }

    pub fn is_some(&self) -> Result<Boolean<F>, SynthesisError> {
        Ok(!self.0.is_zero()?)
    }

    pub fn decode_or_zero(&self, one: &FpVar<F>) -> Result<FpVar<F>, SynthesisError> {
        let is_zero = self.0.is_zero()?;
        let value = &self.0 - one;
        is_zero.select(&FpVar::zero(), &value)
    }

    pub fn select_encoded(
        switch: &Boolean<F>,
        when_true: &FpVar<F>,
        when_false: &OptionalFpVar<F>,
    ) -> Result<OptionalFpVar<F>, SynthesisError> {
        let selected = switch.select(when_true, &when_false.encoded())?;
        Ok(OptionalFpVar::new(selected))
    }

    pub fn value(&self) -> Result<F, SynthesisError> {
        self.0.value()
    }
}
