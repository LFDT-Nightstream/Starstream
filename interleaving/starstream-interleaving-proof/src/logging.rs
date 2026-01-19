use ark_relations::gr1cs::{ConstraintLayer, TracingMode};
use tracing_subscriber::{Registry, fmt, layer::SubscriberExt as _};

pub(crate) fn setup_logger() {
    static INIT: std::sync::Once = std::sync::Once::new();

    INIT.call_once(|| {
        let constraint_layer = ConstraintLayer::new(TracingMode::All);

        let subscriber = Registry::default()
            .with(fmt::layer().with_test_writer())
            .with(constraint_layer);

        tracing::subscriber::set_global_default(subscriber)
            .expect("Failed to set global default subscriber");
    });
}
