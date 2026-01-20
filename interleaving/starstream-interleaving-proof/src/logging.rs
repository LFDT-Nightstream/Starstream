use ark_relations::gr1cs::{ConstraintLayer, TracingMode};
use tracing_subscriber::Layer;
use tracing_subscriber::{EnvFilter, Registry, fmt, layer::SubscriberExt as _};

pub(crate) fn setup_logger() {
    static INIT: std::sync::Once = std::sync::Once::new();

    INIT.call_once(|| {
        let constraint_layer = ConstraintLayer::new(TracingMode::All);
        let env_filter = EnvFilter::try_from_default_env()
            .unwrap_or_else(|_| EnvFilter::new("starstream_interleaving_proof=debug,g1rcs=off"));

        let fmt_layer = if cfg!(test) {
            fmt::layer().with_test_writer().boxed()
        } else {
            fmt::layer().boxed()
        }
        .with_filter(env_filter);

        let subscriber = Registry::default().with(fmt_layer).with(constraint_layer);

        tracing::subscriber::set_global_default(subscriber)
            .expect("Failed to set global default subscriber");
    });
}
