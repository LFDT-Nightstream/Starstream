use ark_relations::gr1cs::{ConstraintLayer, TracingMode};
use tracing_subscriber::{Registry, fmt, layer::SubscriberExt as _};

pub(crate) fn init_test_logging() {
    static INIT: std::sync::Once = std::sync::Once::new();

    INIT.call_once(|| {
        let constraint_layer = ConstraintLayer::new(TracingMode::All);

        let subscriber = Registry::default()
            .with(fmt::layer().with_test_writer())
            // .with(
            //     EnvFilter::from_default_env()
            //         .add_directive("starstream_ivc_proto=debug".parse().unwrap())
            //         .add_directive("warn".parse().unwrap()), // Default to warn for everything else
            // )
            .with(constraint_layer);

        tracing::subscriber::set_global_default(subscriber)
            .expect("Failed to set global default subscriber");
    });
}
