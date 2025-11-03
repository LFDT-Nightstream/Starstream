use tracing_subscriber::{EnvFilter, fmt};

pub(crate) fn init_test_logging() {
    static INIT: std::sync::Once = std::sync::Once::new();

    INIT.call_once(|| {
        fmt()
            .with_env_filter(
                EnvFilter::from_default_env()
                    .add_directive("starstream_ivc_proto=debug".parse().unwrap())
                    .add_directive("warn".parse().unwrap()) // Default to warn for everything else
            )
            .with_test_writer()
            .init();
    });
}
