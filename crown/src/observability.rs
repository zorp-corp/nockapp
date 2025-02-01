use std::net::SocketAddr;

const JAEGER_ENDPOINT_ENV: &str = "OTEL_EXPORTER_JAEGER_ENDPOINT";

pub fn init_prometheus() {
    // Set up the Prometheus exporter to listen on port 9000.
    metrics_exporter_prometheus::PrometheusBuilder::new()
        .with_http_listener("127.0.0.1:9000".parse::<SocketAddr>().unwrap())
        .install()
        .expect("failed to install Prometheus exporter");
}

pub fn init_tracing() -> Result<impl tracing::Subscriber, opentelemetry::trace::TraceError> {
    use opentelemetry::trace::TracerProvider;
    use opentelemetry_otlp::WithExportConfig;
    use tracing_subscriber::layer::SubscriberExt;

    let endpoint = std::env::var(JAEGER_ENDPOINT_ENV).unwrap_or("http://localhost:4317".to_owned());
    eprintln!("OTLP gRPC endpoint: {}", endpoint);
    let otlp_exporter = opentelemetry_otlp::SpanExporter::builder()
        .with_tonic()
        .with_endpoint(endpoint)
        .build()
        .unwrap();

    let provider = opentelemetry_sdk::trace::TracerProvider::builder()
        .with_batch_exporter(otlp_exporter, opentelemetry_sdk::runtime::Tokio)
        .build();

    let tracer = provider.tracer("crown");
    let telemetry = tracing_opentelemetry::layer().with_tracer(tracer);
    let fmt_layer = tracing_subscriber::fmt::layer()
        .with_target(true)
        .with_thread_ids(true)
        .with_line_number(true);

    let subscriber = tracing_subscriber::Registry::default()
        .with(tracing_subscriber::EnvFilter::from_default_env())
        .with(fmt_layer)
        .with(telemetry);
    Ok(subscriber)
}
