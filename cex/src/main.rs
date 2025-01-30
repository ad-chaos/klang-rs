use axum::{response::IntoResponse, routing::get, Router};

#[tokio::main]
async fn main() {
    let app = Router::new().route("/", get(highlight));
    let listener = tokio::net::TcpListener::bind("0.0.0.0:25565")
        .await
        .unwrap();

    axum::serve(listener, app).await.unwrap()
}

async fn highlight(body: String) -> impl IntoResponse {
    println!("{}", body);

    "ok"
}
