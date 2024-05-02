use std::convert::Infallible;
use std::net::SocketAddr;

use bytes::Bytes;
use http_body_util::combinators::BoxBody;
use hyper::body::Incoming;
use hyper::server::conn::http1;
use hyper::{Request, Response};
use hyper_content_encoding::{full, Compressor};
use hyper_util::rt::TokioIo;
use tokio::net::TcpListener;
use tower::ServiceBuilder;

async fn hello(_: Request<Incoming>) -> Result<Response<BoxBody<Bytes, hyper::Error>>, Infallible> {
    Ok(Response::new(full(Bytes::from("Hello, World!\n"))))
}

#[tokio::main]
pub async fn main() -> Result<(), Box<dyn std::error::Error + Send + Sync>> {
    let addr: SocketAddr = ([127, 0, 0, 1], 3000).into();

    let listener = TcpListener::bind(addr).await?;
    println!("Listening on http://{}", addr);

    loop {
        let (tcp, _) = listener.accept().await?;
        let io = TokioIo::new(tcp);

        tokio::task::spawn(async move {
            let svc = hyper::service::service_fn(hello);

            let svc = ServiceBuilder::new().layer_fn(Compressor::new).service(svc);
            if let Err(err) = http1::Builder::new().serve_connection(io, svc).await {
                println!("Error serving connection: {:?}", err);
            }
        });
    }
}
