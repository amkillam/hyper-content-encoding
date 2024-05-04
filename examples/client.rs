use std::env;

use bytes::Bytes;
use http::HeaderValue;
use http_body_util::Empty;
use hyper::Request;
use hyper_content_encoding::response_to_string;
use hyper_util::rt::TokioIo;
use tokio::net::TcpStream;

type Result<T> = std::result::Result<T, Box<dyn std::error::Error + Send + Sync>>;

#[tokio::main]
async fn main() -> Result<()> {
    let url = match env::args().nth(1) {
        Some(url) => url,
        None => {
            println!("Usage: client <url>");
            return Ok(());
        }
    };

    let url = url.parse::<hyper::Uri>().unwrap();
    if url.scheme_str() != Some("http") {
        println!("This example only works with 'http' URLs.");
        return Ok(());
    }

    fetch_url(url).await
}

async fn fetch_url(url: hyper::Uri) -> Result<()> {
    let host = url.host().expect("uri has no host");
    let port = url.port_u16().unwrap_or(80);
    let addr = format!("{}:{}", host, port);
    let stream = TcpStream::connect(addr).await?;
    let io = TokioIo::new(stream);

    let (mut sender, conn) = hyper::client::conn::http1::handshake(io).await?;
    tokio::task::spawn(async move {
        if let Err(err) = conn.await {
            println!("Connection failed: {:?}", err);
        }
    });

    let authority = url.authority().unwrap().clone();

    let path = url.path();
    let req = Request::builder()
        .uri(path)
        .header(hyper::header::HOST, authority.as_str())
        .header(
            hyper::header::ACCEPT_ENCODING,
            HeaderValue::from_static("deflate, gzip"),
        )
        .body(Empty::<Bytes>::new())?;

    let res = sender.send_request(req).await?;

    println!("Response: {}", res.status());
    println!("Headers: {:#?}\n", res.headers());

    match response_to_string(res).await {
        Ok(s) => println!("Body (str):\n{:}", s),
        Err(e) => println!("Error {:?}", e),
    };

    println!("\n\nDone!");

    Ok(())
}
