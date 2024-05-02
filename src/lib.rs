use bytes::Bytes;
use core::fmt;
use flate2::{
    read::{DeflateDecoder, DeflateEncoder, GzDecoder, GzEncoder, ZlibDecoder},
    Compression,
};
use http::{
    header::{CONTENT_ENCODING, CONTENT_LENGTH, CONTENT_TYPE},
    HeaderValue, Request, Response,
};
use http_body_util::{combinators::BoxBody, BodyExt, Full};
use hyper::{body::Incoming, service::Service};
use std::io::prelude::*;
use std::{fmt::Debug, future::Future, pin::Pin};

type Result<T> = std::result::Result<T, ResponseToStringError>;

#[derive(Debug, Clone)]
pub struct ResponseToStringError {
    inner: String,
}

impl std::error::Error for ResponseToStringError {}

impl ResponseToStringError {
    pub fn new(inner: String) -> Self {
        ResponseToStringError { inner }
    }
}

impl From<String> for ResponseToStringError {
    fn from(value: String) -> Self {
        ResponseToStringError::new(value)
    }
}

impl fmt::Display for ResponseToStringError {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}", self.inner)
    }
}

fn convert_err<E>(e: E) -> ResponseToStringError
where
    E: Debug,
{
    format!("{:?}", e).into()
}

trait Decoder<T>: Read
where
    T: Read,
{
    fn new(_: T) -> Self;
}

impl<R> Decoder<R> for GzDecoder<R>
where
    R: Read,
{
    fn new(reader: R) -> Self {
        GzDecoder::new(reader)
    }
}

impl<R> Decoder<R> for DeflateDecoder<R>
where
    R: Read,
{
    fn new(reader: R) -> Self {
        DeflateDecoder::new(reader)
    }
}

impl<R> Decoder<R> for ZlibDecoder<R>
where
    R: Read,
{
    fn new(reader: R) -> Self {
        ZlibDecoder::new(reader)
    }
}

fn decompress<'a, T>(body: &'a Bytes) -> Result<String>
where
    T: Decoder<&'a [u8]>,
{
    let reader: &[u8] = body;
    let mut decoder = T::new(reader);
    let mut s = String::new();
    decoder.read_to_string(&mut s).map_err(convert_err)?;
    Ok(s)
}

/// Extracts the body of a response as a String.
/// The response *must* contain a `Content-Type` with the word `text`
///
/// Currently the only handled `Content-Encodings` that are supported are
/// - identity
/// - x-gzip
/// - gzip
/// - deflate
pub async fn response_to_string(res: Response<Incoming>) -> Result<String> {
    if let Some(content_type) = res.headers().get(CONTENT_TYPE) {
        if content_type.to_str().map_err(convert_err)?.contains("text") {
            let content_encoding = match res.headers().get(CONTENT_ENCODING) {
                Some(content_encoding) => {
                    content_encoding.to_str().map_err(convert_err)?.to_owned()
                }
                None => "identity".to_owned(),
            };

            let res = res.map(|b| b.boxed());
            let body: Bytes = res.collect().await.map_err(convert_err)?.to_bytes();

            match content_encoding.as_str() {
                "identity" => {
                    let body = String::from_utf8_lossy(&body).to_string();
                    Ok(body)
                }

                "x-gzip" => decompress::<GzDecoder<_>>(&body),
                "gzip" => decompress::<GzDecoder<_>>(&body),

                "compress" => {
                    todo!()
                }

                "deflate" => decompress::<DeflateDecoder<_>>(&body),

                _ => Err(format!("Unknown Content-Type {content_encoding}").into()),
            }
        } else {
            Err("Content-Type does not specify text".to_string().into())
        }
    } else {
        Err("No Content-Type specified".to_string().into())
    }
}

/// An enum describing the different available encoding types
#[derive(Debug, Clone)]
pub enum Encoding {
    /// gzip
    Gzip,

    /// deflate
    Deflate,
}

impl Encoding {
    fn as_header_value(&self) -> HeaderValue {
        match &self {
            Encoding::Gzip => HeaderValue::from_static("gzip"),
            Encoding::Deflate => HeaderValue::from_static("deflate"),
        }
    }
}
/// Creates a boxed body from a Byte like type
pub fn full<T: Into<Bytes>>(chunk: T) -> BoxBody<Bytes, hyper::Error> {
    Full::new(chunk.into())
        .map_err(|never| match never {})
        .boxed()
}

/// Compresses a response with the desired compression algorithm
/// Currently, only gzip and deflate are supported
/// This method will modify the Content-Encoding and Content-Length headers
pub async fn encode_response(res: Res, content_encoding: Encoding) -> Result<Res> {
    let headers = res.headers().clone();
    let status = res.status();

    let res = res.map(|b| b.boxed());

    let body: Bytes = res
        .into_body()
        .collect()
        .await
        .map_err(convert_err)?
        .to_bytes();
    let body: &[u8] = &body;

    let mut ret_vec: Vec<u8> = Vec::new();
    match content_encoding {
        Encoding::Gzip => GzEncoder::new(body, Compression::fast())
            .read_to_end(&mut ret_vec)
            .map_err(convert_err),

        Encoding::Deflate => DeflateEncoder::new(body, Compression::fast())
            .read_to_end(&mut ret_vec)
            .map_err(convert_err),
    }?;

    let body: Bytes = ret_vec.into();
    let body_len = body.len();

    let mut res = Response::new(full(body));
    *res.headers_mut() = headers;
    *res.status_mut() = status;

    res.headers_mut()
        .insert(CONTENT_ENCODING, content_encoding.as_header_value());

    res.headers_mut().insert(
        CONTENT_LENGTH,
        body_len
            .to_string()
            .parse()
            .expect("Unexpected Content-Length"),
    );

    Ok(res)
}

/// A hyper service that compresses the responses
#[derive(Debug, Clone)]
pub struct Compressor<S> {
    inner: S,
    // encoding: Encoding,
}

impl<S> Compressor<S> {
    /// Creates a new Compression middleware that uses Gzip
    pub fn new(inner: S) -> Self {
        Compressor {
            inner,
            // encoding: Encoding::Gzip,
        }
    }

    // pub fn with_encoding(&mut self, encoding: Encoding) -> &mut Self {
    //     self.encoding = encoding;
    //     self
    // }
}

type Req = Request<Incoming>;
type Res = Response<BoxBody<Bytes, hyper::Error>>;

impl<S> Service<Req> for Compressor<S>
where
    S: Service<Req, Response = Res>,
    S::Future: 'static + Send,
    S::Error: 'static + Send,
    S::Error: Debug,
    S::Response: 'static,
{
    // type Response = S::Response;
    // type Error = S::Error;
    // type Future = S::Future;
    type Response = Res;
    type Error = Box<ResponseToStringError>;
    type Future =
        Pin<Box<dyn Future<Output = std::result::Result<Self::Response, Self::Error>> + Send>>;
    //type Future = Pin<Box<dyn Future<Output = std::result::Result<Self::Response, Self::Error>>>>;

    fn call(&self, req: Req) -> Self::Future {
        let fut = self.inner.call(req);
        let encoding = Encoding::Gzip; // self.encoding.clone();

        let f = async move {
            match fut.await {
                Ok(response) => encode_response(response, encoding).await.map_err(Box::new),
                Err(e) => Err(Box::new(convert_err(e))),
            }
        };

        Box::pin(f)
    }
}
