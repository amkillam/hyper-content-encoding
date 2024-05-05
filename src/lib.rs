//! Utility for handling Content-Encoding with [`hyper`](https://docs.rs/hyper)
//!
//! This crate currently only supports `gzip`, `deflate` and `identity`

// TODO:
// List of encodings:
// https://www.iana.org/assignments/http-parameters/http-parameters.xml#http-parameters-1

use bytes::Bytes;
use core::fmt;
use flate2::{
    read::{DeflateDecoder, DeflateEncoder, GzDecoder, GzEncoder, ZlibDecoder},
    Compression,
};
use http::{
    header::{ACCEPT_ENCODING, CONTENT_ENCODING, CONTENT_LENGTH, CONTENT_TYPE},
    HeaderValue, Request, Response, StatusCode,
};
use http_body_util::{combinators::BoxBody, BodyExt, Full};
use hyper::{body::Incoming, service::Service};
use std::{fmt::Debug, future::Future, pin::Pin};
use std::{io::prelude::*, str::FromStr};

type Result<T> = std::result::Result<T, HyperContentEncodingError>;

/// The error used in hyper-content-encoding
#[derive(Debug, Clone)]
pub struct HyperContentEncodingError {
    inner: String,
}

impl std::error::Error for HyperContentEncodingError {}

impl HyperContentEncodingError {
    pub fn new(inner: String) -> Self {
        HyperContentEncodingError { inner }
    }
}

impl From<String> for HyperContentEncodingError {
    fn from(value: String) -> Self {
        HyperContentEncodingError::new(value)
    }
}

impl fmt::Display for HyperContentEncodingError {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}", self.inner)
    }
}

fn convert_err<E>(e: E) -> HyperContentEncodingError
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
/// - `identity`
/// - `x-gzip`
/// - `gzip`
/// - `deflate`
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

/// The different supported encoding types
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Encoding {
    /// gzip
    Gzip,

    /// deflate
    Deflate,

    /// Identity
    Identity,
}

impl Encoding {
    fn as_header_value(&self) -> HeaderValue {
        match &self {
            Encoding::Gzip => HeaderValue::from_static("gzip"),

            Encoding::Deflate => HeaderValue::from_static("deflate"),

            Encoding::Identity => HeaderValue::from_static("identity"),
        }
    }
}
/// Creates a boxed body from a Byte like type
pub fn full<T: Into<Bytes>>(chunk: T) -> BoxBody<Bytes, hyper::Error> {
    Full::new(chunk.into())
        .map_err(|never| match never {})
        .boxed()
}

impl FromStr for Encoding {
    type Err = HyperContentEncodingError;
    fn from_str(value: &str) -> std::result::Result<Self, Self::Err> {
        match value {
            "x-gzip" | "gzip" => Ok(Encoding::Gzip),
            "deflate" => Ok(Encoding::Deflate),
            "identity" => Ok(Encoding::Identity),
            _ => Err(format!("Unrecognized encoding {}", value).into()),
        }
    }
}

/// Compresses a response with the desired compression algorithm.
///
/// Currently, only `gzip` and `deflate` are supported
///
/// This method will modify the `Content-Encoding` and `Content-Length` headers
///
/// TODO: encode the stream
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

    let mut ret_vec: Vec<u8> = Vec::new();
    match content_encoding {
        Encoding::Gzip => {
            let body: &[u8] = &body;
            GzEncoder::new(body, Compression::fast())
                .read_to_end(&mut ret_vec)
                .map_err(convert_err)
        }

        Encoding::Deflate => {
            let body: &[u8] = &body;
            DeflateEncoder::new(body, Compression::fast())
                .read_to_end(&mut ret_vec)
                .map_err(convert_err)
        }

        Encoding::Identity => {
            ret_vec = body.into();
            Ok(ret_vec.len())
        }
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

/// A hyper service that compresses responses
#[derive(Debug, Clone)]
pub struct Compressor<S> {
    inner: S,
}

impl<S> Compressor<S> {
    /// Creates a new Compression middleware that uses `gzip`
    pub fn new(inner: S) -> Self {
        Compressor { inner }
    }
}

/// Pareses a Quality Values
/// https://datatracker.ietf.org/doc/html/rfc9110#quality.values
///
///  weight = OWS ";" OWS "q=" qvalue
///  qvalue = ( "0" [ "." 0*3DIGIT ] )
///         / ( "1" [ "." 0*3("0") ] )
fn parse_weight(input: &str) -> Option<f32> {
    let mut chars = input.chars().peekable();

    // Parse leading optional white space
    while let Some(ch) = chars.peek() {
        if !ch.is_whitespace() {
            break;
        }

        chars.next();
    }

    // Parse ";"
    if chars.next() != Some(';') {
        eprintln!("psyche");
        return None;
    }

    // Parse optional white space after ";"
    while let Some(ch) = chars.peek() {
        if !ch.is_whitespace() {
            break;
        }

        chars.next();
    }

    // Parse "q="
    if chars.next() != Some('q') || chars.next() != Some('=') {
        eprintln!("Butt");
        return None;
    }

    // Parse qvalue
    let mut qvalue_str = String::new();
    for ch in &mut chars {
        if !ch.is_ascii_digit() && ch != '.' {
            break;
        }
        qvalue_str.push(ch);
    }

    // Parse qvalue into a float
    let qvalue: f32 = qvalue_str.parse().ok()?;

    if (0.0..1.0).contains(&qvalue) {
        Some(qvalue)
    } else {
        eprintln!("Q={}", qvalue);
        None
    }
}

// Parses the prefered_encoding (only keeping the currently supported encodings)
fn parse_encoding(accepted_encodings: &str) -> Vec<(Encoding, f32)> {
    let mut accepted_encodings = accepted_encodings.trim();

    let mut res = Vec::new();

    let mut default_weight: Option<f32> = None;

    loop {
        for token in ["gzip", "deflate", "identity", "*"] {
            if accepted_encodings.starts_with(token) {
                (_, accepted_encodings) = accepted_encodings.split_at(token.len());

                let mut weight: f32 = 1.0;
                if let Some(res) = parse_weight(accepted_encodings) {
                    weight = res;
                }

                if token == "*" {
                    default_weight = Some(weight);
                } else {
                    res.push((Encoding::from_str(token).unwrap(), weight));
                }

                break;
            }
        }

        if let Some(index) = accepted_encodings.find(',') {
            (_, accepted_encodings) = accepted_encodings.split_at(index);
            let mut chars = accepted_encodings.chars();
            chars.next();
            accepted_encodings = chars.as_str().trim();
        } else {
            break;
        }
    }

    if let Some(weigth) = default_weight {
        for encoding in [Encoding::Gzip, Encoding::Deflate, Encoding::Identity] {
            if !res.iter().any(|(x, _)| *x == encoding) {
                res.push((encoding, weigth));
            }
        }
    } else if !res.iter().any(|(x, _)| *x == Encoding::Identity) {
        res.push((Encoding::Identity, 1.0));
    }

    res

    // Else fail with 415
}

fn prefered_encoding(accepted_encodings: &str) -> Option<Encoding> {
    let mut encodings = parse_encoding(accepted_encodings);
    encodings.sort_by_key(|&(_, w)| -(w * 1000.0) as i32);

    encodings
        .iter()
        .find(|(_, w)| *w > 0.0)
        .map(|(e, _)| e.to_owned())
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
    type Response = Res;
    type Error = Box<HyperContentEncodingError>;
    type Future =
        Pin<Box<dyn Future<Output = std::result::Result<Self::Response, Self::Error>> + Send>>;

    fn call(&self, req: Req) -> Self::Future {
        let headers = req.headers().clone();

        // Gets the desired encoding
        let encoding = if let Some(accepted_encodings) = headers.get(ACCEPT_ENCODING) {
            if let Some(desired_encoding) =
                accepted_encodings.to_str().ok().and_then(prefered_encoding)
            {
                desired_encoding
            } else {
                return Box::pin(async move {
                    let mut res = Response::new(full("Unsuported requestedd encoding"));
                    *res.status_mut() = StatusCode::NOT_ACCEPTABLE;
                    Ok(res)
                });
            }
        } else {
            Encoding::Gzip
        };

        let fut = self.inner.call(req);

        let f = async move {
            match fut.await {
                Ok(response) => encode_response(response, encoding).await.map_err(Box::new),
                Err(e) => Err(Box::new(convert_err(e))),
            }
        };

        Box::pin(f)
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn no_weight() {
        let result = parse_encoding("compress, gzip");
        assert_eq!(
            vec![(Encoding::Gzip, 1.0), (Encoding::Identity, 1.0)],
            result
        );
    }

    #[test]
    fn empty() {
        let result = parse_encoding("");
        assert_eq!(vec![(Encoding::Identity, 1.0)], result);
    }

    #[test]
    fn star() {
        let result = parse_encoding("*");
        assert_eq!(
            vec![
                (Encoding::Gzip, 1.0),
                (Encoding::Deflate, 1.0),
                (Encoding::Identity, 1.0)
            ],
            result
        );
    }

    #[test]
    fn weigth() {
        let result = parse_encoding("deflate;q=0.5, gzip;q=1.0");
        eprintln!("{:?}", result);
        assert_eq!(
            vec![
                (Encoding::Deflate, 0.5),
                (Encoding::Gzip, 1.0),
                (Encoding::Identity, 1.0)
            ],
            result
        );
    }

    #[test]
    fn no_identity() {
        let result = parse_encoding("gzip;q=1.0, deflate; q=0.5, *;q=0");
        eprintln!("{:?}", result);
        assert_eq!(
            vec![
                (Encoding::Gzip, 1.0),
                (Encoding::Deflate, 0.5),
                (Encoding::Identity, 0.0)
            ],
            result
        );
    }
}
