//! The newest release of oxabl, read from the GitHub API over HTTPS. Only a
//! loopback host may replace the API (`OXABL_UPDATE_API_URL`, for tests), and
//! only a loopback host may use `http`.

use std::fmt;
use std::time::Duration;

use reqwest::{Client, Url, redirect};
use serde::Deserialize;

pub const ASSET: &str = "oxabl-x86_64-unknown-linux-musl";
const API: &str = "https://api.github.com";
const TAG_PREFIX: &str = "oxabl-v";
const MAX_PAGES: usize = 50;
const MAX_JSON: usize = 8 * 1024 * 1024;

/// A plain `X.Y.Z` version.
#[derive(Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Debug)]
pub struct Version(pub u64, pub u64, pub u64);

impl Version {
    pub fn parse(text: &str) -> Option<Version> {
        let mut parts = text.split('.');
        let mut next = || {
            let part = parts.next()?;
            (!part.is_empty() && part.bytes().all(|b| b.is_ascii_digit()))
                .then(|| part.parse().ok())
                .flatten()
        };
        let version = Version(next()?, next()?, next()?);
        parts.next().is_none().then_some(version)
    }
}

impl fmt::Display for Version {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}.{}.{}", self.0, self.1, self.2)
    }
}

/// A release that is complete: published, final, with both assets.
pub struct Release {
    pub version: Version,
    pub binary: Url,
    pub checksum: Url,
}

fn loopback(url: &Url) -> bool {
    matches!(url.host_str(), Some("127.0.0.1" | "localhost" | "[::1]"))
}

/// `https`, or `http` to a loopback host.
pub fn allowed(url: &Url) -> bool {
    url.scheme() == "https" || (url.scheme() == "http" && loopback(url))
}

pub struct Api {
    base: Url,
    client: Client,
}

impl Api {
    pub fn new() -> Result<Api, String> {
        let base = match std::env::var("OXABL_UPDATE_API_URL") {
            Ok(text) if !text.is_empty() => match Url::parse(&text) {
                Ok(url) if allowed(&url) && loopback(&url) => url,
                _ => {
                    eprintln!("warning: OXABL_UPDATE_API_URL is not a loopback URL; ignored");
                    Url::parse(API).expect("a constant URL")
                }
            },
            _ => Url::parse(API).expect("a constant URL"),
        };
        let client = Client::builder()
            .user_agent(concat!("oxabl-self-update/", env!("CARGO_PKG_VERSION")))
            .connect_timeout(Duration::from_secs(15))
            .timeout(Duration::from_secs(600))
            .redirect(redirect::Policy::custom(|attempt| {
                if attempt.previous().len() >= 5 {
                    attempt.error("too many redirects")
                } else if allowed(attempt.url()) {
                    attempt.follow()
                } else {
                    attempt.error("a redirect to a URL that is not https")
                }
            }))
            .build()
            .map_err(|e| format!("cannot start the HTTP client: {e}"))?;
        Ok(Api { base, client })
    }

    /// A GET whose answer is 200; any other answer is an error.
    pub async fn get(&self, url: &Url) -> Result<reqwest::Response, String> {
        if !allowed(url) {
            return Err(format!("refusing {url}: not https"));
        }
        let response = self
            .client
            .get(url.clone())
            .header("Accept", "application/vnd.github+json")
            .send()
            .await
            .map_err(|e| format!("cannot fetch {url}: {e}"))?;
        match response.status().as_u16() {
            200 => Ok(response),
            status => Err(format!("{url} answered {status}")),
        }
    }

    /// The whole body, up to `limit` bytes.
    pub async fn bytes(&self, url: &Url, limit: usize) -> Result<Vec<u8>, String> {
        let mut response = self.get(url).await?;
        let mut body = Vec::new();
        while let Some(chunk) = response
            .chunk()
            .await
            .map_err(|e| format!("cannot read {url}: {e}"))?
        {
            if body.len() + chunk.len() > limit {
                return Err(format!("{url} is larger than {limit} bytes"));
            }
            body.extend_from_slice(&chunk);
        }
        Ok(body)
    }

    fn endpoint(&self, path: &str) -> Result<Url, String> {
        self.base
            .join(path)
            .map_err(|e| format!("bad API path {path}: {e}"))
    }

    /// The highest plain version among the tags `oxabl-vX.Y.Z`.
    async fn newest(&self) -> Result<Version, String> {
        #[derive(Deserialize)]
        struct Reference {
            r#ref: String,
        }
        let mut url = Some(self.endpoint(&format!(
            "repos/oxabl-project/oxabl/git/matching-refs/tags/{TAG_PREFIX}?per_page=100"
        ))?);
        let mut newest = None;
        for _ in 0..MAX_PAGES {
            let Some(page) = url.take() else { break };
            let response = self.get(&page).await?;
            let next = next_link(response.headers());
            let body = response
                .bytes()
                .await
                .map_err(|e| format!("cannot read {page}: {e}"))?;
            if body.len() > MAX_JSON {
                return Err(format!("{page} is too large"));
            }
            let references: Vec<Reference> = serde_json::from_slice(&body)
                .map_err(|e| format!("{page} is not a list of references: {e}"))?;
            for reference in references {
                let version = reference
                    .r#ref
                    .strip_prefix("refs/tags/")
                    .and_then(|tag| tag.strip_prefix(TAG_PREFIX))
                    .and_then(Version::parse);
                newest = newest.max(version);
            }
            url = next;
        }
        if url.is_some() {
            return Err("too many pages of tags".into());
        }
        newest.ok_or_else(|| "no release of oxabl was found".into())
    }

    /// The newest complete release, or why none was established.
    pub async fn latest(&self) -> Result<Release, String> {
        #[derive(Deserialize)]
        struct Asset {
            name: String,
            browser_download_url: String,
        }
        #[derive(Deserialize)]
        struct Document {
            draft: bool,
            prerelease: bool,
            assets: Vec<Asset>,
        }
        let version = self.newest().await?;
        let url = self.endpoint(&format!(
            "repos/oxabl-project/oxabl/releases/tags/{TAG_PREFIX}{version}"
        ))?;
        let body = self.bytes(&url, MAX_JSON).await?;
        let document: Document =
            serde_json::from_slice(&body).map_err(|e| format!("{url} is not a release: {e}"))?;
        if document.draft || document.prerelease {
            return Err(format!("release {version} is a draft or a pre-release"));
        }
        let asset = |name: &str| {
            let found = document.assets.iter().find(|asset| asset.name == name);
            let found = found.ok_or_else(|| format!("release {version} has no {name}"))?;
            Url::parse(&found.browser_download_url)
                .map_err(|e| format!("release {version}: bad URL for {name}: {e}"))
        };
        Ok(Release {
            version,
            binary: asset(ASSET)?,
            checksum: asset(&format!("{ASSET}.sha256"))?,
        })
    }
}

/// The `rel="next"` target of a `Link` header.
fn next_link(headers: &reqwest::header::HeaderMap) -> Option<Url> {
    let value = headers.get(reqwest::header::LINK)?.to_str().ok()?;
    value.split(',').find_map(|part| {
        let (target, rest) = part.split_once('>')?;
        rest.contains("rel=\"next\"")
            .then(|| Url::parse(target.trim().strip_prefix('<')?).ok())
            .flatten()
    })
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn only_a_plain_three_part_version_parses() {
        assert_eq!(Version::parse("5.2.0"), Some(Version(5, 2, 0)));
        for bad in [
            "5.2",
            "5.2.0.1",
            "5.2.0-rc1",
            "v5.2.0",
            "5..0",
            "5.2.+1",
            "",
        ] {
            assert_eq!(Version::parse(bad), None, "{bad}");
        }
        assert!(Version(5, 10, 0) > Version(5, 9, 9));
    }

    #[test]
    fn https_and_loopback_http_are_allowed_and_nothing_else() {
        let url = |text: &str| Url::parse(text).unwrap();
        assert!(allowed(&url("https://example.com/a")));
        assert!(allowed(&url("http://127.0.0.1:80/a")));
        assert!(allowed(&url("http://localhost/a")));
        assert!(allowed(&url("http://[::1]:9/a")));
        assert!(!allowed(&url("http://example.com/a")));
        assert!(!allowed(&url("ftp://127.0.0.1/a")));
        assert!(!loopback(&url("https://example.com/")));
    }
}
