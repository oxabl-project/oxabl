//! `weak-crypto` (ABLSEC009).

use oxabl_lint::security::weak_crypto::{HELP, run};

use super::{check, check_expanded, flagged};

#[test]
fn reports_weak_hashes_of_password_and_token_like_names() {
    let src = "\
cHash = ENCODE(cPassword).
cHash = MD5-DIGEST(cPassword + cSalt).
cHash = SHA1-DIGEST(pcToken).
";
    let diags = check(run, src);
    assert_eq!(diags.len(), 3, "{diags:?}");
    assert_eq!(
        flagged(src, &diags),
        [
            "ENCODE(cPassword)",
            "MD5-DIGEST(cPassword + cSalt)",
            "SHA1-DIGEST(pcToken)"
        ]
    );
}

#[test]
fn hashes_of_other_values_are_not_reported() {
    let src = "\
cHash = ENCODE(cName).
cHash = MD5-DIGEST(cFileData).
cHash = SHA1-DIGEST(mBlob).
";
    assert!(check(run, src).is_empty());
}

#[test]
fn reports_message_digest_with_a_weak_algorithm() {
    let src = "\
cHash = MESSAGE-DIGEST(\"MD5\", cPassword).
cHash = MESSAGE-DIGEST(\"SHA-1\", cPassword).
";
    assert_eq!(check(run, src).len(), 2);
}

#[test]
fn a_strong_algorithm_or_other_data_is_not_reported() {
    let src = "\
cHash = MESSAGE-DIGEST(\"SHA-256\", cPassword).
cHash = MESSAGE-DIGEST(\"MD5\", cFileData).
";
    assert!(check(run, src).is_empty());
}

#[test]
fn reports_random_that_feeds_a_token_nonce_or_id() {
    let src = "\
cToken = STRING(RANDOM(1, 999999)).
iNonce = RANDOM(1, 100).
cSessionId = SUBSTITUTE(\"&1-&2\", RANDOM(1, 9), ETIME).
ASSIGN iUserId = RANDOM(1, 100000).
";
    assert_eq!(check(run, src).len(), 4);
}

#[test]
fn random_for_other_purposes_is_not_reported() {
    let src = "\
iDice = RANDOM(1, 6).
iPick = RANDOM(1, iCount).
cToken = BASE64-ENCODE(GENERATE-RANDOM-KEY).
cToken = \"fixed\".
";
    assert!(check(run, src).is_empty());
}

#[test]
fn reports_in_blocks_and_class_members() {
    let src = "\
CLASS Acme.Auth:
    METHOD PUBLIC CHARACTER Hash(INPUT pcPassword AS CHARACTER):
        RETURN ENCODE(pcPassword).
    END METHOD.
END CLASS.
";
    assert_eq!(check(run, src).len(), 1);
}

#[test]
fn a_define_that_expands_to_the_name_is_reported() {
    let main = "\
&GLOBAL-DEFINE SECRET cPassword
cHash = ENCODE({&SECRET}).
";
    assert_eq!(check_expanded(run, &[], main).len(), 1);
}

#[test]
fn a_sink_in_an_include_is_reported() {
    let files = [("hash.i", "cHash = MD5-DIGEST(cPassword).\n")];
    assert_eq!(check_expanded(run, &files, "{hash.i}\n").len(), 1);
}

#[test]
fn the_help_names_the_safe_patterns() {
    assert!(HELP.contains("GENERATE-RANDOM-KEY"), "{HELP}");
    assert!(HELP.contains("salted"), "{HELP}");
}
