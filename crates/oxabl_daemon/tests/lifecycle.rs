//! One session per workspace root, and what a session shares between clients
//! (R6, KTD21).

use std::path::{Path, PathBuf};

use oxabl_daemon::Sessions;

#[test]
fn two_workspace_roots_get_two_sessions() {
    let mut sessions = Sessions::new();
    sessions.for_root("/proj/alpha");
    sessions.for_root("/proj/beta");

    assert_eq!(sessions.len(), 2);
    assert_eq!(
        sessions.roots(),
        vec![Path::new("/proj/alpha"), Path::new("/proj/beta")]
    );
}

/// The property the daemon exists for: two clients on one root hold one session
/// between them, not one each.
#[test]
fn two_clients_on_one_root_share_one_session() {
    let mut sessions = Sessions::new();
    sessions.for_root("/proj").attach(true);
    sessions.for_root("/proj").attach(false);

    assert_eq!(sessions.len(), 1, "one root is one session");
    let session = sessions.get("/proj").expect("the session exists");
    assert_eq!(session.clients(), 2);
    assert_eq!(
        session.editor_clients(),
        1,
        "only the editor contributes unsaved buffers"
    );
}

/// Two spellings of one root are one session. Without normalising, a client that
/// said `/proj/.` would index the workspace a second time and hold it a second time
/// — the exact cost the daemon exists to avoid.
#[test]
fn two_spellings_of_one_root_are_one_session() {
    let mut sessions = Sessions::new();
    sessions.for_root("/proj").attach(false);
    sessions.for_root("/proj/./sub/..").attach(false);

    assert_eq!(sessions.len(), 1, "got {:?}", sessions.roots());
    assert_eq!(sessions.get("/proj").expect("the session").clients(), 2);
}

#[test]
fn two_sessions_do_not_share_cached_facts() {
    let mut sessions = Sessions::new();
    sessions
        .for_root("/proj/alpha")
        .set_buffer("main.p", "MESSAGE \"alpha\".\n".to_string(), None);

    assert_eq!(
        sessions.for_root("/proj/beta").open_buffers(),
        0,
        "a buffer opened in one session must not appear in another"
    );
    assert_eq!(sessions.for_root("/proj/alpha").open_buffers(), 1);
}

#[test]
fn a_detaching_client_leaves_the_session_for_the_other_one() {
    let mut sessions = Sessions::new();
    let session = sessions.for_root("/proj");
    session.attach(true);
    session.attach(false);
    session.detach(true);

    assert_eq!(session.clients(), 1);
    assert_eq!(session.editor_clients(), 0);
    assert_eq!(
        sessions.len(),
        1,
        "the session survives one client leaving — that is the point of sharing it"
    );
}

/// A double detach cannot wrap the counts into a very large number of imaginary
/// clients, which would make an answer claim a working tree nobody is editing.
#[test]
fn detaching_more_clients_than_attached_saturates() {
    let mut sessions = Sessions::new();
    let session = sessions.for_root("/proj");
    session.attach(true);
    session.detach(true);
    session.detach(true);

    assert_eq!(session.clients(), 0);
    assert_eq!(session.editor_clients(), 0);
}

#[test]
fn a_buffer_can_be_replaced_and_closed() {
    let mut sessions = Sessions::new();
    let session = sessions.for_root("/proj");
    let path = Some(PathBuf::from("/proj/main.p"));

    let first = session.set_buffer("main.p", "MESSAGE \"one\".\n".to_string(), path.clone());
    let second = session.set_buffer("main.p", "MESSAGE \"two\".\n".to_string(), path);
    assert!(
        first == second,
        "an edit sets the existing input rather than minting a second one"
    );
    assert_eq!(session.buffer_keys(), vec!["main.p".to_string()]);

    session.close_buffer("main.p");
    assert!(session.buffer("main.p").is_none());
    assert_eq!(session.open_buffers(), 0);
}

/// Installing a configuration bumps the generation, which is what lets a completed
/// computation be recognised as computed under rules the user has replaced.
#[test]
fn installing_a_configuration_bumps_the_generation() {
    let mut sessions = Sessions::new();
    let session = sessions.for_root("/proj");
    assert_eq!(session.config_generation(), 0);

    session.install_config(oxabl_pipeline::PipelineConfig::default());
    assert_eq!(session.config_generation(), 1);

    session.install_config(oxabl_pipeline::PipelineConfig::default());
    assert_eq!(session.config_generation(), 2);
}
