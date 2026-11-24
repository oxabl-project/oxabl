//! The warnings of one build, collected and handed out once.

use std::sync::Mutex;

#[derive(Default)]
pub struct Notes(Mutex<Vec<String>>);

impl Notes {
    pub fn push(&self, note: String) {
        if let Ok(mut notes) = self.0.lock() {
            notes.push(note);
        }
    }

    pub fn take(&self) -> Vec<String> {
        self.0
            .lock()
            .map(|mut notes| std::mem::take(&mut *notes))
            .unwrap_or_default()
    }
}
