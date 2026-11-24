// SYNTAX TEST "source.abl" "records: a table named like an unreserved keyword"

// an unreserved keyword names a table after FIND, FOR EACH, AVAILABLE and the other record positions
    FIND Item WHERE Item.ItemNum = 1 NO-LOCK NO-ERROR.
//  ^^^^ keyword.other.abl
//       ^^^^ variable.other.abl
    FIND FIRST Item NO-LOCK.
//       ^^^^^ keyword.other.abl
//             ^^^^ variable.other.abl
    FIND NEXT Item. FIND PREV Item. FIND CURRENT Item EXCLUSIVE-LOCK.
//       ^^^^ keyword.control.abl
//            ^^^^ variable.other.abl
//                       ^^^^ keyword.other.abl
//                            ^^^^ variable.other.abl
//                                       ^^^^^^^ keyword.other.abl
//                                               ^^^^ variable.other.abl
    FOR EACH Item NO-LOCK,
//  ^^^ keyword.control.abl
//      ^^^^ keyword.control.abl
//           ^^^^ variable.other.abl
        EACH Menu OF Item NO-LOCK,
//           ^^^^ variable.other.abl
//                ^^ keyword.other.abl
//                   ^^^^ variable.other.abl
        FIRST Item NO-LOCK, LAST Menu NO-LOCK:
//            ^^^^ variable.other.abl
//                          ^^^^ keyword.other.abl
//                               ^^^^ variable.other.abl
    IF AVAILABLE Item OR NOT AVAIL Menu THEN.
//     ^^^^^^^^^ keyword.other.abl
//               ^^^^ variable.other.abl
//                           ^^^^^ keyword.other.abl
//                                 ^^^^ variable.other.abl
    IF AVAILABLE(Item) OR AVAILABLE (Menu) THEN.
//     ^^^^^^^^^ support.function.abl
//               ^^^^ variable.other.abl
//                        ^^^^^^^^^ support.function.abl
//                                   ^^^^ variable.other.abl
    IF LOCKED Item OR AMBIGUOUS Menu THEN.
//            ^^^^ variable.other.abl
//                              ^^^^ variable.other.abl
    IF CAN-FIND(Item WHERE Item.ItemNum = 1) OR CAN-FIND(FIRST Menu) THEN.
//     ^^^^^^^^ support.function.abl
//              ^^^^ variable.other.abl
//                                              ^^^^^^^^ support.function.abl
//                                                       ^^^^^ keyword.other.abl
//                                                             ^^^^ variable.other.abl
    r = ROWID(Item). i = RECID(Menu).
//      ^^^^^ support.function.abl
//            ^^^^ variable.other.abl
//                       ^^^^^ support.function.abl
//                             ^^^^ variable.other.abl
    DO FOR Item, Menu:
//     ^^^ keyword.control.abl
//         ^^^^ variable.other.abl
//               ^^^^ variable.other.abl
    REPEAT FOR Item TRANSACTION:
//             ^^^^ variable.other.abl
    DEFINE QUERY q FOR Item, Menu SCROLLING.
//                     ^^^^ variable.other.abl
//                           ^^^^ variable.other.abl
//                                ^^^^^^^^^ keyword.other.abl
    DEFINE BUFFER bItem FOR Item.
//                          ^^^^ variable.other.abl
    DEFINE TEMP-TABLE ttItem NO-UNDO LIKE Item.
//                                   ^^^^ keyword.other.abl
//                                        ^^^^ variable.other.abl
    RUN p (BUFFER Item). h = BUFFER Menu:HANDLE.
//         ^^^^^^ keyword.other.abl
//                ^^^^ variable.other.abl
//                                  ^^^^ variable.other.abl
    BUFFER-COPY Item TO ttItem. BUFFER-COMPARE Menu TO ttMenu.
//              ^^^^ variable.other.abl
//                                             ^^^^ variable.other.abl
    FOR EACH Menu OF Item EXCLUSIVE-LOCK:
//                   ^^^^ variable.other.abl
    FOR EACH Menu OF Item /* join */ NO-LOCK:
//                   ^^^^ variable.other.abl
    ON WRITE OF Item OLD BUFFER oldItem DO:
//     ^^^^^ keyword.other.abl
//              ^^^^ variable.other.abl
    ON FIND OF Menu DO:
//     ^^^^ keyword.other.abl
//          ^^ keyword.other.abl
//             ^^^^ variable.other.abl
    CREATE Item. DELETE Menu NO-ERROR. RELEASE Item. VALIDATE Menu.
//  ^^^^^^ keyword.other.abl
//         ^^^^ variable.other.abl
//               ^^^^^^ keyword.other.abl
//                      ^^^^ variable.other.abl
//                                             ^^^^ variable.other.abl
//                                                            ^^^^ variable.other.abl
    DISABLE TRIGGERS FOR LOAD OF Item.
//                       ^^^^ keyword.other.abl
//                            ^^ keyword.other.abl
//                               ^^^^ variable.other.abl

// the same positions keep the keywords that are not tables
    CREATE WIDGET-POOL. DELETE WIDGET-POOL.
//         ^^^^^^^^^^^ keyword.other.abl
//                             ^^^^^^^^^^^ keyword.other.abl
    DELETE PROCEDURE THIS-PROCEDURE. DELETE OBJECT hObject NO-ERROR.
//         ^^^^^^^^^ keyword.other.abl
//                   ^^^^^^^^^^^^^^ variable.language.abl
//                                          ^^^^^^ keyword.other.abl
    CREATE MENU-ITEM hMenuItem.
//         ^^^^^^^^^ keyword.other.abl
    ON CHOOSE OF MENU-ITEM m-exit IN MENU m-bar DO:
//               ^^^^^^^^^ keyword.other.abl
//                                   ^^^^ keyword.other.abl
    ON MENU-DROP OF MENU m-bar DO:
//                  ^^^^ keyword.other.abl
    ON CHOOSE OF MENU-ITEM m-exit /* File */
//               ^^^^^^^^^ keyword.other.abl
    ON WINDOW-CLOSE OF CURRENT-WINDOW DO:
//                     ^^^^^^^^^^^^^^ variable.language.abl
    ON CLOSE OF THIS-PROCEDURE
//              ^^^^^^^^^^^^^^ variable.language.abl
    DEFINE BUFFER bTt FOR TEMP-TABLE tt.
//                        ^^^^^^^^^^ keyword.other.abl
    FIND PREV
//       ^^^^ keyword.other.abl
        Item NO-LOCK.
    x = my-find Item. x = a.find Item.
//              ^^^^ keyword.other.abl
//                               ^^^^ keyword.other.abl
