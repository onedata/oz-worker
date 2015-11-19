// View that allows selecting auth records by a user_id
function (doc) {
    if (doc.record__ == "auth")
        emit(doc.user_id, null)
}
