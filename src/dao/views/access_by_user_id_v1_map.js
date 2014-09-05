function(doc) {
    if (doc.record__ == "access") {
        emit(doc.user_id, null)
    }
}