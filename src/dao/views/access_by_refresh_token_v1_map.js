function(doc) {
    if (doc.record__ == "access") {
        emit(doc.refresh_token, null)
    }
}