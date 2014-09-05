function(doc) {
    if (doc.record__ == "access") {
        emit(doc.token, null)
    }
}