function(doc) {
    if (doc.record__ == "authorization") {
        emit(doc.code, null)
    }
}