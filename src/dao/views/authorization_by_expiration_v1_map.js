function(doc) {
    if (doc.record__ == "authorization") {
        emit(doc.expiration_time, null)
    }
}