// View that allows to select an authorization document by its code property.
// The record defining authorization document's structure can be found in dao_auth.hrl
function(doc) {
    if (doc.record__ == "authorization") {
        emit(doc.code, null)
    }
}