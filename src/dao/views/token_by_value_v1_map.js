// View that allows to select a token document by its value property.
// The record defining token document's structure can be found in dao_token.hrl
function (doc) {
    if (doc.record__ == "token") {
        emit(doc.value, null)
    }
}