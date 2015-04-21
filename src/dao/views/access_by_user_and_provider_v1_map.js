// View that allows to select an access document by a combination of its
// user and provider_id properties.
// The record defining access document's structure can be found in dao_auth.hrl
function (doc) {
    if (doc.record__ == "access") {
        emit([doc.user_id, doc.provider_id], null)
    }
}
