// View that allows selecting user by his ID that came from OAuth / OpenID provider
function(doc)
{
    if (doc.record__ == "user")
        for (acc in doc.connected_accounts)
            emit([doc.connected_accounts[acc].provider_id, doc.connected_accounts[acc].user_id], null);
}
