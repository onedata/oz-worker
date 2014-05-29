// View that allows selecting user by its email 
function (doc) {
    if (doc.record__ == "user") {
        for (key in doc.email_list)
            emit(doc.email_list[key], null);
        for (acc in doc.connected_accounts)
            for (key in doc.connected_accounts[acc])
                emit(doc.connected_accounts[acc].email_list[key], null);
    }
}