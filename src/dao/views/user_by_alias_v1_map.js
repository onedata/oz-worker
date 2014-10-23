// View that allows selecting user by its alias
function (doc) {
    if (doc.record__ == "user" && doc.alias != "") {  // 0 means empty alias (not set)
        emit(doc.alias, null)
    }
}