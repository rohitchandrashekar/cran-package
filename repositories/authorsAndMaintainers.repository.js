const { AuthorsAndMaintainersModel } = require('../model/authorsAndMaintaners.model')

const findAuthorByName = async (authorName) => {
    return AuthorsAndMaintainersModel.findOne({ name: authorName })
}
const createNewAuthor = async (authorData) => {
    return AuthorsAndMaintainersModel.create(authorData)
}
const updateAuthor = async (id, authorValues) => {
    return AuthorsAndMaintainersModel.findByIdAndUpdate(
        { _id: Types.ObjectId(id) }, authorValues, { new: true },
      )
}

module.exports = {
    findAuthorByName,
    createNewAuthor,
    updateAuthor
}
