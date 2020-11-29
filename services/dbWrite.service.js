const { findAuthorByName, createNewAuthor } = require('../repositories/authorsAndMaintainers.repository')
const { findPackageByName, createNewPackage } = require('../repositories/package.repository')
const writeToPackageDb = async (eventData) => {
    let { authors, maintainers} = eventData
    trimmedAuthor = authors.toLowerCase().replace(/\s*\[.*?\]|\<.*?\>\s*/g, '').trim()
    trimmedMaintainer = maintainers.toLowerCase().replace(/\s*\[.*?\]|\<.*?\>\s*/g, '').trim()
    let authorDetails
    let maintainerDetails
    if(trimmedAuthor === trimmedMaintainer) {
        authorDetails = await findAuthorByName(trimmedAuthor)
        if(!authorDetails) {
            const newAuthorDetails = {
                name: trimmedAuthor,
                email: getEmailFromString(maintainers)
            }
        authorDetails = await createNewAuthor(newAuthorDetails)
        }
        eventData.authors = authorDetails._id
        eventData.maintainers = authorDetails._id
    } else {
        authorDetails = await findAuthorByName(trimmedAuthor)
        maintainerDetails = await findAuthorByName(trimmedAuthor)
        if(!authorDetails) {
            const newAuthorDetails = {
                name: trimmedAuthor,
                email: ''
            }
        authorDetails = await createNewAuthor(newAuthorDetails)
        }
        if(!maintainerDetails) {
            const newMaintainerDetails = {
                name: trimmedMaintainer,
                email: getEmailFromString(maintainers)
            }
        maintainerDetails = await createNewAuthor(newMaintainerDetails)
        }
        eventData.authors = authorDetails._id
        eventData.maintainers = maintainerDetails._id
    }

    return createNewPackage(eventData)

}

const getEmailFromString = (str) => {
    const regExp = /\<([^)]+)\>/
    const matches = regExp.exec(str)
    return matches[1] || ''
}

module.exports = {
    writeToPackageDb
}
