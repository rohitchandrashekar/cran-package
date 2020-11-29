const { PackageModel } = require('../model/packages.model')

const findPackageByName = async (packageName) => {
    return PackageModel.findOne({ packageName: packageName })
}
const createNewPackage = async (packageValues) => {
    return PackageModel.create(packageValues)
}
const updatePackage = async (id, packageValues) => {
    return packageModel.findByIdAndUpdate(
        { _id: Types.ObjectId(id) }, updateData, { new: true },
      )
}

module.exports = {
    findPackageByName,
    createNewPackage,
    updatePackage
}
