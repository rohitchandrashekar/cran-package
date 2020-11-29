const { Schema, model, Types } = require('mongoose')

const PackageSchema = new Schema({
  packageName: {
    type: String, trim: true, required: true, unique: true,
  },
  version: { type: String, required: true },
  r_version_needed: { type: String, required: true },
  dependencies: { type: String },
  suggestions: { type: String },
  title: { type: String, required: true },
  description: { type: String, required: true },
  authors: { type: Types.ObjectId, required: true },
  maintainers: { type: Types.ObjectId, required: true },
  license: { type: String, required: true },
  publication: Date,
  createdAt: Date,
  updatedAt: Date,
  archived: { type: Boolean, default: false },

})

PackageSchema.index({ packageName: 'text' })

const PackageModel = model('Package', PackageSchema)

module.exports = { PackageModel, PackageSchema }
