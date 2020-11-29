const { Schema, model } = require('mongoose')

const AuthorsAndMaintainersSchema = new Schema({
  name: {
    type: String, trim: true, required: true, unique: true,
  },
  email: { type: String, default: ''},

})

AuthorsAndMaintainersSchema.index({ name: 'text' })

const AuthorsAndMaintainersModel = model('AuthorsAndMaintainers', AuthorsAndMaintainersSchema)

module.exports = { AuthorsAndMaintainersModel, AuthorsAndMaintainersSchema }
