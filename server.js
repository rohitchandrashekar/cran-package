const mongoose = require('mongoose')
const express = require('express')
const app = express()
const logger = require('./utils/logger.util')
const bodyParser = require('body-parser')
const { parsePackages } = require('./services/processMainPackage.service')
const {MONGO_URI, PORT} = require('./constants/constant.js')
app.use(bodyParser.urlencoded({ extended: false }))
app.use(bodyParser.json())

app.listen(PORT, () => {
    console.log(`Example app listening at http://localhost:${PORT}`)
    mongoose.connect(MONGO_URI, {
        useNewUrlParser: true,
        useFindAndModify: false,
        useCreateIndex: true,
    })
    mongoose.connection.on('open', () => {
        logger.info('Connected to Mongo.')
        parsePackages()
    })
    mongoose.connection.on('error', (err) => {
        logger.error(err)
    })
})
module.exports = app
