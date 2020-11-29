const events = require('events')
const dbWriteEvent = new events.EventEmitter()
const { writeToPackageDb } = require('../services/dbWrite.service')

const publishWriteDbEvent = (eventData) => {
    dbWriteEvent.emit('db-write', eventData)
}

dbWriteEvent.on('db-write', writeToPackageDb)

module.exports = {
    publishWriteDbEvent,
}
