const events = require('events')
const subPackageEvent = new events.EventEmitter()
const { parseSubPackages } = require('../services/processSubPackages.service')

const publishSubPackageEvent = (eventData) => {
    subPackageEvent.emit('sub-package', eventData)
}

subPackageEvent.on('sub-package', parseSubPackages)

module.exports = {
    publishSubPackageEvent,
}
