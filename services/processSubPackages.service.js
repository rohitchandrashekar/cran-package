const fs = require('fs')
const eventStream = require('event-stream')
const debianControl = require("debian-control")
const { publishWriteDbEvent } = require('../events/dbWrite.event')

const { downloadFile, unZipFile } = require('../utils/file.util')
const logger = require('../utils/logger.util')
const parseSubPackages = async (eventData) => {
    const { packageName, version } = eventData
    const url = `http://cran.r-project.org/src/contrib/${packageName}_${version}.tar.gz`
    const downloadInfo = await downloadFile(url, `./downloads/subPackages/${packageName}.tar.gz`)
    logger.info(`downloaded package ${packageName}`)
    await unZipFile(`./downloads/subPackages/${packageName}.tar.gz`, './downloads/subPackages/')
    const subPackageFile = fs.createReadStream(`./downloads/subPackages/${packageName}/DESCRIPTION`).pipe(eventStream.split('\n\n')).pipe(eventStream.mapSync(async (element)=> {
        const subPackageObj = debianControl.parse(element)
        let completePackageData = Object.assign({},eventData)
        completePackageData.suggestions = subPackageObj.Suggests
        completePackageData.title = subPackageObj.Title && typeof subPackageObj.Title !== 'string' ? subPackageObj.Title.toString() : subPackageObj.Title
        completePackageData.description = subPackageObj.Description && typeof subPackageObj.Description !== 'string' ? subPackageObj.Description.toString() : subPackageObj.Description
        completePackageData.authors = subPackageObj.Author
        completePackageData.maintainers = subPackageObj.Maintainer
        completePackageData.publication = subPackageObj['Date/Publication']
        publishWriteDbEvent(completePackageData)
    })).on('error',(error) => {
        logger.error(`error in parsing sub file ${packageName}`, error)
    }).on('end',()=> {
        logger.info(`completed reading sub package file ${packageName}`)
    })


}

module.exports = {
    parseSubPackages
}
