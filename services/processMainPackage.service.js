const fs = require('fs')
const eventStream = require('event-stream')
const debianControl = require("debian-control")
const logger = require('../utils/logger.util')
const { publishSubPackageEvent } = require('../events/subpackage.event')
const { DOWNLOAD_MAIN_PACKAGE, MAIN_PACKAGE_URL, MAIN_PACKAGE_DOWNLOAD_DEST } = require('../constants/constant')
const { downloadFile, unZipGzFile } = require('../utils/file.util')
const parsePackages = async () => {
    let dataCount = 0
    let mainPackagePath = './stubs/PACKAGES'
    if(DOWNLOAD_MAIN_PACKAGE) {
        const url = MAIN_PACKAGE_URL
        const downloadInfo = await downloadFile(url, MAIN_PACKAGE_DOWNLOAD_DEST)
        await unZipGzFile(MAIN_PACKAGE_DOWNLOAD_DEST, './downloads/mainPackages/PACKAGES')
        mainPackagePath = './downloads/mainPackages/PACKAGES'
    }
    const packageFile = fs.createReadStream(mainPackagePath).pipe(eventStream.split('\n\n')).pipe(eventStream.mapSync((element)=> {
        console.log(element)
        const packageObj = debianControl.parse(element)
        dataCount++
        const eventData = {
            packageName: packageObj.Package,
            version: packageObj.Version,
            r_version_needed: packageObj.Depends,
            dependencies: packageObj.Imports && typeof packageObj.Imports !==  'string' ? packageObj.Imports.toString() : packageObj.Imports,
            license: packageObj.License,
            suggestions: packageObj.Suggests
        }
        publishSubPackageEvent(eventData)
    })).on('error',(error) => {
        logger.error('error in parsing main file', error)
    }).on('end',()=> {
        logger.info('completed reading main file')
        logger.info(`total packages read ${dataCount}`)
    })
}
   
   module.exports = {
       parsePackages
   }
   