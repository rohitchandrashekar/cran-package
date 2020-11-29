const fs = require('fs')
const fetch = require('node-fetch')
const targz = require('targz')
const zlib = require('zlib')

const downloadFile = async (url, destination) => {
    const res = await fetch(url)
    const fileStream = fs.createWriteStream(destination)
    await new Promise((resolve, reject) => {
      res.body.pipe(fileStream)
      res.body.on("error", reject)
      fileStream.on("finish", resolve)
    })
}

const unZipFile = async (filePath, destinationPath) => {
await new Promise((resolve, reject) => {
  targz.decompress({
    src: filePath,
    dest: destinationPath}, function(err){
    if(err) {
        reject(err);
    } else {
        resolve()
    }
  })
})
}

const unZipGzFile = async (filePath, destinationPath) => {
  var rStream = fs.createReadStream(filePath)
  var wStream = fs.createWriteStream(destinationPath)
  const unzip = zlib.createGunzip()
  await new Promise((resolve, reject) => {
    rStream.pipe(unzip).pipe(wStream).on('error', reject).on('finish', resolve)
  })
}

module.exports = {
    downloadFile,
    unZipFile,
    unZipGzFile
}
