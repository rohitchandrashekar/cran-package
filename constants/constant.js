// ideally the URI salt and other config props would go into .env file based on ENV's
module.exports = Object.freeze({
    MONGO_URI: 'mongodb://127.0.0.1/packages',
    PORT: 3004,
    DOWNLOAD_MAIN_PACKAGE: false,
    MAIN_PACKAGE_URL: `http://cran.r-project.org/src/contrib/PACKAGES.gz`,
    MAIN_PACKAGE_DOWNLOAD_DEST: `./downloads/mainPackages/PACKAGES.gz`
})
  