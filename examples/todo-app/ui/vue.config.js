module.exports = {
    devServer: {
        proxy: process.env.VUE_APP_BACKEND,
        port: 3000
    }
}