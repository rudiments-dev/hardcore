module.exports = {
  devServer: {
    proxy: process.env.VUE_APP_BACKEND,
    port: process.env.VUE_APP_FRONTEND_PORT
  },
};