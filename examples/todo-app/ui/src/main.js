import App from './App.vue'
import Vue from 'vue'
import Vuex from 'vuex'
import store from './store/store.js'
import './assets/css/tailwind.css'

Vue.config.productionTip = false

Vue.use(Vuex);

new Vue({
  render: h => h(App),
  store,
}).$mount('#app')
