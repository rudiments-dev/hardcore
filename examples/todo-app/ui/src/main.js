import App from './App.vue'
import Vue from 'vue';
import Vuex from 'vuex';
import todos from './modules/todos';

Vue.use(Vuex);

export const store = new Vuex.Store({
  state: {},
  getters: {},
  mutations: {},
  actions: {},
  modules: {
    todos,
  },
});

import './assets/css/tailwind.css'

Vue.config.productionTip = false

new Vue({
  render: h => h(App),
  store,
}).$mount('#app')
