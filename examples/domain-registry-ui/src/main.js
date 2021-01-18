import Vue from 'vue'
import App from './App.vue'
import './assets/css/tailwind.css'
import VueRouter from 'vue-router'
import TypeCard from "@/components/TypeCard";
import AddType from "@/components/AddType";

Vue.use(VueRouter);

const routes = [
  {
    path: "",
    component: TypeCard,
  },
  {
    path: "add-type",
    component: AddType
  }
];

const router = new VueRouter({
  mode: 'history',
  routes
});

Vue.config.productionTip = false

new Vue({
  router,
  render: h => h(App),
}).$mount('#app')
