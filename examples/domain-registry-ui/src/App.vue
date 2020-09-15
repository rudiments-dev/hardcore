<template>
  <div id="app">
    Vue.js / Tailwind CSS framework
    <VueJsonPretty/>
  </div>
</template>

<script>
import VueJsonPretty from './Card.vue'

  export default {
    name: 'App',
    components: {VueJsonPretty}
  }
</script>

<style>
#app {
  font-family: Avenir, Helvetica, Arial, sans-serif;
  -webkit-font-smoothing: antialiased;
  -moz-osx-font-smoothing: grayscale;
  text-align: center;
  color: #2c3e50;
  margin-top: 60px;
}
</style>
