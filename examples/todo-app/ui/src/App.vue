<template>
  <div id="app" class="w-full h-screen p-2">
    <Todo v-bind:todos="todos" />
  </div>
</template>

<script>
import Todo from './components/Todo.vue'
import axios from 'axios'

export default {
  name: 'App',
  components: {
    Todo
  },
  data: function() {
    return {
      todos: {},
      errors: []
    }
  },
    created() {
    axios.get(process.env.VUE_APP_BACKEND + "/todo")
    .then(response => {
      this.todos = response.data
    })
    .catch(e => {
      this.errors.push(e)
    })
  }
}
</script>

<style>
#app {
  background: #406CC3
}
</style>
