<template>
  <div id="app" class="w-full h-screen p-2">
    <Todo v-bind:todos="todos" @todoAdded="addTodo" />
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
      newTodo: '',
      errors: []
    }
  },
  watch: {
    todos: {
      handler: 'fetchTodos'
    }
  },
  methods: {
    addTodo(todoName) {
      axios({
        method: 'post',
        url: process.env.VUE_APP_BACKEND + '/todo',
        data: {
          name: todoName,
          comments: '',
          id: this.todos.length+500,
          done: false
        }
      });
      this.newTodo = '';
    },
    fetchTodos() {
      axios.get(process.env.VUE_APP_BACKEND + "/todo")
          .then(response => {
            this.todos = response.data
          })
          .catch(e => {
            this.errors.push(e)
          })
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
