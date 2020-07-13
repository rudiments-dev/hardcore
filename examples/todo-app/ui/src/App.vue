<template>
  <div id="app" class="w-full h-screen p-2">
    <Todo v-bind:todos="todos" @todoAdded="addTodo" @todoStatusChanged="changeTodoStatus"/>
    <div>{{ todoList }}</div>
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
  computed : {
    todoList() {
      return this.$store.getters.TODOS
    }
  }
,
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
      this.fetchTodos();
    },
    changeTodoStatus(todo) {
      console.log("todoId: " + todo.id + " todostatus: " + todo.done )
      if(todo.done) {
        axios({
          method: 'post',
          url: process.env.VUE_APP_BACKEND + '/todo/' + todo.id + '/done',
          data: {}
        });
        console.log("todoId: " + todo.id + " todostatus: " + todo.done )
      }
      else {
        axios({
          method: 'post',
          url: process.env.VUE_APP_BACKEND + '/todo/' + todo.id + '/undone',
          data: {}
        });
        console.log("todoId: " + todo.id + " todostatus: " + todo.done )
      }
      todo.id = ''
      setTimeout(function() { this.fetchTodos() }, 500);
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
