<template>
<div class="max-w-sm p-1 container-todos rounded-lg pt-2">
    <div class="font-bold text-base ml-2">Todo</div>
  <ol>
    <li v-for="todo in todos" v-bind:key="todo.name">
      <div class="rounded-md overflow-hidden bg-white m-2">
        <div class="p-2">
          <div class="font-bold text-xl mb-2">{{ todo.name }}</div>
          <p class="text-gray-700 text-base">
            {{ todo.comment }}
          </p>
          <div class="font-light text-sm mb-2">Done: {{ todo.done }}</div>
        </div>
      </div>
    </li>
  </ol>
<form class="w-full max-w-sm">
  <div class="flex items-center border-b border-b-2 py-2 mr-2">
    <input
        class="appearance-none bg-transparent border-none w-full text-gray-700 mr-3 py-1 px-2 leading-tight focus:outline-none"
        type="text"
        placeholder="Todo text"
        aria-label="Todo name"
        v-model="todoName"

    />
    <button class="flex-shrink-0 bg-blue-500 hover:bg-blue-700 border-blue-500 hover:border-blue-700 text-sm border-2 text-white py-1 px-2 rounded"
            type="button"
            @click.prevent="createNew"
    >
      Post
    </button>
  </div>
</form>
</div>
</template>

<script>
    export default {
      data: function() {
        return {
          todoName: ''
        };
      },
      methods: {
        createNew() {
          console.log(this.todoName)
          this.$emit('todoAdded', this.todoName);
          this.todoName = ''
        }
      },
      props: ['todos']
    }
</script>

<style>
.container-todos {
    background: #E5EFF6
}
</style>