<template>
  <div id="app">
    /api/domain
  <type-list v-for="type in types" :type="type" :key="type" />
  </div>
</template>

<script>

import axios from 'axios'
import TypeList from './components/TypeList.vue'

export default {
  name: 'App',
  components: { TypeList },
  data: () => ({
    types: []
  }),

  created() {
    axios.get('http://localhost:8765/api/domain')
    .then(response => {
      this.types = response.data
    })
    .catch(e => {
      this.errors.push(e)
    })
  }
}
</script>

<style lang="less">
html, body {
  margin: 0;
  margin-left: 10px;
  background-color: #f9f9f9;
}
.example {
  position: relative;
  padding: 0 15px;
  margin: 0 auto;
  width: 1200px;
}
.example-box {
  margin: 0 -15px;
  overflow: hidden;
  h3 {
    display: inline-block;
  }
  .title {
    text-align: center;
  }
  .block {
    float: left;
    padding: 0 15px;
    width: 50%;
    box-sizing: border-box;
  }
  input,
  select,
  textarea {
    padding: 3px 8px;
    box-sizing: border-box;
    border-radius: 5px;
    border: 1px solid #bbb;
    font-family: inherit;
    &:focus {
      outline: none;
      border-color: #1d8ce0;
      box-shadow: 0 0 3px #1d8ce0;
    }
  }
  textarea {
    width: 100%;
    height: 150px;
    resize: vertical;
  }
  pre {
    margin: 0;
    font-family: Consolas;
    overflow: hidden;
    text-overflow: ellipsis;
  }
  .options {
    font-size: 14px;
  }
}
</style>
