<template>
  <div id="app">
    <navigation />
    <div class="flex flex-wrap flex-grow h-screen">
      <div class="w-full md:w-1/6 bg-blue-500 p-6 text-left text-gray-200">
        <div class="order-3 mt-2 flex-shrink-0 w-full sm:order-2 sm:mt-0 sm:w-auto">
          <a href="#" class="flex items-center justify-center px-4 py-2 border border-transparent rounded-md shadow-sm text-sm font-medium text-indigo-600 bg-white hover:bg-indigo-50">
            <router-link
                to="/create-type"
                v-slot="{ href, route, navigate, isActive }"
            >
              <NavLink :active="isActive" :href="href" @click="showTypeCreateForm(false)"
              >Create Type</NavLink
              >
            </router-link>
          </a>
        </div>


        <ul>
          <li v-for="type in orderedTypes" :key="type.name"><a href="#" @click="setCurrentType(type.name); showTypeCreateForm(true)">{{ type.name }}</a></li>
        </ul>
      </div>
      <div class="w-full md:w-5/6 p-4 text-left">
        <create-type v-if="modeEdit === false" />
        <type-card v-else :type=currentType />
      </div>
    </div>
  </div>
</template>

<script>

import _ from 'lodash'
import axios from 'axios'
import TypeCard from './components/TypeCard.vue'
import Navigation from './components/Navigation.vue'
import CreateType from "@/components/CreateType";

export default {
  name: 'App',
  components: {CreateType, TypeCard, Navigation },
  data: () => ({
    types: [],
    currentType: null,
    modeEdit: false,
  }),
  methods: {
    setCurrentType(typeName) {
      let valObj = this.types.filter(function(elem){
        if(elem.name == typeName) return elem;
      });
      this.currentType = valObj[0];

      this.$router.push(`/api/domain/${this.currentType.name}`)
    },
    showTypeCreateForm(state) {
      this.modeEdit = state;
    }
  },
  computed: {
    orderedTypes: function() {
      return _.orderBy(this.types, 'name')
    }
  },
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
