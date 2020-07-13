import Axios from 'axios'
import Vuex from 'vuex'
import Vue from 'vue'

Vue.use(Vuex)

export default new Vuex.Store({
    state: {
      todos: null
    },
    getters: {
        TODOS: state => {
            return state.todos;
    }},
    mutations: {
        SET_TODO: (state,payload) => {
            state.todos = payload
        },
        ADD_TODO: (state,payload) => {
            state.todos.push(payload)
        },
    },
    actions: {
        GET_TODO: async (context) => {
            let { data } = await Axios.get(process.env.VUE_APP_BACKEND + '/todo')
            context.commit('SET_TODO', data)
        },
        SAVE_TODO: async (context, payload) => {
            let { data } = await Axios.post(process.env.VUE_APP_BACKEND + '/todo', payload)
            context.commit('ADD_TODO', data)
        }
    }
})