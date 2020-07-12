import Axios from 'axios'

const state = {
    todos : null
}

const getters = {
    TODOS : state => {
        return state.todos;
    }
}

const mutations = {
    SET_TODO : (state, payload) => {
        state.todos = payload
    },
    ADD_TODO : (state, payload) => {
            state.todos.push(payload)
        },
    TOGGLE_TODO: (state, payload) => {
            let item = state.todos.find(todo => todo.id === payload);
            item.completed = !item.completed;
        },
    DELETE_TODO: (state, payload) => {
            let index = state.todos.findIndex(todo => todo.id === payload);
            state.todos.splice(index, 1);
        }
}
    const actions = {
        GET_TODO : async (context) => {
            let { data } = await Axios.get(process.env.VUE_APP_BACKEND + '/todo')
            context.commit('SET_TODO', data)
        },
        ADD_TODO : async (context, payload) => {
            let { data } = await Axios.post(process.env.VUE_APP_BACKEND + '/todo', payload)
            context.commit('ADD_TODO', data)
        },
    }

export default {
    state,
    getters,
    mutations,
    actions
}