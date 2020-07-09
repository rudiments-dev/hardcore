import Vue from 'vue'
import Vuex from 'vuex'
import axios from "axios";

Vue.use(Vuex)

export default new Vuex.Store({
    state: {
        todos: axios.get(process.env.VUE_APP_BACKEND + "/todo")
            .then(response => {
                this.todos = response.data
            })
            .catch(e => {
                this.errors.push(e)
            })
    },
})