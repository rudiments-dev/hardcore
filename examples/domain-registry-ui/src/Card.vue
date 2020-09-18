<template>
  <div>
    <vue-json-pretty
        :path="'res'"
        :data="{ key: 'value' }"
        @click="handleClick">
    </vue-json-pretty>
  </div>
</template>

<script>
import VueJsonPretty from 'vue-json-pretty'
<vue-json-pretty :data="$store.state.catsList"
export default {
  components: {
    VueJsonPretty
  }
}
</script>