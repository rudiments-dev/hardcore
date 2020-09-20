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
export default {
  components: {
    VueJsonPretty
  }
}
</script>