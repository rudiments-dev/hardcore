<template>
  <div>
    <h3 class="text-1xl font-bold leading-7 text-gray-900 sm:text-xl sm:leading-9 sm:truncate">{{ type.name }}</h3>
    <vue-json-pretty
        :deep=4
        :showLine=true
        :data=type
        @click="handleClick">
    </vue-json-pretty>
  </div>
</template>

<script>
import VueJsonPretty from 'vue-json-pretty'
export default {
  props: {
    type: Object
  },
  components: {
    VueJsonPretty
  }
}
</script>