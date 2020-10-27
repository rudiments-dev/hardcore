<template>

  <div>
    <div class="w-full md:w-5/6">
    <table class="table-auto">
      <thead>
        <tr>
          <th class="border px-4 py-2 bg-gray-600 text-white" colspan="3">
            {{type.name}}
          </th>
        </tr>
        <tr>
          <th class="border px-4 py-2">Name</th><th class="border px-4 py-2">
            Thing
          </th>
          <th class="border px-4 py-2">
            isRequired
          </th>
        </tr>
      </thead>
      <tbody>
        <tr v-for="(value, name) in type.fields" :key="name">
          <td class="border px-4 py-2">{{name}}</td>
          <td class="border px-4 py-2">{{value.thing.type}}</td>
          <td class="border px-4 py-2">{{value.isRequired}}</td>
        </tr>
      </tbody>
    </table>
    </div>
    <div class="w-full md:w-1/3">
      <vue-json-pretty
        :deep=4
        :showLine=true
        :data=type
        @click="handleClick">
      </vue-json-pretty>
    </div>
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