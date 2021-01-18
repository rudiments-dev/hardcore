<template>
  <div>
    <div class="grid grid-cols-2 gap-4">
      <div>
        <div class="bg-white shadow overflow-hidden sm:rounded-lg">
          <div class="px-3 py-3 sm:px-6">
            <h3 class="text-lg leading-6 font-medium text-gray-900">
              {{ type.name }}
            </h3>
            <p class="mt-1 max-w-2xl text-sm text-gray-500">
              Click on fields to adjust the values
            </p>
          </div>
          <div class="border-t border-gray-200">
            <dl>
              <div class="bg-gray-50 px-4 py-5 sm:grid sm:grid-cols-3 sm:gap-4 sm:px-6">
                <dt class="text-sm font-medium text-gray-500">
                  Type
                </dt>
                <dd class="mt-1 text-sm text-gray-900 sm:mt-0 sm:col-span-2">
                  {{ type.type }}
                </dd>
              </div>
              <div class="bg-white px-4 py-5 sm:grid sm:grid-cols-3 sm:gap-4 sm:px-6">
                <dt class="text-sm font-medium text-gray-500">
                  Type Name
                </dt>
                <dd class="mt-1 text-sm text-gray-900 sm:mt-0 sm:col-span-2">
                  {{ type.name }}
                </dd>
              </div>
              <div class="bg-gray-50 px-4 py-5 sm:grid sm:grid-cols-3 sm:gap-4 sm:px-6">
                <dt class="text-sm font-medium text-gray-500">
                  Full Name
                </dt>
                <dd class="mt-1 text-sm text-gray-900 sm:mt-0 sm:col-span-2">
                  {{ type.fullName }}
                </dd>
              </div>

              <div class="bg-white px-4 py-5 sm:grid sm:grid-cols-3 sm:gap-4 sm:px-6">
                <dt class="text-sm font-medium text-gray-500">
                  Fields
                </dt>
                <dd class="mt-1 text-sm text-gray-900 sm:mt-0 sm:col-span-2">
                  <ul class="border border-gray-200 rounded-md divide-y divide-gray-200">
                    <li class="pl-3 pr-4 py-3 flex items-center justify-between text-sm">
                      <div class="w-0 flex-1 flex items-center">

                        <span class="ml-2 flex-1 w-0 truncate">
                  {{ type.fields}}
                </span>
                      </div>

                    </li>

                  </ul>
                </dd>
              </div>
            </dl>
          </div>
        </div>
      </div>

      <div>
        JSON Representation (DEBUG: ON)
        <div class="w-full md:w-1/3">
          <vue-json-pretty
              :deep=4
              :showLine=true
              :data=type
              @click="handleClick">
          </vue-json-pretty>
        </div>
      </div>


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