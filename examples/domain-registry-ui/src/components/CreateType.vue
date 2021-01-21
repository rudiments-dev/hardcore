<template>
  <div class="mt-10 sm:mt-0">
    <div class="md:grid md:grid-cols-4 md:gap-6">
<!--      {-->
<!--      "type": "Spec",-->
<!--      "name": "SpecExample",-->
<!--      "fullName": "dev.rudiments.Examples.SpecExample",-->
<!--      "fields": {-->
<!--      "b": {-->
<!--      "thing": {-->
<!--      "type": "Bool"-->
<!--      },-->
<!--      "isRequired": true-->
<!--      }-->
<!--      }-->
<!--      }-->
      <div class="mt-5 md:mt-0 md:col-span-2">
        <form action="#" method="POST">
          <div class="shadow overflow-hidden sm:rounded-md">
            <div class="px-4 py-5 bg-white sm:p-6">
              <div class="grid grid-cols-6 gap-6">
                <div class="col-span-6 sm:col-span-4">
                  <label for="type" class="block text-sm font-medium text-gray-700">Type</label>
                  <input type="text" name="Type" id="type" class="mt-1 focus:ring-indigo-500 focus:border-indigo-500 block w-full shadow-sm sm:text-sm border-black rounded-md">
                </div>

                <div class="col-span-6 sm:col-span-4">
                  <label for="name" class="block text-sm font-medium text-gray-700">Name</label>
                  <input type="text" name="name" id="name" class="mt-1 focus:ring-indigo-500 focus:border-indigo-500 block w-full shadow-sm sm:text-sm border-gray-300 rounded-md">
                </div>

                <div class="col-span-6 sm:col-span-5">
                  <label for="full_name" class="block text-sm font-medium text-gray-700">Full Name</label>
                  <input type="text" name="full_name" id="full_name" class="mt-1 focus:ring-indigo-500 focus:border-indigo-500 block w-full shadow-sm sm:text-sm border-gray-300 rounded-md">
                </div>
                <div class="col-span-6 sm:col-span-5">
                  <label for="fields" class="block text-sm font-medium text-gray-700">Fields</label>
                  <input type="text" name="fields" id="fields" class="mt-1 focus:ring-indigo-500 focus:border-indigo-500 block w-full shadow-sm sm:text-sm border-gray-300 rounded-md">
                </div>
              </div>
            </div>
            <div class="px-4 py-3 bg-gray-50 text-right sm:px-6">
              <button type="submit" class="inline-flex justify-center py-2 px-4 border border-transparent shadow-sm text-sm font-medium rounded-md text-white bg-indigo-600 hover:bg-indigo-700 focus:outline-none focus:ring-2 focus:ring-offset-2 focus:ring-indigo-500">
                Save
              </button>
            </div>
          </div>
        </form>
      </div>
    </div>
  </div>
</template>