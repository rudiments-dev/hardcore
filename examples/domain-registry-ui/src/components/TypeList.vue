<template>
  <div>
    <li>{{ type.name }}</li>
  </div>
</template>

<script>
  export default {
    props: {
      type: Object
    }
  }
</script>