# Hard Core

[![Build Status](https://dev.azure.com/rudiments-dev/hardcore/_apis/build/status/rudiments-dev.hardcore?branchName=master&jobName=Run%20unit%20testing%20with%20gradlew)](https://dev.azure.com/rudiments-dev/hardcore/_build/latest?definitionId=2&branchName=master)
[![codecov](https://codecov.io/gh/rudiments-dev/hardcore/branch/develop/graph/badge.svg)](https://codecov.io/gh/rudiments-dev/hardcore)
[![Maven Central](https://img.shields.io/maven-central/v/dev.rudiments/implementation.svg?label=Maven%20Central)](https://search.maven.org/search?q=g:%22dev.rudiments%22%20AND%20a:%22implementation%22)

Research project and bootstrap library.

## Gradle project

```
apply plugin: 'java'
repositories {
    mavenCentral()
    maven { url "https://oss.sonatype.org/content/repositories/snapshots" }
}

sourceCompatibility = 1.8
targetCompatibility = 1.8

dependencies {
    implementation 'dev.rudiments:type-registry:0.3-SNAPSHOT'
    implementation 'dev.rudiments:sql:0.3-SNAPSHOT'
    implementation 'dev.rudiments:implementation:0.3-SNAPSHOT'
}
```
