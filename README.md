# Hard Core

[![Build Status](https://travis-ci.org/rudiments-dev/hardcore.svg?branch=master)](https://travis-ci.org/rudiments-dev/hardcore)
[![Maven Central](https://img.shields.io/maven-central/v/dev.rudiments/implementation.svg?label=Maven%20Central)](https://search.maven.org/search?q=g:%22dev.rudiments%22%20AND%20a:%22implementation%22)

Research project and bootstrap library for mastering:

* akka-http
* akka-streams
* circe.io
* enumeratum
* cats-effects
* doobie

* kafka
* elasticsearch
* gradle
* docker

## Skill DSL

Skill - ability to execute certain Commands and save produced Events into Memory, providing some kind of Execution Inversion ahead of EventSourcing.
Skill produces Action, which ask Memory about Command:

* if Command hadn't been executed earlier, Action will execute function
* if another Action executes Command right now - current Action will wait until finish
* if Command had been executed - resulting Event will be provided
  * if Event is Error - async Future will be failed
  * in other cases - async Future success with Event

HardSkill can handle Command on his own.

DependentSkill Requires Memory to provide dependent Event, and provides Resolver to convert incoming Command into dependent Command.

If Memory doesn't know about dependent Commands (no Action for dependency hadn't been executed), it will try to use SkillSet to execute dependent Command.

## Actor Implementation

ActorMemory - implements Memory and SkillSet, can be extended #withSkill(HardSkill) and #withDependency(Resolver)(DependentSkill), holds complete state in MemoryActor.

ActorHardSkill - provides async implementation via ActorAction, which communicates with MemoryActor about LifeCycle.

DependentSkill - provides async implementation via DependentActorAction, which communicates with MemoryActor about dependency and LifeCycle.
