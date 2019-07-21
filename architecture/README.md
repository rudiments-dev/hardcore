# Rudimental architecture

## Core concepts
Terms from Domain-Driven Design (especially Hexagonal Architecture), but with extra constraints.
* `Domain` - business-entities and business-operations.

### Components
* `Services` - application management
  * consumes `Commands`
  * produces `Events`
  * can pass `Commands` and `Events` to other `Services`, `Adapters` and `Ports`
  * can call `Domain` operations and entities
* `Adapter` - client
  * consumes `Commands` from `Services`
  * requests external systems
  * produces `Events` back to `Services`
* `Port` - server
  * consumes `Messages` from external systems
  * produces `Commands` to `Services` through `Pipeline`
  * respond to external systems with `Events` through `Drainage`

### API
* `Command` - representation of function call
  * opening boundary of some functionality (action, task, tx, etc.)
  * requires finalization Event
  * usually only one success final Event
* `Event` - representation of side-effect
  * `Complete Event` - return of function, represented by `Command`
  * `Error Event` - like exception, often stops further processing or starts fallback actions
  * other `Events` - logs of intermediate results
* state of the application should be determined by incoming `Commands` and can be restored from the happened `Events`. No IO or business-valuable side-effect without `Command` and `Event`!

## Second-level terms
### Pipeline
Chain of filters (`Pipes`), specialized services.
* Command bypass every `Pipe` it fits (by `Type`, `ID`, etc.)
* `Pipe` can
    * pass `Command` further
    * provide new `Event` (possibly `Error`) and return it to sender (`Service` or `Port`)
    * modify internal `State` (possibly shared with `Drain`)
    * wrap `Command` with some context
    * call other `Services`, `Adapters` and `Drains`

Use cases
* Tx management
* borrow resource
* auth
* logging
* caching
* locking
* validations
* deduplication
* circuit breaker
* back-pressure

Pretty simple - `Pipeline` can replace AOP before-processors.

### Drainage
Chain of post-processors (`Drains`), specialized services.
* `Event` bypass every `Drain` it fits (by `Type`, `ID`, etc.)
* `Drain` can
    * pass `Event` further
    * provide new `Event` and replace return `Event` with it
    * modify internal `State` (possibly, shared with `Pipe`)
    * unwrap or finalize context
    * call other `Services`, `Adapters` and `Pipes`
* in case of error in `Drain` `Event` usually pass further

Use cases
* close connections
* close Tx
* cache invalidation (limited by Type, ID, etc.)
* logging
* notifications
* release resources
* post-validations
* data limitation - erase confidential data from output

Pretty simple - `Drainage` can replace `final` keyword and AOP after-processors.

### Router
Generalized `Service` DSL
* mapping of values for generated `Commands` and `Events` from consumed `Command` and `Memory`
* calling (down-streaming) `Commands` and `Events` to other `Services`, `Adapters` and `Ports`
* conditional calls
* sequential calls

Use cases
* choose down-stream by `Type`, `ID`, etc. of `Command` or `Event`
* choose down-stream by values in `Command`, `Event` or `Memory`
* consume Command or Event, usually providing new `Events` or `Commands`
* iterate over finite content of `Command`, `Event` or `Memory`
* notify User with `Command` as `User Task` and await `User Action` (special `Event` from `User`)

### Memory and Projections
* `Memory` is a relation between consumed `Commands` and produced `Events`. Usually managed through `Pipeline` and `Drainage`.
* for `Memory` better terms will be `Cause` instead of `Command` and `Effect` instead of `Event`.
    * `Long-term Memory` - stream of consumed `Commands` and produced `Events`. Expected, that every `Event` caused by `Command` should reference it.
    * `Short-term Memory` - `State`, Map `Command` -> `Event` with actual (possibly last) `Event` for every `Command`.
* `Projection` is somehow processed Memory.
TODO: rename, all 4 starting with `S`
    * `Stage Projection` - projection of `Memory` to a single value, made by state machine. Can be cached or streamed.
    * `Sequence Projection` - filtered (by Type, ID, etc.) and transformed `Memory`. Can be cached and streamed.
    * `State Projection` - use `Sequence Projection` to modify internal `State`.
    * `Stream Projection` - `Memory` as `Stream` of `Commands` and `Events`. `Memory` itself acts like `Stream Projection`.
* often can be clustered by `Types` and `IDs` in `Sub-Domain`.
* safe (read-only) `Commands` can be memorized differently from unsafe (modification) `Commands`.

Use cases
* order processing
* audit
* migrations
* saga - long-running tasks with multiple interactions with external systems - just a projection
* simplify CRUD-persisted model (no logs and history tables)
* performance measuring and testing
* monitoring
* Tx rollback and compensating actions
* if persisted - recovery after system failures and restarts
* make mocks obsolete - can put expected result as `Completing Event` into `Memory`