# Component Management
Managed components can work only over the Type Registry.

## Managed Service
Management provides in -> out mapping, including
* awaiting dependencies through Memory
* sync execution of internal `Commands`
* async execution management where possible
* boundaries can be defined in a Pipeline
* dependency management

## Managed Pipeline
We can make some `Pipes` managed by `Categories` without serious investment:
* `Memory` management
    * remember `Unsafe Commands` and it's `Events` except `LifeCycle Events`
    * remember `Safe Commands` and facts of it `Complete Event` (for analysis)
    * register `LifeCycle Events` in `Context Memory`
    * be able to restore `Context Memory` from `Eventual Memory`
    * persist `Eventual Memory`
    * notify `Ports`, `Adapters` and `Services` about `Events` through `Stream Projection`
* `Tx` management
    * Require
    * RequiresNew
    * Supports
    * NotSupports
* `Lock` management
    * based on `Types`
    * based on `IDs` (expecting IDs of Aggregates)
    * can await until timeout, drop after reach or queue limit, etc.
* `Cache` management
    * on `Adapter` can cache `Safe Commands` results and evict on `Unsafe Commands`
    * on `Port` can cache `Safe Commands` results and evict on `Unsafe Commands`
* `Auth` management
    * memory based
    * in future - OpenAuth, LDAP, Certificate
    * RBAC
    * RACI as form of RBAC
    * manual modify roles and groups
* Privacy protection
    * hide passwords
    * encrypt privacy data with salted key
* Deduplication - possibly, based on Hash and time window. Size of window dictates by size of memory and it's growth
* Back-pressure and Circuit breaker based on rate of downstream and queue

## Managed Ports and Adapters
Use various mapping and `SerDe` from `Type System` to handle `Commands` and return `Events`.

## Use Cases
* analytics-managed applications
* `Impact Analisys` based on sequence of mappings in `Service`, `Adapter` and `Port`
* business-readable log (`Eventual Memory`) and stack-trace (`Context Projection`) with history of values.
* propagation of new requirements - help to map new values from the root of changes
* agnostic change of EventStore and DataBase
* `Command` and `Event` based tests. Integration test in form of unit-test. `Cache` put instead of mocks of DB and ES.
* migration based on `Impact Analisys`, reconciliation, and changelog projection
* data discovery and service discovery support, API publication
* derivation and composition of runnable artifacts
* clustering (based on `Types` and `IDs`)
