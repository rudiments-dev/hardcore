# Type Management
In Rudimental Architecture expected, that Developer made all the types of Commands, Events and Domain entities in preferred language.

But it's not a constraint.
All `Components` can work all the same with runtime-managed objects.
It means, `Types`, their `Fields`, mapping between other `Types` and their `Fields`, `Serialization` and `Deserialization` (`SerDe`) can be managed at runtime in some application.
With domain-separated modules (`Sub-Domains`).

## Type Registry
Holds information about `Types` and `Fields`, mapping between `Types`.

### Types
Filled with `Fields`, `Categories` (mostly marker interfaces) and `Methods` (distant future), unique identified.

Mapping is the key mechanism of transferring `Fields` between different `Types`.

Inheritance allows transfer `Fields`, `Categories` and `Methods` between `Types` in order to structure them.

### Fields
Are strictly typed. Can be any serializable type, but strongly preferred DTO-compatible:
* number
* logical (true/false, y/n, enums)
* string
* date and time
* self ID
* reference (ID[T])
* other DTO
* collection (set, array, map) of any above

Also fields can be marked with some features, like
* optional flag
* autogenerated ID
* security mark (hide passwords)
* privacy mark (censor privacy data, require crypto)
* grants for different user groups
* version of change
* problems mark (something goes wrong and should be fixed)

### Mapping
Bounds at least 2 `Types` as conversion one to another.
Requires every field of output `Type`
* taken and possibly transformed from input `Type`
* or has a default value, possibly with logic to calculate