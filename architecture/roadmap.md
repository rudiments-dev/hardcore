# Rudimental Architecture road map

## memory of CRUD
* Commands and Events
* HTTP Port
* in-memory Adapter
* Memory as collection
* CRUD

## pipes and vents
* Features as traits on Command
* Usage of features in Pipes
    * Lock by Type and ID
    * remember in Memory
    * cache

## lucid dreams
* Memory in Kafka as stream
* streaming operations over memory
* websocket as provider of memory

## keep the ground
* SQL adapter
* meta-data for SQL mapping of Queries
* draft of mapping Meta -> SQL schema

## sequence of events
* Service requires events to process the command
* meta-data about service and it's dependencies

## register the types
* configurable (at runtime) meta on Maps
* configurable mapping
* SerDe to SQL and HTTP/Json

## manage component
* Service operations - sequence of produced commands, mapping events to result event