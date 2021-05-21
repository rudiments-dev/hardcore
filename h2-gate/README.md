# H2 Gate
To run registry:
1. `./gradlew serveH2` - run local H2 server with TCP connections
2. `./gradlew gateH2` - run registry on localhost:8080

### Requests:
* `GET /health` - always OK
* `GET /db/` - list of schemes
* `POST /db/inspect` - inspect DB to registry
* `GET /db/:schema/` - tables of :schema
