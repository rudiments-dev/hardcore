package dev.rudiments.hardcore

import dev.rudiments.hardcore.Predicate.Anything

object Initial {
  val types: ID = ID("types")
  private val predicate: Declared = Declared(types / "Predicate")

  def init(mem: Memory): Unit = {
    locations(mem)
    plain(mem)
    predicates(mem)

    mem += types / "Data" -> Type(
      Field("what", predicate),
      Field("data", Anything)
    )
    mem += types / "Agent" -> Nothing

    messages(mem)
  }

  private def messages(mem: Memory): Unit = {
    val message = Declared(types / "Message")
    val crudIn = Map(
      "Create" -> Type(Field("what", Anything)),
      "Read" -> Nothing,
      "Update" -> Type(Field("what", Anything)),
      "Delete" -> Nothing,
      "Find" -> Type(Field("p", predicate)),
      "Prepare" -> Nothing,
      "Verify" -> Nothing,
      "Commit" -> Type(
        Field("crud", Index(mem ! (types / "Location"), Declared(types / "Event")))
      )
    )

    val commitLink = Link(types / "Commit", crudIn("Commit"))

    val crudOut = Map(
      "Created" -> Type(Field("data", Anything)),
      "Readen" -> Type(Field("data", Anything)),
      "Updated" -> Type(Field("old", Anything), Field("data", Anything)),
      "Deleted" -> Type(Field("old", Anything)),
      "Found" -> Type(
        Field("p", predicate),
        Field("values", Index(mem ! (types / "Location"), Anything))
      ),
      "NotExist" -> Nothing,
      "NotFound" -> Type(Field("missing", mem ! (types / "Location"))),
      "Prepared" -> Type(Field("commit", commitLink)),
      "Committed" -> Type(Field("commit", commitLink)),
      "Valid" -> Nothing
    )

    val crudErrors = Map(
      "AlreadyExist" -> Type(Field("data", Anything)),
      "Conflict" -> Type(
        Field("that", message),
        Field("other", message)
      ),
      "NotImplemented" -> Nothing
    )

    val crud = crudIn ++ crudOut ++ crudErrors

    crud.foreach { case (k, v) =>
      mem += types / k -> v
    }

    mem += types / "CRUD" -> MemoryNode.leafs(types, crud)
    mem += types / "In" -> MemoryNode.leafs(types, crudIn)
    mem += types / "Out" -> MemoryNode.leafs(types, crudOut)
    mem += types / "Error" -> MemoryNode.leafs(types, crudErrors)

    mem += types / "Command" -> MemoryNode.leafs(types, Map(
      "Create" -> crud("Create"),
      "Update" -> crud("Update"),
      "Delete" -> crud("Delete"),
      "Commit" -> crud("Commit"),
    ))

    mem += types / "Event" -> MemoryNode.leafs(types, Map(
      "Created" -> crud("Created"),
      "Updated" -> crud("Updated"),
      "Deleted" -> crud("Deleted"),
      "Committed" -> crud("Committed")
    ))

    mem += types / "Query" -> MemoryNode.leafs(types, Map(
      "Read" -> crud("Read"),
      "Find" -> crud("Find"),
      "Prepare" -> crud("Prepare"),
      "Verify" -> crud("Verify")
    ))

    mem += types / "Report" -> MemoryNode.leafs(types, Map(
      "Readen" -> crud("Readen"),
      "Found" -> crud("Found"),
      "NotExist" -> crud("NotExist"),
      "NotFound" -> crud("NotFound"),
      "Prepared" -> crud("Prepared"),
      "Valid" -> crud("Valid")
    ))
  }

  private def predicates(mem: Memory): Unit = {
    val constants = Seq("All", "Anything", "Nothing")
    constants.foreach { s => mem += types / s -> Nothing}

    val field = Type(
      Field("name", Declared(ID("String"))),
      Field("of", predicate)
    )
    val fieldLink = Link(types / "Field", field)

    val composite = Map(
      "Field" -> field,
      "Type" -> Type(
        Field("fields", Enlist(fieldLink))
      ),
      "Enlist" -> Type(Field("of", predicate)),
      "Index" -> Type(Field("of", predicate), Field("over", predicate)),
      "AnyOf" -> Type(Field("p", Enlist(predicate))),
      "Link" -> Type(
        Field("where", mem ! (types / "Location")),
        Field("what", predicate)
      ),
      "Data" -> Type(
        Field("what", predicate),
        Field("data", Anything)
      ),
      "Declared" -> Type(Field("where", mem ! (types / "Location")))
    )

    composite.foreach { case (k, v) => mem += types / k -> v }

    mem += types / "Predicate" -> MemoryNode.leafs(types, composite)
  }

  private def plain(mem: Memory): Unit = {
    val plainTypes = Map(
      "Text" -> Type(
        Field("maxSize", Declared(ID("Int")))
      ),
      "Number" -> Type(
        Field("from", Anything),
        Field("to", Anything)
      ),
      "Bool" -> Nothing,
      "Binary" -> Nothing
    )
    plainTypes.foreach { case (k, v) => mem += types / k -> v }
    mem += types / "Plain" -> MemoryNode.leafs(types, plainTypes)
  }

  private def locations(mem: Memory): Unit = {
    val id = Type(Field("key", Anything))
    val idLink = Link(types / "ID", id)

    val path = Type(Field("ids", Enlist(idLink)))
    val loc = Map(
      "ID" -> id,
      "Path" -> path,
      "Root" -> Nothing,
      "Unmatched" -> Nothing
    )

    loc foreach { case (k, v) => mem += types / k -> v }
    mem += types / "Location" -> MemoryNode.leafs(types, loc)
  }
}
