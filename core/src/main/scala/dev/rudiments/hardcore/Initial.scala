package dev.rudiments.hardcore

import dev.rudiments.hardcore.Predicate.Anything

object Initial {
  val types: ID = ID("types")
  private val predicate: Declared = Declared(types / "Predicate")

  def init(ctx: Context): Unit = {
    val tx = new Tx(ctx)
    locations(tx)
    plain(tx)
    predicates(tx)

    tx += types / "Data" -> Type(
      Field("what", predicate),
      Field("data", Anything)
    )
    tx += types / "Agent" -> Nothing

    messages(tx)

    tx.>> match {
      case Prepared(c) => ctx << c match {
        case Committed(_) =>
        case _ => throw new IllegalStateException("Initial commit failed")
      }
      case _ => throw new IllegalStateException("Initial commit not prepared")
    }
  }

  private def messages(tx: Tx): Unit = {
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
        Field("crud", Index(tx ! (types / "Location"), Declared(types / "Event")))
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
        Field("values", Index(tx ! (types / "Location"), Anything))
      ),
      "NotExist" -> Nothing,
      "NotFound" -> Type(Field("missing", tx ! (types / "Location"))),
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
      tx += types / k -> v
    }

    tx += types / "CRUD" -> Memory.leafs(types, crud)
    tx += types / "In" -> Memory.leafs(types, crudIn)
    tx += types / "Out" -> Memory.leafs(types, crudOut)
    tx += types / "Error" -> Memory.leafs(types, crudErrors)

    tx += types / "Command" -> Memory.leafs(types, Map(
      "Create" -> crud("Create"),
      "Update" -> crud("Update"),
      "Delete" -> crud("Delete"),
      "Commit" -> crud("Commit"),
    ))

    tx += types / "Event" -> Memory.leafs(types, Map(
      "Created" -> crud("Created"),
      "Updated" -> crud("Updated"),
      "Deleted" -> crud("Deleted"),
      "Committed" -> crud("Committed")
    ))

    tx += types / "Query" -> Memory.leafs(types, Map(
      "Read" -> crud("Read"),
      "Find" -> crud("Find"),
      "Prepare" -> crud("Prepare"),
      "Verify" -> crud("Verify")
    ))

    tx += types / "Report" -> Memory.leafs(types, Map(
      "Readen" -> crud("Readen"),
      "Found" -> crud("Found"),
      "NotExist" -> crud("NotExist"),
      "NotFound" -> crud("NotFound"),
      "Prepared" -> crud("Prepared"),
      "Valid" -> crud("Valid")
    ))
  }

  private def predicates(tx: Tx): Unit = {
    val constants = Seq("All", "Anything", "Nothing")
    constants.foreach { s => tx += types / s -> Nothing}

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
        Field("where", tx ! (types / "Location")),
        Field("what", predicate)
      ),
      "Data" -> Type(
        Field("what", predicate),
        Field("data", Anything)
      ),
      "Declared" -> Type(Field("where", tx ! (types / "Location")))
    )

    composite.foreach { case (k, v) => tx += types / k -> v }

    tx += types / "Predicate" -> Memory.leafs(types, composite)
  }

  private def plain(tx: Tx): Unit = {
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
    plainTypes.foreach { case (k, v) => tx += types / k -> v }
    tx += types / "Plain" -> Memory.leafs(types, plainTypes)
  }

  private def locations(tx: Tx): Unit = {
    val id = Type(Field("key", Anything))
    val idLink = Link(types / "ID", id)

    val path = Type(Field("ids", Enlist(idLink)))
    val loc = Map(
      "ID" -> id,
      "Path" -> path,
      "Root" -> Nothing,
      "Unmatched" -> Nothing
    )

    loc foreach { case (k, v) => tx += types / k -> v }
    tx += types / "Location" -> Memory.leafs(types, loc)
  }
}
