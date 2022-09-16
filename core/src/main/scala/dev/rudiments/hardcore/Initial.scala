package dev.rudiments.hardcore

import dev.rudiments.hardcore.Predicate.Anything

object Initial {
  val types: ID = ID("types")
  private val predicate: Declared = Declared(types / "Predicate")

  def init(ctx: Memory): Unit = {
    val tx = new Tx(ctx)
    tx += types -> Node.empty

    {
      tx += types / "ID" -> Type(Field("key", Anything))
      tx += types / "Path" -> Type(Field("ids", Enlist(tx ! types / "ID")))
      tx += types / "Root" -> Nothing
      tx += types / "Unmatched" -> Nothing

      tx += types / "Location" -> Node.partnership(types, Seq("ID", "Path", "Root", "Unmatched"))
    }

    {
      tx += types / "Text" -> Type(
        Field("maxSize", Anything)
      )
      tx += types / "Number" -> Type(
        Field("from", Anything),
        Field("to", Anything)
      )
      tx += types / "Bool" -> Nothing
      tx += types / "Binary" -> Nothing

      tx += types / "Plain" -> Node.partnership(types, Seq("Text", "Number", "Bool", "Binary"))
    }

    {
      tx += types / "All" -> Nothing
      tx += types / "Anything" -> Nothing
      tx += types / "Nothing" -> Nothing

      tx += types / "Field" -> Type(
        Field("name", Text(Int.MaxValue)),
        Field("of", predicate)
      )
      tx += types / "Type" -> Type(
        Field("fields", Enlist(tx ! types / "Field"))
      )
      tx += types / "Enlist" -> Type(Field("of", predicate))
      tx += types / "Index" -> Type(Field("of", predicate), Field("over", predicate))
      tx += types / "AnyOf" -> Type(Field("p", Enlist(predicate)))
      tx += types / "Link" -> Type(
        Field("where", tx ! (types / "Location")),
        Field("what", predicate)
      )
      tx += types / "Declared" -> Type(Field("where", tx ! (types / "Location")))

      tx += types / "Predicate" -> Node.partnership(types, Seq(
        "Anything", "Nothing", "Type", "Enlist", "Index", "AnyOf", "Link", "Declared", "All"
      ))
    }

    {
      tx += types / "Data" -> Type(
        Field("what", predicate),
        Field("data", Anything)
      )

      tx += types / "Agent" -> Node.partnership(types, Seq("Node"))
      tx += types / "Node" -> Type(
        Field("self", Anything),
        Field("keyIs", predicate),
        Field("leafIs", predicate)
      )
    }

    {
      tx += types / "Message" -> Node.partnership(types, Seq("In", "Out"))
      tx += types / "In" -> Node.partnership(types, Seq("Query", "Command"))
      tx += types / "Out" -> Node.partnership(types, Seq("Report", "Event", "Error"))
      tx += types / "Query" -> Node.partnership(types, Seq("Read", "Find", "Prepare", "Verify"))
      tx += types / "Command" -> Node.partnership(types, Seq("Create", "Update", "Delete", "Commit"))
      tx += types / "Report" -> Node.partnership(types, Seq("Readen", "Found", "NotExist", "NotFound", "Prepared", "Valid"))
      tx += types / "Event" -> Node.partnership(types, Seq("Created", "Updated", "Deleted", "Committed"))
      tx += types / "Error" -> Node.partnership(types, Seq("AlreadyExist", "Conflict", "NotImplemented"))

      tx += types / "CRUD" -> Node.partnership(types, Seq(
        "Create", "Read", "Update", "Delete", "Find", "Prepare", "Verify", "Commit",
        "Created", "Readen", "Updated", "Deleted", "Found", "Prepared", "Valid", "Committed",
        "NotExist", "NotFound", "AlreadyExist", "Conflict", "NotImplemented"
      ))
    }

    {
      tx += types / "Create" -> Type(Field("what", Anything))
      tx += types / "Read" -> Nothing
      tx += types / "Update" -> Type(Field("what", Anything))
      tx += types / "Delete" -> Nothing
      tx += types / "Find" -> Type(Field("p", predicate))
      tx += types / "Prepare" -> Nothing
      tx += types / "Verify" -> Nothing
      tx += types / "Commit" -> Type(
        Field("crud", Index(tx ! types / "Location", Declared(types / "Event")))
      )

      tx += types / "Created" -> Type(Field("data", Anything))
      tx += types / "Readen" -> Type(Field("data", Anything))
      tx += types / "Updated" -> Type(Field("old", Anything), Field("data", Anything))
      tx += types / "Deleted" -> Type(Field("old", Anything))
      tx += types / "Found" -> Type(
        Field("p", predicate),
        Field("values", Index(tx ! types / "Location", Anything))
      )
      tx += types / "NotExist" -> Nothing
      tx += types / "NotFound" -> Type(Field("missing", tx ! types / "Location"))
      tx += types / "Prepared" -> Type(Field("commit", tx ! types / "Commit"))
      tx += types / "Committed" -> Type(Field("commit", tx ! types / "Commit"))
      tx += types / "Valid" -> Nothing

      tx += types / "AlreadyExist" -> Type(Field("data", Anything))
      tx += types / "Conflict" -> Type(
        Field("that", Declared(types / "Message")),
        Field("other", Declared(types / "Message"))
      )
      tx += types / "NotImplemented" -> Nothing
    }

    val prepared = tx.>>
    prepared match {
      case Prepared(c) => ctx << c match {
        case Committed(cmt) =>
          cmt
        case other =>
          throw new IllegalStateException("Initial commit failed")
      }
      case other =>
        throw new IllegalStateException("Initial commit not prepared")
    }
  }
}
