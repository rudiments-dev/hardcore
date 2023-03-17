package dev.rudiments.git

import dev.rudiments.utils.{Hashed, SHA1}

enum WriteStatus:
  case Success
  case Failure

enum GitObject(content: String, kind: String):
  val header: String = kind + " " + content.getBytes(Hashed.utf8).length
  val fullContent: String = header + "\u0000" + content
  val hash: SHA1 = SHA1(fullContent)

  case Blob(content: String) extends GitObject(content, "blob")

  //TODO fix content for tree and commit
  case Tree(content: String) extends GitObject(content, "tree")
  case Commit(content: String) extends GitObject(content, "commit")
