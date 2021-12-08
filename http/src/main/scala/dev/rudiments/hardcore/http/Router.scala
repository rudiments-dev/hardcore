package dev.rudiments.hardcore.http

import akka.http.scaladsl.server.Route

trait Router {
  val routes: Route
}