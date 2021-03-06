package model

import play.api.libs.json.{Json, Reads}

case class CheckQuery(text: String)

object CheckQuery {
  implicit val reads: Reads[CheckQuery] = Json.reads[CheckQuery]
}