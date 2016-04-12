import org.scalatest.{Matchers, FlatSpec}
import org.json4s.jackson.JsonMethods._
import org.json4s._

/**
  * Created by gurghet on 12.04.16.
  */
class PreferencesConverterSpec extends FlatSpec with Matchers {
  val exampleJSON = """[
                      |      {"ofTheMonth":1,"shifts":[
                      |        {"order":1,"properties":[], "preference": "0"},
                      |        {"order":2,"properties":["altroRep"], "preference": "0"},
                      |        {"order":3,"properties":[], "preference": "0"}
                      |        ]},
                      |      {"ofTheMonth":2,"shifts":[
                      |        {"order":1,"properties":[], "preference": "0"},
                      |        {"order":2,"properties":[], "preference": "0"},
                      |        {"order":3,"properties":[], "preference": "0"}
                      |        ]}
                      |    ]"""

  val result: Map[(Int, Int), Int] = Map(
    (1,1) -> 1, (1,2) -> 5, (1,3) -> 0, (2,1) -> -3, (2,2) -> 0, (2,3) -> 0
  )

  it should "do" in {
    val o = parse(exampleJSON)
    val h = for {
      JArray(l) <- o
      JObject(w) <- l
      JField("shifts", JArray(s)) <- w
      JObject(i) <- s
      JField("order", JInt(r)) <- i
    } yield r // List(1,2,3,1,2,3)

  }
}
