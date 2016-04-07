import org.scalatest.{FlatSpec, Matchers}

/**
  * Created by Andrea on 06/04/2016.
  */
class RotaSpec extends FlatSpec with Matchers {
  "Rota" should "non contain duplicate values" in {
    val rota = new Rota(1, Set(WorkerId(1), WorkerId(2)))
    rota.init()
    rota.get() should contain noneOf (Set(WorkerId(1), WorkerId(1)), Set(WorkerId(2), WorkerId(2)))
  }

  it should "correctly select a function" in {
    val rota = new Rota(1, Set(WorkerId(1), WorkerId(2)))
    rota.init()
    rota.pickNeighborhood() shouldBe a [() => _]
  }

  it should "swap two workers" in {
    val rota = new Rota(1, Set(WorkerId(1), WorkerId(2)))
    rota.init()
    val rotaInit = rota.get
    rota.swap()
    rota.get should not equal rotaInit
  }
}