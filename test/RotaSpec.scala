import org.scalatest.{FlatSpec, Matchers}

/**
  * Created by Andrea on 06/04/2016.
  */
class RotaSpec extends FlatSpec with Matchers {
  def smallExampleRota: Rota = new Rota(1, Set(WorkerId(1), WorkerId(2)))
  def smallExampleRotaManyWorkers: Rota = new Rota(1, Set(WorkerId(1), WorkerId(2), WorkerId(3), WorkerId(4), WorkerId(5)))
  def bigExampleRota: Rota =
    new Rota(30, Set(WorkerId(1), WorkerId(2), WorkerId(3), WorkerId(4), WorkerId(5), WorkerId(6)))

  "Rota" should "non contain duplicate values" in {
    val rota = smallExampleRota
    rota.init()
    rota.get() should contain noneOf (Set(WorkerId(1), WorkerId(1)), Set(WorkerId(2), WorkerId(2)))
  }

  it should "swap two workers" in {
    val rota = bigExampleRota
    rota.init()
    val rotaInit = rota.get
    rota.movingRandomWorkerSomewhereElse()
    rota.get should not equal rotaInit
  }

  it should "unswap two workers" in {
    val rota = bigExampleRota
    rota.init()
    val rotaInit = rota.get
    rota.movingRandomWorkerSomewhereElse()
    rota.unMovingRandomWorkerSomewhereElse()
    rota.get shouldEqual rotaInit
  }

  it should "return a solution" in {
    val rota = smallExampleRota
    rota.init()
    val rotaInit = rota.get
    val rotaFinal = rota.go()
    rotaFinal should not equal rotaInit
  }

  it should "correctly classify two patterns of presence" in {
    val ex = smallExampleRota
    val good = ex.simplePresencesToLoss(List(0,0,0))
    val equallyGood1 = ex.simplePresencesToLoss(List(0,0,0,1,0,0,0))
    val equallyGood2 = ex.simplePresencesToLoss(List(0,1,0,0,1))
    val bad = ex.simplePresencesToLoss(List(1,0,1,0,0,0,0))
    val worse = ex.simplePresencesToLoss(List(0,0,1,1,0,0,0))
    val superWorse = ex.simplePresencesToLoss(List(1,1,1))
    good shouldEqual equallyGood1
    equallyGood2 shouldEqual equallyGood1
    bad shouldBe > (worse)
    worse shouldBe > (superWorse)
  }

  it should "correctly classify two patterns of preferences" in {
    val good = smallExampleRota
    val bad = smallExampleRota
    good.setPreferencesFor(WorkerId(1), Map((1, 1) -> -4, (1, 2) -> 0, (1, 3) -> 2))
    good.setPreferencesFor(WorkerId(2), Map((1, 1) -> 4, (1, 2) -> -2, (1, 3) -> 0))
    good.init(Option(Map((1, 1) -> Set(WorkerId(2)), (1, 2) -> Set(WorkerId(1)), (1, 3) -> Set(WorkerId(1)))))
    bad.setPreferencesFor(WorkerId(1), Map((1, 1) -> -4, (1, 2) -> 0, (1, 3) -> 2))
    bad.setPreferencesFor(WorkerId(2), Map((1, 1) -> 4, (1, 2) -> -2, (1, 3) -> 0))
    bad.init(Option(Map((1, 1) -> Set(WorkerId(1)), (1, 2) -> Set(WorkerId(1)), (1, 3) -> Set(WorkerId(2)))))
    good.gain shouldBe > (bad.gain)
  }

  it should "size the shifts appropriately" in {
    val r = smallExampleRotaManyWorkers
    val shiftProps =
      Map((1, 1) -> Set("mattino"), (1, 2) -> Set("pomeriggio"), (1, 3) -> Set("altroRep"))
    r.setShiftProperties(shiftProps)
    r.init()
    println(s"Init     ${r.get}")
    println(s"Init     $shiftProps")
    val solution = r.go(200)
    println(s"Solution $solution")
    solution((1, 1)).size shouldBe 4
    solution((1, 2)).size shouldBe 1
    solution((1, 3)).size shouldBe 0
  }

  it should "work" in {
    val example = smallExampleRota
    example.init()
    println(s"Init     ${example.get}")
    val solution = example.go()
    println(s"Solution $solution")
  }
}