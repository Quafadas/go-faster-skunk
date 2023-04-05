package gfSkunk

import java.time.LocalDate
import skunk.Codec

class MySuite extends munit.FunSuite {

  type Unknown = Seq[Int]

  case class DeriveTest(anInt: Int)

  //type aTuple

  test("one row") {       
    val derived = deriveCodec[DeriveTest]
    println(derived) 

    assert( derived.isInstanceOf[Product1[Codec[String]]] )
  }  
}
