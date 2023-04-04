package gfSkunk

import java.time.LocalDate

class MySuite extends munit.FunSuite {

  case class DeriveTest(anInt: Int, aString: String)

  //type aTuple

  test("one row") {       
    println(deriveCodec[DeriveTest])
  }  
}
