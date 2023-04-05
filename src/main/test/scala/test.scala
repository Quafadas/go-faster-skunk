package gfSkunk

import java.time.LocalDate
import skunk.Codec

class MySuite extends munit.FunSuite {

  type Unknown = Seq[Int]

  case class DeriveTest(anInt: Int)

  //type aTuple

  // write a test to check if the type of derived test is a Codec[Int]  
  test("one row") {       
    val derived : Tuple1[Codec[Int]] = deriveCodec[DeriveTest].asInstanceOf[Tuple1[Codec[Int]]]    
  }  
}
