package gfSkunk

import java.time.LocalDate
import skunk.Codec

import skunk._
import skunk.implicits._
import skunk.codec.all._
import java.util.UUID


class MySuite extends munit.FunSuite {

  // write a test to check if the type of derived test is a Codec[Int]  
  test("simple") {
    case class DeriveTest(s: String, i: Int)
    val derived: Codec[(String, Int)] = deriveCodec[DeriveTest]          
    val deriveTestCodec : Codec[DeriveTest] =  derived.pimap[DeriveTest]

    assertEquals(deriveTestCodec.encode(DeriveTest("hello", 1)), List(Some("hello"), Some("1")))
  }
  
  test("optional") {
    case class DeriveTest(s: Option[String], i: Option[Int])

    val derived: Codec[(Option[String], Option[Int])] = deriveCodec[DeriveTest]
    val deriveTestCodec : Codec[DeriveTest] =  derived.pimap[DeriveTest]
    val check = deriveTestCodec.encode(DeriveTest(Some("hello"), Some(1)))
    assertEquals(check, List(Some("hello"), Some("1")))
  }

    
  test("mix them up") {
    case class DeriveTest(s: String, i: Int, so: Option[Short], io: Option[Boolean])
    val instantiate = DeriveTest("hello", 1, Some(2), Some(true))

    val manual2 = List[Codec[_]](text, int4, int2.opt, bool.opt)
    val hmmm = deriveCodec.unwrap(manual2)
    println("hmmm")
    println(hmmm)
    val manual2TestCodec : Codec[DeriveTest] =  hmmm.pimap[DeriveTest]
    val check2 = manual2TestCodec.encode(instantiate)

    
    // val manual: Codec[(String, Int, Option[Short], Option[Boolean])] = text *: int4 *: int2.opt *: bool.opt
    // val manualTestCodec : Codec[DeriveTest] =  manual.pimap[DeriveTest]
    // val check = manualTestCodec.encode(instantiate)
    // assertEquals(check, List(Some("hello"), Some("1"),Some("2"), Some("t")))
    // println("Manual version succeeded")

    // assertEquals(check2, List(Some("hello"), Some("1"),Some("2"), Some("t")))
    // println("Manual version succeeded")

    // val derived: Codec[(String, Int, Option[Short], Option[Boolean])] = deriveCodec[DeriveTest]    
    // val deriveTestCodec : Codec[DeriveTest] =  derived.pimap[DeriveTest]
    // val check3 = deriveTestCodec.encode(instantiate)
    // assertEquals(check3, List(Some("hello"), Some("1"),Some("2"), Some("t")))
  }

  // test("all known types") {
  //   case class KnownTypes(
  //     sh: Short,
  //     i: Int,
  //     l: Long,
  //     s: String,            
  //     b: BigDecimal,
  //     f: Float,
  //     d: Double,
  //     b2: Boolean,
  //     // ld: LocalDate,
  //     // lt: java.time.LocalTime,
  //     // ot: java.time.OffsetTime,
  //     // ldt: java.time.LocalDateTime,
  //     // dur: java.time.Duration,
  //     // ab: Array[Byte],
  //     uuid: java.util.UUID,
  //     opt: Option[Int],
  //     opt2: Option[String],
  //     opt3: Option[BigDecimal],
  //     // opt4: Option[LocalDate],
  //     // opt5: Option[java.time.LocalTime],
  //     // opt6: Option[java.time.OffsetTime],
  //     // opt7: Option[java.time.LocalDateTime],
  //     // opt8: Option[java.time.Duration],
  //     // opt9: Option[Array[Byte]],
  //     opt10: Option[java.util.UUID],
  //     opt11: Option[Short],
  //     opt12: Option[Float],
  //     opt13: Option[Double],
  //     opt14: Option[Boolean]
  //   )
  //   val derived1: Codec[(Short, Int, Long, String, BigDecimal, Float, Double, Boolean, UUID, Option[Int], Option[String], Option[BigDecimal], Option[UUID], Option[Short], Option[Float], Option[Double], Option[Boolean])] = deriveCodec[KnownTypes]
  //   val deriveTestCodec : Codec[KnownTypes] =  derived1.pimap[KnownTypes]
  //   val instantiate = KnownTypes(1, 2, 3, "hello", BigDecimal(1.0), 1.0f, 1.0, true, java.util.UUID.randomUUID, Some(1), Some("hello"), Some(BigDecimal(1.0)), Some(java.util.UUID.randomUUID), Some(1), Some(1.0f), Some(1.0), Some(true))
  //   //val instantiate = KnownTypes(1, 2, 3, "hello", BigDecimal(1.0), 1.0f, 1.0, true, LocalDate.now, java.time.LocalTime.now, java.time.OffsetTime.now, java.time.LocalDateTime.now, java.time.Duration.ofDays(1), Array[Byte](1,2,3), java.util.UUID.randomUUID, Some(1), Some("hello"), Some(BigDecimal(1.0)), Some(LocalDate.now), Some(java.time.LocalTime.now), Some(java.time.OffsetTime.now), Some(java.time.LocalDateTime.now), Some(java.time.Duration.ofDays(1)), Some(Array[Byte](1,2,3)), Some(java.util.UUID.randomUUID), Some(1), Some(1.0f), Some(1.0), Some(true))
  //   //println(instantiate)
  //   val encoded = deriveTestCodec.encode(instantiate)
  //   //println(encoded )
  //   assertEquals(encoded, List())

  // }

  

}
