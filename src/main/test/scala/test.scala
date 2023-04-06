package gfSkunk

import java.time.LocalDate
import skunk.Codec

import skunk._
import skunk.implicits._
import skunk.codec.all._
import java.util.UUID
import scala.quoted.*
import scala.deriving.*
import scala.compiletime.erasedValue
import scala.compiletime.constValue
import scala.compiletime.summonInline
import scala.compiletime.{error, codeOf}
import cats.syntax.all.*

class MySuite extends munit.FunSuite {

  test("small number of known types") {
    case class DeriveTest(
        s: String,
        i: Int,
        so: Option[Short],
        io: Option[Boolean]
    )
    assertEquals(
      deriveCodec[DeriveTest],
      "text *: int4 *: int2.opt *: bool.opt"
    )

    val c = (text *: int4 *: int2.opt *: bool.opt).pimap[DeriveTest]

    assertEquals(
      c.encode(DeriveTest("a", 1, Some(2), Some(true))),
      List(Some("a"), Some("1"), Some("2"), Some("t"))
    )

  }

  test("int only2") {
    val s = deriveCodec[(Int, Int, Int)]
    assertEquals(s, "int4 *: int4 *: int4")
  }

  test("all known types") {
    case class KnownTypes(
        sh: Short,
        i: Int,
        l: Long,
        s: String,
        b: BigDecimal,
        f: Float,
        d: Double,
        b2: Boolean,
        ld: LocalDate,
        lt: java.time.LocalTime,
        ot: java.time.OffsetTime,
        ldt: java.time.LocalDateTime,
        dur: java.time.Duration,
        ab: Array[Byte],
        uuid: java.util.UUID,
        opt: Option[Int],
        opt2: Option[String],
        opt3: Option[BigDecimal],
        opt4: Option[LocalDate],
        opt5: Option[java.time.LocalTime],
        opt6: Option[java.time.OffsetTime],
        opt7: Option[java.time.LocalDateTime],
        opt8: Option[java.time.Duration],
        opt9: Option[Array[Byte]],
        opt10: Option[java.util.UUID],
        opt11: Option[Short],
        opt12: Option[Float],
        opt13: Option[Double],
        opt14: Option[Boolean]
    )
    val s = deriveCodec[KnownTypes]
    assertEquals(
      s,
      "int2 *: int4 *: int8 *: text *: numeric *: float4 *: float8 *: bool *: date *: time *: timetz *: timestamp *: interval *: bytea *: uuid *: int4.opt *: text.opt *: numeric.opt *: date.opt *: time.opt *: timetz.opt *: timestamp.opt *: interval.opt *: bytea.opt *: uuid.opt *: int2.opt *: float4.opt *: float8.opt *: bool.opt"
    )
  }

}
