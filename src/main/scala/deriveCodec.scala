package gfSkunk

import scala.deriving.Mirror
import scala.compiletime.erasedValue
import scala.compiletime.constValue
import scala.compiletime.summonInline
import scala.compiletime.{error, codeOf}

import java.time.LocalDate
import skunk.Codec
import skunk.codec.all.*
import skunk.syntax.codec.*
import scala.collection.View.Empty
import scala.deriving.Mirror

import scala.compiletime.{error, codeOf}
import java.time.LocalTime
import java.time.Duration
import scala.quoted.*
import scala.deriving.*
import cats.syntax.all.*
//import quotes.reflect.*

object deriveCodec {

  inline def caseClassTypes[T <: Product]: String = {
    inline erasedValue[T] match
      case _: EmptyTuple => ""

      case _: (t *: ts) =>
        val thisTpe = inline erasedValue[t] match
          case _: Short      => "int2"
          case _: Int        => "int4"
          case _: Long       => "int8"
          case _: BigDecimal => "numeric"
          case _: Float      => "float4"
          case _: Double     => "float8"

          case _: String => "text"

          case _: LocalDate               => "date"
          case _: LocalTime               => "time"
          case _: java.time.OffsetTime    => "timetz"
          case _: java.time.LocalDateTime => "timestamp"
          case _: Duration                => "interval"

          case _: Boolean => "bool"

          case _: Array[Byte] => "bytea"

          case _: java.util.UUID => "uuid"

          case o: Option[t2] =>
            inline erasedValue[t2] match
              case _: Short      => "int2.opt"
              case _: Int        => "int4.opt"
              case _: Long       => "int8.opt"
              case _: BigDecimal => "numeric.opt"
              case _: Float      => "float4.opt"
              case _: Double     => "float8.opt"

              case _: String => "text.opt"

              case _: LocalDate               => "date.opt"
              case _: LocalTime               => "time.opt"
              case _: java.time.OffsetTime    => "timetz.opt"
              case _: java.time.LocalDateTime => "timestamp.opt"
              case _: Duration                => "interval.opt"

              case _: Boolean => "bool.opt"

              case _: Array[Byte] => "bytea.opt"

              case _: java.util.UUID => "uuid.opt"

              case _ =>
                error("A type is not supported ")

          case x: t =>
            error("A type is not supported ")

        thisTpe + " *: " + caseClassTypes[ts]
  }

  transparent inline def apply[A <: Product](using m: Mirror.Of[A]): String = {
    type s = m.MirroredElemTypes
    val t = caseClassTypes[s]
    t.dropRight(4)
  }

}
