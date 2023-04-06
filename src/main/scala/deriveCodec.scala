package gfSkunk

import scala.deriving.Mirror
import scala.compiletime.erasedValue
import scala.compiletime.constValue
import scala.compiletime.summonInline
import java.time.LocalDate
import skunk.Codec
import skunk.codec.all.*
import skunk.syntax.codec.*
import scala.collection.View.Empty
import scala.deriving.Mirror
import scala.quoted.*
import scala.compiletime.{error, codeOf}
import java.time.LocalTime
import java.time.Duration
import scala.quoted.*
import scala.deriving.*
//import quotes.reflect.*

object deriveCodec {

  inline def deriveCodecType[T <: Product]: Tuple = {

    inline erasedValue[T] match
      case _: EmptyTuple => EmptyTuple
      case _: (t *: ts) =>
        val codecType = inline erasedValue[t] match
          case _: Int        => Codec[Int]
          case _: Long       => Codec[Long]
          case _: BigDecimal => Codec[BigDecimal]
          case _: Float      => Codec[Float]
          case _: Double     => Codec[Double]
          case _: String     => Codec[String]
          case _             => error("A type is not supported ")

        (codecType *: deriveCodecType[ts])
  }

  inline def caseClassTypes[T <: Product]: Tuple = {
    inline erasedValue[T] match
      case _: EmptyTuple => EmptyTuple

      case _: (t *: ts) =>
        val thisTpe = inline erasedValue[t] match
          case _: Short      => int2
          case _: Int        => int4
          case _: Long       => int8
          case _: BigDecimal => numeric
          case _: Float      => float4
          case _: Double     => float8

          case _: String => text

          case _: LocalDate               => date
          case _: LocalTime               => time
          case _: java.time.OffsetTime    => timetz
          case _: java.time.LocalDateTime => timestamp
          case _: Duration                => interval

          case _: Boolean => bool

          case _: Array[Byte] => bytea

          case _: java.util.UUID => uuid

          case o: Option[t2] =>
            inline erasedValue[t2] match
              case _: Short      => int2.opt
              case _: Int        => int4.opt
              case _: Long       => int8.opt
              case _: BigDecimal => numeric.opt
              case _: Float      => float4.opt
              case _: Double     => float8.opt

              case _: String => text.opt

              case _: LocalDate               => date.opt
              case _: LocalTime               => time.opt
              case _: java.time.OffsetTime    => timetz.opt
              case _: java.time.LocalDateTime => timestamp.opt
              case _: Duration                => interval.opt

              case _: Boolean => bool.opt

              case _: Array[Byte] => bytea.opt

              case _: java.util.UUID => uuid.opt

              case _ =>
                error("A type is not supported ")

          case x: t =>
            error("A type is not supported ")

        type m2 = Mirror.Of[ts]
        (thisTpe *: caseClassTypes[ts])
  }

  def unwrap(l: List[Codec[_]]): Codec[_]= 
    l match
      case x :: Nil => x      
      case x :: xs => 
        xs.foldLeft(x)((a, b) => a *: b)

    // l match {      
    //   case x :: Nil => x
    //   case (x: Codec[_]) :: xs => 
    //     val h = unwrap(xs)
    //     x *: h.head // Somehow, this line appears to be different
      //case Nil => error("This really should not happen")
    
  

  transparent inline def apply[A <: Product](using m: Mirror.Of[A]) = {
    type s = m.MirroredElemTypes              
    //type t1 = Tuple.Map[s, Codec]
    type t2 = Codec[s]    
    val t = caseClassTypes[s]
    
    println("--------------------")
    //val asList = t.toList.asInstanceOf[List[Codec[_]]]
    
    val unwrapped = t.toList.asInstanceOf[List[Codec[_]]]
    println(unwrapped)
    val codec = unwrap(unwrapped)
    println(codec)
    println("--------------------")
    codec.asInstanceOf[t2]    
  }

}
