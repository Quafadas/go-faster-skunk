package gfSkunk

import scala.deriving.Mirror
import scala.compiletime.erasedValue
import scala.compiletime.constValue
import scala.compiletime.summonInline
import java.time.LocalDate
import skunk.Codec
import skunk.codec.all.*
import scala.collection.View.Empty
import scala.deriving.Mirror
import scala.quoted.*
import scala.compiletime.{error, codeOf}
import java.time.LocalTime
import java.time.Duration

object deriveCodec {

  // inline def getElemLabels[A <: Tuple]: List[String] =
  //   inline erasedValue[A] match {
  //     case _: EmptyTuple => Nil // stop condition - the tuple is empty
  //     case _: (head *:
  //           tail) => // yes, in scala 3 we can match on tuples head and tail to deconstruct them step by step
  //       val headElementLabel =
  //         constValue[head].toString // bring the head label to value space
  //       val tailElementLabels =
  //         getElemLabels[tail] // recursive call to get the labels from the tail
  //       headElementLabel :: tailElementLabels // concat head + tail
  //   }
  // inline def getInstances[A <: Tuple]: List[TC[Any]] =
  // inline erasedValue[A] match {
  //   case _: EmptyTuple => Nil
  //   case _: (t *: ts) =>
  //     summonInline[TC[t]].asInstanceOf[TC[Any]] :: getInstances[ts]
  // }

  // inline def getInstances[A <: Tuple]: Tuple[Codec[A]] = {
  //   inline erasedValue[A] match
  //     case _: EmptyTuple => throw new Exception("not supported")
  //     case _: (t *: ts) =>
  //       val something = [t] => (v: t) => v match {
  //         case _ : Int    => int4
  //         case _ : String => text
  //         case _        => throw new Exception("not supported")
  //       }
  //       val tailElementLabels = getInstances[ts]
  //       something *: tailElementLabels
  // }  

  inline def caseClassTypes[T <: Product] : Tuple =
    //type s = m.MirroredElemTypes
    
    inline erasedValue[T] match
      case _: EmptyTuple => EmptyTuple

      case _: (t *: ts) =>         
        val thisTpe = inline erasedValue[t] match
          case _: Short => int2
          case _: Int => int4
          case _: Long => int8
          case _: BigDecimal => numeric
          case _: Float => float4
          case _: Double => float8

          case _: String => text                            
          
          case _: LocalDate => date
          case _: LocalTime => time
          case _: java.time.OffsetTime => timetz
          case _: java.time.LocalDateTime => timestamp          
          case _: Duration => interval
          
          case _: Boolean => bool

          case _: Array[Byte] => bytea

          case _: java.util.UUID => uuid

          case o: Option[t2] => 
            inline erasedValue[t2] match
              case _: Short => int2.opt
              case _: Int => int4.opt
              case _: Long => int8.opt
              case _: BigDecimal => numeric.opt
              case _: Float => float4.opt
              case _: Double => float8.opt

              case _: String => text.opt                            
              
              case _: LocalDate => date.opt
              case _: LocalTime => time.opt
              case _: java.time.OffsetTime => timetz.opt
              case _: java.time.LocalDateTime => timestamp.opt          
              case _: Duration => interval.opt
              
              case _: Boolean => bool.opt

              case _: Array[Byte] => bytea.opt

              case _: java.util.UUID => uuid.opt

              case _ => 
                error("A type is not supported " )                        
            
          case x: t =>             
            error("A type is not supported " )
        
        type m2 = Mirror.Of[ts]
        (thisTpe *: caseClassTypes[ts])
    
      

  transparent inline def apply[A <: Product](using m: Mirror.Of[A]) = {
    type s = m.MirroredElemTypes
    caseClassTypes[s]    
  }

}
