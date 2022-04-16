package runtime

import parser._

abstract class MemoryValue {
    def toFloat: Number
    def toBool: Bool
    def toFunction: Function
}

case class Number(value: Float) extends MemoryValue {

    def toFloat = this

    def toBool = value match {
        case 1 => Bool(true)
        case 0 => Bool(false)
        case _ => throw new Exception("Cannot cast " + value + " to bool")
    }

    def toFunction = Function("_", Literal(value), Map.empty)
}

case class Bool(value: Boolean) extends MemoryValue {

    def toFloat = value match {
        case true => Number(1)
        case _ => Number(0)
    }

    def toBool = this

    def toFunction = Function("_", Literal(value), Map.empty)
}

case class Function(paramater: String, body: Expression, closure: MemoryFrame) extends MemoryValue {

    def toFloat = throw new Exception("Cannot cast function to number")

    def toBool = throw new Exception("Cannot cast function to bool")

    def toFunction = this
}

case class Null() extends MemoryValue {

    def toFloat = throw new Exception("Cannot cast null to number")

    def toBool = throw new Exception("Cannot cast null to bool")

    def toFunction = throw new Exception("Cannot cast null to function")
}

object MemoryValue {
    def runtimeTrue = Bool(true)
    def runtimeFalse = Bool(false)
    def runtimeNull = Null()
}