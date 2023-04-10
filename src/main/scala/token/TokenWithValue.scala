package token

import scala.reflect.ClassTag

class TokenWithValue[T: ClassTag](tokenCode: TokenCode, override val line: Int, val tokenValue: T) extends Token(tokenCode, line):
  override def toString: String =
    super.toString + s":$tokenValue"

object TokenWithValue:
  def unapply[T](arg: TokenWithValue[T]): Option[(TokenCode, Int, T)] = Option(arg.tokenCode, arg.line, arg.tokenValue)
