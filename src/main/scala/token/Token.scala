package token

import parser.parsed.Tokens

import scala.annotation.tailrec

import scala.reflect.ClassTag

sealed trait Token:

  def line: Int
  override def toString: String =
    s"$line\t${this.getClass.getSimpleName}"


object Token:

  sealed trait TokenWithValue[T: ClassTag] extends Token:

    def tokenValue: T

    override def toString: String =
      super.toString + s":$tokenValue"

  final case class IdentifierToken(override val line: Int, override val tokenValue: String) extends TokenWithValue[String]

  sealed trait LiteralToken[T] extends TokenWithValue[T]:
    def tokenValue: T
    
  object LiteralToken:
    final case class IntLiteralToken(override val line: Int, override val tokenValue: Int) extends LiteralToken[Int]
    
    final case class DoubleLiteralToken(override val line: Int, override val tokenValue: Double) extends LiteralToken[Double]
    
    final case class CharLiteralToken(override val line: Int, override val tokenValue: String) extends LiteralToken[String]
    
    final case class StringLiteralToken(override val line: Int, override val tokenValue: String) extends LiteralToken[String]


  sealed trait TypeToken extends Token
  object TypeToken:
    final case class CharTypeToken(override val line: Int) extends TypeToken
    
    final case class IntTypeToken(override val line: Int) extends TypeToken
    
    final case class DoubleTypeToken(override val line: Int) extends TypeToken
    
    final case class StructTypeToken(override val line: Int) extends TypeToken
    
    final case class VoidTypeToken(override val line: Int) extends TypeToken

  sealed trait KeywordToken extends Token
  object KeywordToken:
    final case class IfToken(override val line: Int) extends KeywordToken
    
    final case class ElseToken(override val line: Int) extends KeywordToken
    
    final case class WhileToken(override val line: Int) extends KeywordToken
    
    final case class ReturnToken(override val line: Int) extends KeywordToken


  sealed trait DelimiterToken extends Token
  object DelimiterToken:
    final case class CommaToken(override val line: Int) extends DelimiterToken
    
    final case class SemicolonToken(override val line: Int) extends DelimiterToken
    
    final case class LparToken(override val line: Int) extends DelimiterToken
    
    final case class RparToken(override val line: Int) extends DelimiterToken
    
    final case class LbracketToken(override val line: Int) extends DelimiterToken
    
    final case class RbracketToken(override val line: Int) extends DelimiterToken
    
    final case class LaccToken(override val line: Int) extends DelimiterToken
    
    final case class RaccToken(override val line: Int) extends DelimiterToken
    
    final case class EndToken(override val line: Int) extends DelimiterToken


  sealed trait OperatorToken extends Token
  object OperatorToken:
    sealed trait ArithmeticLogicOperatorToken extends OperatorToken
    
    sealed trait ArithmeticOperatorToken extends ArithmeticLogicOperatorToken
    
    final case class AddToken(override val line: Int) extends ArithmeticOperatorToken
    
    final case class SubToken(override val line: Int) extends ArithmeticOperatorToken
    
    final case class MulToken(override val line: Int) extends ArithmeticOperatorToken
    
    final case class DivToken(override val line: Int) extends ArithmeticOperatorToken
    
    
    
    final case class DotToken(override val line: Int) extends OperatorToken
    
    final case class NotToken(override val line: Int) extends OperatorToken
    
    final case class AssignToken(override val line: Int) extends OperatorToken
    

    sealed trait LogicalOperatorToken extends ArithmeticLogicOperatorToken
    
    final case class AndToken(override val line: Int) extends LogicalOperatorToken
    
    final case class OrToken(override val line: Int) extends LogicalOperatorToken
    
    final case class EqualToken(override val line: Int) extends LogicalOperatorToken
    
    final case class NoteqToken(override val line: Int) extends LogicalOperatorToken
    
    final case class LessToken(override val line: Int) extends LogicalOperatorToken
    
    final case class LesseqToken(override val line: Int) extends LogicalOperatorToken
    
    final case class GreaterToken(override val line: Int) extends LogicalOperatorToken
    
    final case class GreatereqToken(override val line: Int) extends LogicalOperatorToken
