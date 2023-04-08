package parser.helpers

import parser.parsed.{IsParsed, NotParsed, Parsed}
import token.{Token, TokenCode}

import scala.annotation.tailrec
import scala.util.{Success, Try}

type Tokens = List[Token]
type ParsedTokens = Parsed[Tokens]
type Rule = Tokens => ParsedTokens

object Rules {

  def eps: Rule =
  // any tokens provided are considered parsed
    IsParsed(_)

  def term(tokenCode: TokenCode, errContext: String = "", location: String = "after", skip: TokenCode = null): Rule =
  // returns a rule that consumes a token that has the provided tokenCode
  // if the token's tokenCode is the same as skip then the token is not consumed
  // in any other case throws a RuntimeException that can be customized using the errContext and location parameters
    tokens => {
      tokens match
        case Token(code, _) :: _ if code == skip => IsParsed(tokens)
        case Token(code, _) :: tail if code == tokenCode => IsParsed(tail)
        case t =>
          if errContext.isEmpty then NotParsed(tokens)
          else throw RuntimeException(s"Expected '${Token.stringify(tokenCode).trim}' but found " +
            s"'${Token.stringify(t.head).trim}' $location $errContext at line ${tokens.head.line}.")
    }

  private def or(firsRule: Rule, secondRule: Rule = NotParsed(_))(tokens: Tokens): ParsedTokens =
    firsRule(tokens) match
      // rule | rule
      case remainingTokens@IsParsed(_) => remainingTokens
      case NotParsed(_) => secondRule(tokens)

  def or(rules: Rule*)(tokens: Tokens): ParsedTokens =
  // rule | rule | rule | ...
    rules.reduce(or)(tokens)

  private def and(firsRule: Rule, secondRule: Rule)(tokens: Tokens): ParsedTokens =
    firsRule(tokens) match
      // rule rule
      case IsParsed(remainingTokens) => secondRule(remainingTokens)
      case NotParsed(_) => NotParsed(tokens)

  def and(rules: Rule*)(tokens: Tokens): ParsedTokens =
  // rule rule rule ...
    rules.reduce(and)(tokens)

  def optional(rule: Rule)(tokens: Tokens): ParsedTokens =
  // rule?
    or(rule, eps)(tokens)

  @tailrec
  def star(rule: Rule)(tokens: Tokens): ParsedTokens =
  // rule*
    rule(tokens) match
      case IsParsed(remainingTokens) => star(rule)(remainingTokens)
      case NotParsed(_) => IsParsed(tokens)

  def optionalCommaSeq(rule: Rule, errContext: String = "")(tokens: Tokens): ParsedTokens =
  // (rule (COMMA rule)*)?
    optional(and(rule, star(and(term(TokenCode.COMMA, errContext, "inside", TokenCode.RPAR), rule))))(tokens)
}
