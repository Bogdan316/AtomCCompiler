package parser

import lexer.Lexer
import parser.helpers.Rules.*
import parser.helpers.{ParsedTokens, Tokens}
import token.TokenCode.*

import java.io.File
import scala.annotation.{tailrec, targetName}
import scala.util.{Failure, Success, Try}

case class Parser(originalTokens: Tokens):
  private def unit(tokens: Tokens): ParsedTokens =
    and(
      star(or(structDef, fnDef, varDef)),
      term(END, "file", "at the end of")
    )(tokens)

  private def structDef(tokens: Tokens): ParsedTokens =
    and(
      term(STRUCT),
      term(ID, "'struct' keyword"),
      term(LACC), star(varDef),
      term(RACC, "struct definition"),
      term(SEMICOLON, "struct definition")
    )(tokens)

  private def varDef(tokens: Tokens): ParsedTokens =
    and(
      typeBase,
      term(ID),
      optional(arrayDecl),
      term(SEMICOLON, "variable definition")
    )(tokens)

  private def typeBase(tokens: Tokens): ParsedTokens =
    or(
      term(TYPE_INT), term(TYPE_DOUBLE), term(TYPE_CHAR),
      and(term(STRUCT), term(ID, "'struct' keyword"))
    )(tokens)

  private def arrayDecl(tokens: Tokens): ParsedTokens =
    and(
      term(LBRACKET),
      optional(term(INT, "'[]'", "between", RBRACKET)),
      term(RBRACKET, "array declaration")
    )(tokens)

  private def fnDef(tokens: Tokens): ParsedTokens =
  // (typeBase | VOID), parse the return type
    and(
      or(typeBase, term(VOID)),
      term(ID), term(LPAR), optionalCommaSeq(fnParam, "parameter list"),
      term(RPAR, "parameter list"),
      stmCompound
    )(tokens)

  private def fnParam(tokens: Tokens): ParsedTokens =
    and(typeBase, term(ID, "parameter list", "in"), optional(arrayDecl))(tokens)

  def stm(tokens: Tokens): ParsedTokens =
    or(
      stmCompound,
      and(
        term(IF),
        term(LPAR, "if keyword"),
        expr,
        term(RPAR, "if keyword"),
        stm, optional(and(term(ELSE), stm))
      ),
      and(
        term(WHILE),
        term(LPAR, "while keyword"),
        expr,
        term(RPAR, "while keyword"),
        stm
      ),
      and(term(RETURN), optional(expr), term(SEMICOLON, "return statement", "at the end of")),
      // an alternative rule to expr? SEMICOLON
      or(term(SEMICOLON), and(expr, term(SEMICOLON, "expression")))
    )(tokens)

  private def stmCompound(tokens: Tokens): ParsedTokens =
    and(
      term(LACC),
      star(or(varDef, stm)),
      term(RACC, "code block", "at the end of")
    )(tokens)

  private def expr(tokens: Tokens): ParsedTokens =
    exprAssign(tokens)

  private def exprAssign(tokens: Tokens): ParsedTokens =
    or(
      and(exprUnary, term(ASSIGN), exprAssign),
      exprOr
    )(tokens)

  private def exprOr(tokens: Tokens): ParsedTokens =
    def exprOrPrime(tokens: Tokens): ParsedTokens =
      or(
        and(term(OR), exprAnd, exprOrPrime),
        eps
      )(tokens)

    and(exprAnd, exprOrPrime)(tokens)

  private def exprAnd(tokens: Tokens): ParsedTokens =
    def exprAndPrime(tokens: Tokens): ParsedTokens =
      or(
        and(term(AND), exprEq, exprAndPrime),
        eps
      )(tokens)

    and(exprEq, exprAndPrime)(tokens)

  private def exprEq(tokens: Tokens): ParsedTokens =
    def exprEqPrime(tokens: Tokens): ParsedTokens =
      or(
        and(
          or(term(EQUAL), term(NOTEQ)),
          exprRel, exprEqPrime
        ),
        eps
      )(tokens)

    and(exprRel, exprEqPrime)(tokens)

  private def exprRel(tokens: Tokens): ParsedTokens =
    def exprRelPrime(tokens: Tokens): ParsedTokens =
      or(
        and(
          or(term(LESS), term(LESSEQ), term(GREATER), term(GREATEREQ)),
          exprAdd, exprRelPrime
        ),
        eps
      )(tokens)

    and(exprAdd, exprRelPrime)(tokens)

  private def exprAdd(tokens: Tokens): ParsedTokens =
    def exprAddPrime(tokens: Tokens): ParsedTokens =
      or(
        and(
          or(term(ADD), term(SUB)),
          exprMul, exprAddPrime
        ),
        eps
      )(tokens)

    and(exprMul, exprAddPrime)(tokens)

  private def exprMul(tokens: Tokens): ParsedTokens = and(exprCast, exprMulPrime)(tokens)

  private def exprMulPrime(tokens: Tokens): ParsedTokens =
    or(
      and(
        or(term(MUL), term(DIV)),
        exprCast, exprMulPrime
      ),
      eps
    )(tokens)

  private def exprCast(tokens: Tokens): ParsedTokens =
    or(
      and(
        term(LPAR), typeBase, optional(arrayDecl),
        term(RPAR, "matching '(' in cast expression"), exprCast
      ),
      exprUnary
    )(tokens)

  private def exprUnary(tokens: Tokens): ParsedTokens =
    or(
      and(
        or(term(SUB), term(NOT)),
        exprUnary
      ),
      exprPostfix
    )(tokens)

  private def exprPostfix(tokens: Tokens): ParsedTokens =
    def exprPostfixPrime(tokens: Tokens): ParsedTokens =
      or(
        and(term(LBRACKET), expr, term(RBRACKET, "expression between '[]'"), exprPostfixPrime),
        and(term(DOT), term(ID, "dot operator"), exprPostfixPrime),
        eps
      )(tokens)

    and(exprPrimary, exprPostfixPrime)(tokens)

  private def exprPrimary(tokens: Tokens): ParsedTokens =
    or(
      and(
        term(ID),
        optional(
          and(
            term(LPAR),
            optionalCommaSeq(expr, "expression list"),
            term(RPAR, "matching '('")
          )
        )
      ),
      term(INT), term(DOUBLE), term(CHAR), term(STRING),
      and(
        term(LPAR), expr,
        term(RPAR, "matching '('")
      )
    )(tokens)

  def parse: Tokens =
    println(originalTokens)
    expr(originalTokens).get

object Parser extends App :
  val parser = Parser(Lexer(new File("testlex.c")).tokenizeFile)
  println(parser.parse)
