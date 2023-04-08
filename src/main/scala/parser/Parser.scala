package parser

import lexer.Lexer
import parser.ast.AstNode
import parser.ast.AstRoot.astRoot
import parser.parsed.Tokens
import parser.utils.PrettyPrint

import java.io.File
import scala.annotation.{tailrec, targetName}


case class Parser(originalTokens: Tokens):

  def parse: AstNode =
    astRoot(originalTokens)._1.get

case object Parser extends App :
  val parsedAst = Lexer(new File("testlex.c")).tokenizeFile
  PrettyPrint.pprint(Parser(parsedAst).parse)
