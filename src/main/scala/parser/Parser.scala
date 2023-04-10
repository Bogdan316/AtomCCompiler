package parser

import lexer.Lexer
import parser.ast.AstNode
import parser.ast.AstRoot.AstRoot
import parser.ast.AstRoot.astRoot
import parser.parsed.Tokens
import parser.ast.AstNode

import java.io.File
import scala.annotation.{tailrec, targetName}


case class Parser(originalTokens: Tokens):

  def parse: AstRoot =
    astRoot(originalTokens)._1.get
