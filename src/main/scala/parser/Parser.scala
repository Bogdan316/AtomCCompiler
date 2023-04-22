package parser

import lexer.Lexer
import parser.ast.AstNode
import parser.ast.AstNode.AstRoot
import parser.parsed.Tokens
import parser.ast.AstNode
import parser.ast.AstRootRules.astRoot

import java.io.File
import scala.annotation.{tailrec, targetName}


case class Parser(originalTokens: Tokens):

  def parse: AstRoot =
    astRoot(originalTokens)._1.get
