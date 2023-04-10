package lexer

import parser.parsed.Tokens
import token.{NoValueToken, Token, TokenCode}
import token.helpers.Tokenizers

import java.io.*
import java.nio.charset.StandardCharsets
import java.nio.file.Files
import java.util.stream.Collectors
import scala.io.{BufferedSource, Source}
import scala.util.{Failure, Success, Try}

case class Lexer(sourceFile: File):
  private def getToken(line: Int, tokenValue: String, nextChar: Char): Option[Token] =
    tokenValue.trim match
      case t if t.isEmpty => Option.empty
      case t if Token.isIdentifier(t) && !Token.isCharFromId(nextChar) => Tokenizers.tokenizeKeyword(line, t)
      case t if Token.isDelimiter(t) => Tokenizers.tokenizeDelimiter(line, t)
      case t if Token.isOperator(t) => Tokenizers.tokenizeOperator(line, t, nextChar)
      case t => Tokenizers.tokenizeConstant(line, t, nextChar)

  private def tokenizeLine(idx: Int, line: String): Tokens =
    // add two empty characters at the end of the string as delimiting values
    val tokenizeResult = (line :+ Char.MinValue :+ Char.MinValue)
      // list with the tokens found so far, the current token, the next character from the string (starts as white space and gets trimmed later)
      .foldLeft((List[Token](),  "", ' '))((acc, char) =>
        getToken(idx, acc._2, acc._3)
          // if a token has been found, add it to the list and set the current token as the last char
          .map(t => (acc._1 :+ t, acc._3.toString, char))
          // if a token was not returned, add the next character to the current token string and move on
          .getOrElse((acc._1, acc._2 :+ acc._3, char))
      )

    // if the current token value is not an empty string then throw
    if tokenizeResult._2.trim.nonEmpty then
      throw RuntimeException(s"Unknown token '${tokenizeResult._2}' found at line $idx.")
    else tokenizeResult._1

  def tokenizeFile: Tokens =
    val bufferedSource = Source.fromFile(sourceFile)
    val tokens = bufferedSource
      .getLines
      .map(_.trim)
      .zipWithIndex
      // get rid of empty and comment lines
      .filter((line, _) => !line.startsWith("//") && line.length != 0)
      // get rid of inline comments
      .flatMap((line, idx) => tokenizeLine(idx + 1, line.split("//").head)).toList

    bufferedSource.close()

    // add the END token
    tokens :+ NoValueToken(TokenCode.END, tokens.last.line + 1)


  def printTokens(): Unit =
    tokenizeFile.foreach(println)

  def writeTokensToFile(outputFile: File): Unit =
    Files.write(outputFile.toPath, tokenizeFile.mkString(sep="\n").getBytes(StandardCharsets.UTF_8))
