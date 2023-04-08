//import token.Token.stringify
//import token.TokenCode.*
//
//import java.io.File
//import scala.annotation.{tailrec, targetName}
//import scala.util.{Failure, Success, Try}
//
//case class Backup(originalTokens: List[token.Token]):
//  private def unit(tokens: List[token.Token]): Parser.Parsed[List[token.Token]] =
//  // (structDef | fnDef | varDef)* END
//    defs_*(tokens).get match
//      case token.Token(END, _) :: Nil => Parser.IsParsed(Nil)
//      case _ => Parser.NotParsed(tokens)
//
//  private def structDef(tokens: List[token.Token]): Parser.Parsed[List[token.Token]] =
//    tokens match
//      case token.Token(STRUCT, _) :: token.Token(ID, _) :: token.Token(LACC, _) :: tail =>
//        varDef_*(tail).get match
//          case token.Token(RACC, _) :: token.Token(SEMICOLON, _) :: tail => Parser.IsParsed(tail)
//          case token.Token(RACC, line) :: t :: _ =>
//            throw RuntimeException(s"Expected ';' after struct definition but found '${stringify(t)}' at line ${t.line}.")
//          case t :: _ =>
//            throw RuntimeException(s"Expected '}' after struct definition but found '${stringify(t)}' at line ${t.line}.")
//          case _ => Parser.NotParsed(tokens)
//      case _ => Parser.NotParsed(tokens)
//
//  private def varDef(tokens: List[token.Token]): Parser.Parsed[List[token.Token]] =
//  // typeBase ID arrayDecl? SEMICOLON
//    typeBase(tokens) match
//      case Parser.IsParsed(remainingTokens) =>
//        remainingTokens match
//          case token.Token(ID, line) :: tail =>
//            arrayDecl_?(tail).get match
//              case token.Token(SEMICOLON, _) :: tail => Parser.IsParsed(tail)
//              case _ =>
//                throw RuntimeException(s"Missing ';' after '${stringify(tokens.take(tokens.length - tail.length))}' " +
//                  s"at line $line.")
//          case _ => Parser.NotParsed(tokens)
//      case _ => Parser.NotParsed(tokens)
//
//  private def typeBase(tokens: List[token.Token]): Parser.Parsed[List[token.Token]] =
//    tokens match
//      // STRUCT ID
//      case token.Token(STRUCT, _) :: token.Token(ID, _) :: tail => Parser.IsParsed(tail)
//      case token.Token(STRUCT, line) :: t :: _ =>
//        throw RuntimeException(s"Expected identifier after 'struct' but found '${stringify(t)}' at line $line.")
//      // TYPE_INT | TYPE_DOUBLE | TYPE_CHAR
//      case token.Token(typeToken, _) :: tail =>
//        typeToken match
//          case TYPE_INT | TYPE_DOUBLE | TYPE_CHAR => Parser.IsParsed(tail)
//          case _ => Parser.NotParsed(tokens)
//      case _ => Parser.NotParsed(tokens)
//
//  private def arrayDecl(tokens: List[token.Token]): Parser.Parsed[List[token.Token]] =
//    tokens match
//      // LBRACKET INT RBRACKET
//      case token.Token(LBRACKET, _) :: token.Token(INT, _) :: token.Token(RBRACKET, _) :: tail => Parser.IsParsed(tail)
//      case token.Token(LBRACKET, _) :: t :: token.Token(RBRACKET, _) :: _ =>
//        throw RuntimeException(s"Expected an integer between '[]' found '${stringify(t)}'.")
//      case token.Token(LBRACKET, line) :: token.Token(INT, _) :: t :: _ =>
//        throw RuntimeException(s"Expected ']' but found '${stringify(t)}' at line $line.")
//      // LBRACKET RBRACKET
//      case token.Token(LBRACKET, _) :: token.Token(RBRACKET, _) :: tail =>
//        Parser.IsParsed(tail)
//      case token.Token(LBRACKET, line) :: t :: _ =>
//        throw RuntimeException(s"Expected ']' but found ${stringify(t)} at line $line.")
//      case _ => Parser.NotParsed(tokens)
//
//  private def fnDef(tokens: List[token.Token]): Parser.Parsed[List[token.Token]] =
//    // (typeBase | VOID), parse the return type
//    val functionDefinition = typeBase(tokens) match
//      case Parser.IsParsed(remainingTokens) => Parser.IsParsed(remainingTokens)
//      case _ => tokens match
//        case token.Token(VOID, _) :: tail => Parser.IsParsed(tail)
//        case _ => Parser.NotParsed(tokens)
//
//    // ID LPAR (fnParam (COMMA fnParam)*)? RPAR stmCompound
//    functionDefinition match
//      case Parser.IsParsed(remainingTokens) => remainingTokens match
//        case token.Token(ID, _) :: token.Token(LPAR, _) :: tail =>
//          fnParams_?(tail).get match
//            case token.Token(RPAR, _) :: tail => stmCompound(tail)
//            case t@token.Token(_, line) :: _ =>
//              throw RuntimeException(s"Expected ')' but found '${stringify(t.head)}' at line $line.")
//            case _ => Parser.NotParsed(tokens)
//        case _ => Parser.NotParsed(tokens)
//      case _ => Parser.NotParsed(tokens)
//
//  private def fnParam(tokens: List[token.Token]): Parser.Parsed[List[token.Token]] =
//    typeBase(tokens) match
//      case Parser.IsParsed(remainingTokens) => remainingTokens match
//        case token.Token(ID, _) :: tail => arrayDecl_?(tail)
//        case _ => Parser.NotParsed(tokens)
//      case _ => Parser.NotParsed(tokens)
//
//  def stm(tokens: List[token.Token]): Parser.Parsed[List[token.Token]] =
//    tokens match
//      // IF LPAR expr RPAR stm (ELSE stm)?
//      case token.Token(IF, _) :: token.Token(LPAR, _) :: tail =>
//        expr(tail) match
//          case Parser.IsParsed(token.Token(RPAR, _) :: tail) =>
//            stm(tail) match
//              case Parser.IsParsed(remainingTokens) =>
//                remainingTokens match
//                  case token.Token(ELSE, _) :: tail => stm(tail)
//                  case _ => Parser.IsParsed(remainingTokens)
//              case _ => Parser.NotParsed(tokens)
//          case Parser.IsParsed(t :: _) => throw RuntimeException(s"Expected ')' found '${stringify(t)}' at line ${t.line}.")
//          case _ => Parser.NotParsed(tokens)
//      case token.Token(IF, _) :: t :: _ =>
//        throw RuntimeException(s"Expected '(' after 'if' found '${stringify(t)}' at line ${t.line}.")
//
//      // WHILE LPAR expr RPAR stm
//      case token.Token(WHILE, _) :: token.Token(LPAR, _) :: tail =>
//        expr(tail) match
//          case Parser.IsParsed(remainingTokens) =>
//            remainingTokens match
//              case token.Token(RPAR, _) :: tail => stm(tail)
//              case t :: _ => throw RuntimeException(s"Expected ')' found '${stringify(t)}' at line ${t.line}.")
//              case _ => Parser.NotParsed(tokens)
//          case _ => Parser.NotParsed(tokens)
//      case token.Token(WHILE, _) :: t :: _ =>
//        throw RuntimeException(s"Expected '(' after 'while' found '${stringify(t)}' at line ${t.line}.")
//
//      // RETURN expr? SEMICOLON
//      case token.Token(RETURN, _) :: tail =>
//        expr_?(tail).get match
//          case token.Token(SEMICOLON, _) :: tail => Parser.IsParsed(tail)
//          case remainingTokens@t :: _ =>
//            throw RuntimeException(s"Expected ';' after 'return ${stringify(tail.take(tail.length -
//              remainingTokens.length))}' found '${stringify(t)}' at line ${t.line}.")
//          case _ => Parser.NotParsed(tokens)
//
//      case t@token.Token(LACC, _) :: _ => stmCompound(t)
//
//      // expr? SEMICOLON
//      case t => expr_?(t).get match
//        case token.Token(SEMICOLON, _) :: tail => Parser.IsParsed(tail)
//        case _ => Parser.NotParsed(tokens)
//
//  private def stmCompound(tokens: List[token.Token]): Parser.Parsed[List[token.Token]] =
//  // LACC (varDef | stm)* RACC
//    tokens match
//      case token.Token(LACC, _) :: tail =>
//        // the return type will always be Parser.IsParsed (optional repetition)
//        varDefOrStm_*(tail).get match
//          case token.Token(RACC, _) :: tail => Parser.IsParsed(tail)
//          case t :: _ =>
//            throw RuntimeException(s"Expected '}' but found '${stringify(t).trim}' at line ${t.line}.")
//          case _ => Parser.NotParsed(tokens)
//      case _ => Parser.NotParsed(tokens)
//
//  private def expr(tokens: List[token.Token]): Parser.Parsed[List[token.Token]] =
//    exprAssign(tokens)
//
//  @tailrec
//  private def exprAssign(tokens: List[token.Token]): Parser.Parsed[List[token.Token]] =
//    exprUnary(tokens) match
//      case Parser.IsParsed(token.Token(ASSIGN, _) :: tail) => exprAssign(tail)
//      case _ => exprOr(tokens)
//
//  private def exprOr(tokens: List[token.Token]): Parser.Parsed[List[token.Token]] =
//    exprAnd(tokens) match
//      case Parser.IsParsed(remainingTokens) => exprOrPrime(remainingTokens)
//      case _ => Parser.NotParsed(tokens)
//
//  @tailrec
//  private def exprOrPrime(tokens: List[token.Token]): Parser.Parsed[List[token.Token]] =
//    tokens match
//      case token.Token(OR, _) :: tail => exprAnd(tail) match
//        case Parser.IsParsed(remainingTokens) => exprOrPrime(remainingTokens)
//        case _ => Parser.IsParsed(tokens)
//      case _ => Parser.IsParsed(tokens)
//
//  private def exprAnd(tokens: List[token.Token]): Parser.Parsed[List[token.Token]] =
//    exprEq(tokens) match
//      case Parser.IsParsed(remainingTokens) => exprAndPrime(remainingTokens)
//      case _ => Parser.NotParsed(tokens)
//
//  @tailrec
//  private def exprAndPrime(tokens: List[token.Token]): Parser.Parsed[List[token.Token]] =
//    tokens match
//      case token.Token(AND, _) :: tail => exprEq(tail) match
//        case Parser.IsParsed(remainingTokens) => exprAndPrime(remainingTokens)
//        case _ => Parser.IsParsed(tokens)
//      case _ => Parser.IsParsed(tokens)
//
//  private def exprEq(tokens: List[token.Token]): Parser.Parsed[List[token.Token]] =
//    exprRel(tokens) match
//      case Parser.IsParsed(remainingTokens) => exprEqPrime(remainingTokens)
//      case _ => Parser.NotParsed(tokens)
//
//  @tailrec
//  private def exprEqPrime(tokens: List[token.Token]): Parser.Parsed[List[token.Token]] =
//    tokens match
//      case token.Token(EQUAL | NOTEQ, _) :: tail => exprRel(tail) match
//        case Parser.IsParsed(remainingTokens) => exprEqPrime(remainingTokens)
//        case _ => Parser.IsParsed(tokens)
//      case _ => Parser.IsParsed(tokens)
//
//  private def exprRel(tokens: List[token.Token]): Parser.Parsed[List[token.Token]] =
//    exprAdd(tokens) match
//      case Parser.IsParsed(remainingTokens) => exprRelPrime(remainingTokens)
//      case _ => Parser.NotParsed(tokens)
//
//  @tailrec
//  private def exprRelPrime(tokens: List[token.Token]): Parser.Parsed[List[token.Token]] =
//    tokens match
//      case token.Token(LESS | LESSEQ | GREATER | GREATEREQ, _) :: tail =>
//        exprAdd(tail) match
//          case Parser.IsParsed(remainingTokens) => exprRelPrime(remainingTokens)
//          case _ => Parser.IsParsed(tokens)
//      case _ => Parser.IsParsed(tokens)
//
//  private def exprAdd(tokens: List[token.Token]): Parser.Parsed[List[token.Token]] =
//    exprMul(tokens) match
//      case Parser.IsParsed(remainingTokens) => exprAddPrime(remainingTokens)
//      case _ => Parser.NotParsed(tokens)
//
//  @tailrec
//  private def exprAddPrime(tokens: List[token.Token]): Parser.Parsed[List[token.Token]] =
//    tokens match
//      case token.Token(ADD | SUB, _) :: tail => exprMul(tail) match
//        case Parser.IsParsed(remainingTokens) => exprAddPrime(remainingTokens)
//        case _ => Parser.IsParsed(tokens)
//      case _ => Parser.IsParsed(tokens)
//
//  private def exprMul(tokens: List[token.Token]): Parser.Parsed[List[token.Token]] =
//    exprCast(tokens) match
//      case Parser.IsParsed(remainingTokens) => exprMulPrime(remainingTokens)
//      case _ => Parser.NotParsed(tokens)
//
//  @tailrec
//  private def exprMulPrime(tokens: List[token.Token]): Parser.Parsed[List[token.Token]] =
//    tokens match
//      case token.Token(MUL | DIV, _) :: tail => exprCast(tail) match
//        case Parser.IsParsed(remainingTokens) => exprMulPrime(remainingTokens)
//        case _ => Parser.IsParsed(tokens)
//      case _ => Parser.IsParsed(tokens)
//
//  @tailrec
//  private def exprCast(tokens: List[token.Token]): Parser.Parsed[List[token.Token]] =
//    tokens match
//      case token.Token(LPAR, _) :: tail => typeBase(tail) match
//        case Parser.IsParsed(remainingTokens) => arrayDecl_?(remainingTokens).get match
//          case token.Token(RPAR, _) :: tail => exprCast(tail)
//          case _ => Parser.NotParsed(tokens)
//        case _ => Parser.NotParsed(tokens)
//      case _ => exprUnary(tokens)
//
//  @tailrec
//  private def exprUnary(tokens: List[token.Token]): Parser.Parsed[List[token.Token]] =
//    tokens match
//      case token.Token(SUB | NOT, _) :: tail => exprUnary(tail)
//      case _ => exprPostfix(tokens)
//
//  private def exprPostfix(tokens: List[token.Token]): Parser.Parsed[List[token.Token]] =
//    exprPrimary(tokens) match
//      case Parser.IsParsed(remainingTokens) => exprPostfixPrime(remainingTokens)
//      case _ => Parser.NotParsed(tokens)
//
//  @tailrec
//  private def exprPostfixPrime(tokens: List[token.Token]): Parser.Parsed[List[token.Token]] =
//    tokens match
//      case token.Token(LBRACKET, _) :: tail => expr(tail) match
//        case Parser.IsParsed(token.Token(RBRACKET, _) :: tail) => exprPostfixPrime(tail)
//        case _ => Parser.IsParsed(tokens)
//      case token.Token(DOT, _) :: token.Token(ID, _) :: tail => exprPostfixPrime(tail)
//      case _ => Parser.IsParsed(tokens)
//
//  private def exprPrimary(tokens: List[token.Token]): Parser.Parsed[List[token.Token]] =
//    tokens match
//      case token.Token(ID, _) :: token.Token(LPAR, _) :: tail => exprs_?(tail).get match
//        case token.Token(RPAR, _) :: tail => Parser.IsParsed(tail)
//        case _ => Parser.NotParsed(tokens)
//      case token.Token(ID | INT | DOUBLE | CHAR | STRING, _) :: tail => Parser.IsParsed(tail)
//      case token.Token(LPAR, _) :: tail => expr(tail) match
//        case Parser.IsParsed(remainingTokens) => remainingTokens match
//          case token.Token(RPAR, _) :: tail => Parser.IsParsed(tail)
//          case _ => Parser.NotParsed(tokens)
//        case _ => Parser.NotParsed(tokens)
//      case _ => Parser.NotParsed(tokens)
//
//  @tailrec
//  @targetName("varDef*")
//  private def varDef_*(tokens: List[token.Token]): Parser.Parsed[List[token.Token]] =
//    varDef(tokens) match
//      case Parser.IsParsed(remainingTokens) => varDef_*(remainingTokens)
//      case Parser.NotParsed(_) => Parser.IsParsed(tokens)
//
//  @tailrec
//  @targetName("varDefOrStm*")
//  private def varDefOrStm_*(tokens: List[token.Token]): Parser.Parsed[List[token.Token]] =
//  // (varDef | stm)*
//    varDef(tokens) match
//      case Parser.IsParsed(remainingTokens) => varDefOrStm_*(remainingTokens)
//      case Parser.NotParsed(remainingTokens) =>
//        stm(remainingTokens) match
//          case Parser.IsParsed(otherTokens) => varDefOrStm_*(otherTokens)
//          case Parser.NotParsed(_) => Parser.IsParsed(tokens)
//
//  @targetName("arrayDecl?")
//  private def arrayDecl_?(tokens: List[token.Token]): Parser.IsParsed[List[token.Token]] =
//    arrayDecl(tokens) match
//      case remainingTokens@Parser.IsParsed(_) => remainingTokens
//      case Parser.NotParsed(_) => Parser.IsParsed(tokens)
//
//
//  @targetName("expr?")
//  private def expr_?(tokens: List[token.Token]): Parser.IsParsed[List[token.Token]] =
//    expr(tokens) match
//      case remainingTokens@Parser.IsParsed(_) => remainingTokens
//      case Parser.NotParsed(_) => Parser.IsParsed(tokens)
//
//  @tailrec
//  @targetName("fnParams?")
//  private def fnParams_?(tokens: List[token.Token]): Parser.IsParsed[List[token.Token]] =
//  // (fnParam (COMMA fnParam)*)?
//    fnParam(tokens) match
//      case Parser.IsParsed(token.Token(COMMA, _) :: tail) => fnParams_?(tail)
//      case remainingTokens@Parser.IsParsed(t :: _) =>
//        if t.tokenCode != RPAR then
//          throw RuntimeException(s"Expected ',' after '${stringify(tokens.take(tokens.length - remainingTokens.get.length))}' " +
//            s"found '${stringify(t)}' at line ${t.line}.")
//        else remainingTokens
//      case Parser.NotParsed(_) => Parser.IsParsed(tokens)
//
//  @tailrec
//  @targetName("exprs?")
//  private def exprs_?(tokens: List[token.Token]): Parser.IsParsed[List[token.Token]] =
//  // (expr (COMMA expr)*)?
//    expr(tokens) match
//      case Parser.IsParsed(token.Token(COMMA, _) :: tail) => exprs_?(tail)
//      case remainingTokens@Parser.IsParsed(t :: _) =>
//        if t.tokenCode != RPAR then
//          throw RuntimeException(s"Expected ',' after '${stringify(tokens.take(tokens.length - remainingTokens.get.length))}' " +
//            s"found '${stringify(t)}' at line ${t.line}.")
//        else remainingTokens
//      case Parser.NotParsed(_) => Parser.IsParsed(tokens)
//
//  @tailrec
//  @targetName("defs*")
//  private def defs_*(tokens: List[token.Token]): Parser.Parsed[List[token.Token]] =
//    structDef(tokens) match
//      case Parser.IsParsed(remainingTokens) => defs_*(remainingTokens)
//      case Parser.NotParsed(_) => fnDef(tokens) match
//        case Parser.IsParsed(remainingTokens) => defs_*(remainingTokens)
//        case Parser.NotParsed(_) => varDef(tokens) match
//          case Parser.IsParsed(remainingTokens) => defs_*(remainingTokens)
//          case Parser.NotParsed(_) => Parser.IsParsed(tokens)
//
//  def parse: List[token.Token] =
//    println(originalTokens)
//    unit(originalTokens).get
//
//
//object Backup extends App :
//  val parser = Backup(lexer.Lexer(new File("testlex.c")).tokenizeFile)
//  println(parser.parse)
