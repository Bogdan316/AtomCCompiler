package token

sealed trait TokenCode

object TokenCode:
  object Id:
    case object ID extends TokenCode

  sealed trait Types
  object Types:
    case object TYPE_CHAR extends Types
    case object TYPE_INT extends Types
    case object TYPE_DOUBLE extends Types
    case object STRUCT extends Types
    case object VOID extends Types

  // keywords

   IF, ELSE, WHILE, RETURN
  //  delimiters
  case COMMA, SEMICOLON, LPAR, RPAR, LBRACKET, RBRACKET, LACC, RACC, END
  //  operators
  case ADD, SUB, MUL, DIV, DOT, AND, OR, NOT, ASSIGN, EQUAL, NOTEQ, LESS, LESSEQ, GREATER, GREATEREQ
  // constants
  case INT, DOUBLE, CHAR, STRING
