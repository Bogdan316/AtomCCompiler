package token

enum TokenCode:
  case ID
  // keywords
  case TYPE_CHAR, TYPE_INT, TYPE_DOUBLE, STRUCT, IF, ELSE, WHILE, VOID, RETURN
  //  delimiters
  case COMMA, SEMICOLON, LPAR, RPAR, LBRACKET, RBRACKET, LACC, RACC, END
  //  operators
  case ADD, SUB, MUL, DIV, DOT, AND, OR, NOT, ASSIGN, EQUAL, NOTEQ, LESS, LESSEQ, GREATER, GREATEREQ
  // constants
  case INT, DOUBLE, CHAR, STRING
