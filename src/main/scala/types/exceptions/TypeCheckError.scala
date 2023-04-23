package types.exceptions

class TypeCheckError(msg: String, line: Int) extends RuntimeException(s"$msg at line $line.")