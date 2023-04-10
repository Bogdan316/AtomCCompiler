package scope.exceptions

class RedefinedSymbolError(symbolName: String, line: Int) 
  extends RuntimeException(s"Redefined '$symbolName' at line $line.")
