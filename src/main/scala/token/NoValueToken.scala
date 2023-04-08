package token

class NoValueToken(tokenCode: TokenCode, override val line: Int) extends Token(tokenCode, line)
