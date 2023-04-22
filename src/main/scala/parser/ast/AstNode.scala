package parser.ast

import token.{Token, TokenWithValue}

trait AstNode

object AstNode:

  final case class AstRoot(definitions: DefinitionNode*) extends AstNode


  sealed trait BodyStatementNode extends AstNode


  sealed trait StatementNode extends BodyStatementNode

  object StatementNode:

    final case class CompoundStmNode(statements: BodyStatementNode*) extends StatementNode

    final case class IfStmNode(condition: ExpressionNode, thenBranch: StatementNode, elseBranch: Option[StatementNode] = None) extends StatementNode

    final case class WhileStmNode(condition: ExpressionNode, body: StatementNode) extends StatementNode

    final case class ReturnStmNode(expr: Option[ExpressionNode] = None) extends StatementNode

    final case class ExpressionStmNode(expr: Option[ExpressionNode] = None) extends StatementNode



  sealed trait DefinitionNode extends BodyStatementNode

  object DefinitionNode:

    final case class StructDefNode (id: Token, variableDefinitions: VariableDefNode*) extends DefinitionNode

    final case class VariableDefNode (typeBase: DefinitionUtils.TypeBaseNode, id: Token, arraySize: Option[DefinitionUtils.ArraySizeNode] = None ) extends DefinitionNode

    final case class FunctionDefNode (returnType: DefinitionUtils.TypeBaseNode, id: Token, compoundStm: AstNode.StatementNode.CompoundStmNode, functionParams: DefinitionUtils.FunctionParamNode * ) extends DefinitionNode



  sealed trait DefinitionUtils extends AstNode

  object DefinitionUtils:

    final case class TypeBaseNode (baseType: Token, structId: Option[Token] = None) extends DefinitionUtils

    final case class ArraySizeNode (size: Option[Int] = None) extends DefinitionUtils

    final case class FunctionParamNode (typeBase: TypeBaseNode, id: Token, arraySize: Option[ArraySizeNode] = None) extends DefinitionUtils



  sealed trait ExpressionNode extends AstNode

  object ExpressionNode:

    final case class BinaryExprNode (left: ExpressionNode, operator: Token, right: ExpressionNode) extends ExpressionNode

    final case class UnaryExprNode (operator: Token, right: ExpressionNode) extends ExpressionNode

    final case class FunctionCallExprNode (funName: Token, expressions: ExpressionNode *) extends ExpressionNode

    final case class LiteralExprNode[T] (literal: TokenWithValue[T]) extends ExpressionNode

    final case class VariableExprNode[T] (variable: TokenWithValue[T]) extends  ExpressionNode

    final case class FieldAccessExprNode[T] (left: ExpressionNode, field: TokenWithValue[T]) extends ExpressionNode

    final case class ArrayAccessExprNode (arrayExpression: ExpressionNode, idxExpression: ExpressionNode) extends ExpressionNode

    final case class CastExprNode (typeBase: DefinitionUtils.TypeBaseNode, arraySize: Option[DefinitionUtils.ArraySizeNode] = None, castedExpr: ExpressionNode) extends ExpressionNode
