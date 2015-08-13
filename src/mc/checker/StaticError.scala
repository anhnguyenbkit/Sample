package mc.checker

/**
 * @author nhphung
 */
case class RedeclareVariable(name:String,vartype:MCType) extends Exception
case class RedeclareMethod(name:String,partype:List[MCType],rettype:MCType) extends Exception
case class UndeclareIdentifier(name:String) extends Exception
case class TypeMismatchInExpression(exp:String) extends Exception
case class TypeMismatchInStatement(stmt:String ) extends Exception
