package mc.checker

/**
 * @author nhphung
 */


import mc.parser._
import java.io.{PrintWriter,File}
import org.antlr.v4.runtime.ANTLRFileStream;
import org.antlr.v4.runtime.CommonTokenStream;
import org.antlr.v4.runtime.tree._
import scala.collection.JavaConverters._

object StaticChecker {
  def test(infile:ANTLRFileStream,outfile:PrintWriter) = {
    val lexer = new MCLexer(infile);
    val tokens = new CommonTokenStream(lexer);
    val parser = new MCParser(tokens);
    val progtree = parser.program()
    val astbuild = new BuildAST()
    val ast = astbuild.visit(progtree)
    outfile.println(ast)
    val bldecls = new BuildDeclaration()
    val env = bldecls.visit(progtree).asInstanceOf[List[(String,MCType)]]
    val dup = findDuplicate(env,(x:(String,MCType)) => x._1)
    dup match {
      case Some((n,MethodType(p,r))) => throw RedeclareMethod(n,p,r)
      case Some((n,t)) => throw RedeclareVariable(n,t)
      case None => outfile.println(env)
    }
    val tc = new TypeChecking(env)
    tc.visit(progtree)  
  }
  def findDuplicate[T](lst:List[T],mapfunc:T=>String):Option[T] = lst match {
    case List() => None
    case head::tail => findDuplicate(head,tail,mapfunc) match {
      case None => findDuplicate(tail,mapfunc)
      case Some(t) => Some(t)
    }
  }
  def findDuplicate[T](head:T,rest:List[T],mapfunc:T=>String):Option[T] = rest match {
    case List() => None
    case h::t => if (mapfunc(head) == mapfunc(h)) Some(h) else findDuplicate(head,t,mapfunc)
  }
  //def getName(d:Declare) = d.getName
}

trait MCType
case object IntType extends MCType
case object FloatType extends MCType
case object ClassType extends MCType
case class MethodType(partype:List[MCType],rettype:MCType) extends MCType
case class JointType(typlst:List[MCType]) extends MCType

/*trait Declare {def getName:String}
case class VarDeclare(name:String,vartype:MCType) extends Declare {
  def getName = name
}
case class MethodDeclare(name:String,partype:List[MCType],rettype:MCType) extends Declare {
  def getName = name
}
*/

class BuildDeclaration extends MCBaseVisitor[Object] {
  
  override def visitProgram(ctx:MCParser.ProgramContext) = ctx.clas_decl().asScala.map(visit).toList.asInstanceOf[List[List[(String,MCType)]]].flatten
  
  override def visitClas_decl(ctx:MCParser.Clas_declContext) ={
    val name = ctx.IDENTIFIER(0).getText
    List((name,ClassType))
  }
  
  override def visitDecl(ctx: MCParser.DeclContext) = visit(ctx.getChild(0))//if (ctx.var_dl() != null) visit(ctx.var_dl()) else visit(ctx.func_decl())
  
  override def visitVar_dl(ctx:MCParser.Var_dlContext) = visit(ctx.var_decl())
  
  override def visitVar_decl(ctx:MCParser.Var_declContext) = {
    val il = visit(ctx.idlist()).asInstanceOf[List[String]]
    val vtype = visit(ctx.mctype()).asInstanceOf[MCType]
    il.map(x => (x,vtype))
  }
  
  override def visitBody(ctx:MCParser.BodyContext) = ctx.mem().asScala.map(visit).toList.asInstanceOf[List[(String,MCType)]]
  
  override def visitMem(ctx:MCParser.MemContext) = ctx.getChild(0)
  
  override def visitIdlist(ctx:MCParser.IdlistContext) = ctx.IDENTIFIER().asScala.map(visit).toList
  
  override def visitFunc_decl(ctx: MCParser.Func_declContext) = {
    val name = ctx.IDENTIFIER().getText
    val partype = if (ctx.parlist() != null) visit(ctx.parlist()).asInstanceOf[List[MCType]] else List()
    val rettype = visit(ctx.mctype()).asInstanceOf[MCType]
    List((name,MethodType(partype,rettype)))
  }
  
  override def visitMctype(ctx: MCParser.MctypeContext) = ctx.getChild(0).getText match {
    case "int" => IntType
    case "float" => FloatType
  }
  
  override def visitParlist(ctx: MCParser.ParlistContext) = 
    ctx.var_decl()
    .asScala
    .map(visit)
    .toList
    .asInstanceOf[List[List[(String,MCType)]]]
    .flatten
    .map(_ match {
      case (_,t) => t
    })
    
  override def visitTerminal(node:TerminalNode) = node.getText
}
class TypeChecking(env:List[(String,MCType)]) extends MCBaseVisitor[Object] {
  def lookup[T](name:String,lst:List[T],func:T=>String):Option[T] = lst match {
    case List() => None
    case head::tail => if (func(head) == name) Some(head) else lookup(name,tail,func)
  }
  def typecheck(pt:MCType,at:MCType):Boolean = pt == at
  def typecheck(pt:List[MCType],at:List[MCType]):Boolean = 
      if (pt.length != at.length) false 
      else if (pt.length == 0) true 
      else pt.zip(at).forall(x=>typecheck(x._1,x._2))
  def typecheck(pt:JointType,at:List[MCType]):Boolean = typecheck(pt.typlst,at)

  override def visitCall(ctx: MCParser.CallContext) = {
    val id = ctx.IDENTIFIER.getText
    val iddecl = lookup(id,env,(x:(String,MCType)) => x._1)
    if (iddecl == None) 
        throw UndeclareIdentifier(id)
    else {
        val explst = ctx.explist()      
        val at = if (explst != null) visit(explst).asInstanceOf[JointType].typlst else List()
        
        iddecl match {                   
              case Some((_,MethodType(pt,rt))) => if (typecheck(pt,at)) rt else throw TypeMismatchInStatement(ctx.getText)            
              case _ => throw TypeMismatchInStatement(ctx.getText)
        }       
    }
  }
  
  override def visitExplist(ctx:MCParser.ExplistContext) = JointType(ctx.exp().asScala.map(visit).toList.asInstanceOf[List[MCType]])

  override def visitExp(ctx:MCParser.ExpContext) = 
              ctx.moreterm().asScala.map(visit).toList.
              asInstanceOf[List[(String,MCType)]].
              foldLeft(visit(ctx.term()).asInstanceOf[MCType])((x,y)=> y match {
                case (_,t) => if (typecheck(x,t)) t else throw TypeMismatchInExpression(ctx.getText)
              })
              
  
  override def visitMoreterm(ctx:MCParser.MoretermContext) = (visit(ctx.getChild(0)),visit(ctx.getChild(1)))
  
  override def visitTerm(ctx:MCParser.TermContext) = 
              ctx.morefact().asScala.map(visit).toList.
              asInstanceOf[List[(String,MCType)]].
              foldLeft(visit(ctx.fact()).asInstanceOf[MCType])((x,y)=> y match {
                case ("*",t) => if (typecheck(x,t)) t else throw TypeMismatchInExpression(ctx.getText)
                case ("/",t) => if (typecheck(x,t)) FloatType else throw TypeMismatchInExpression(ctx.getText)
              })
              
  
  override def visitMorefact(ctx:MCParser.MorefactContext) = (visit(ctx.getChild(0)),visit(ctx.getChild(1)))
  
  
  override def visitFact(ctx: MCParser.FactContext) = {
    if (ctx.INTLIT() != null) IntType
    else if (ctx.FLOATLIT() != null) FloatType
         else if (ctx.IDENTIFIER() != null) {
              val id = ctx.IDENTIFIER.getText
              val iddecl = lookup(id,env,(x:(String,MCType)) => x._1)
              if (iddecl == None) 
                throw UndeclareIdentifier(id)
              else {
                val explst = ctx.explist()      
                if (ctx.LBRAC() != null) {
                    val at = if (explst != null) visit(explst).asInstanceOf[JointType].typlst else List()
                    iddecl match {                   
                      case Some((_,MethodType(pt,rt))) => if (typecheck(pt,at)) rt else throw TypeMismatchInExpression(ctx.getText)
                      case _ => throw TypeMismatchInExpression(ctx.getText)
                    }
                } else 
                  iddecl match {
                    case Some((_,IntType)) => IntType
                    case Some((_,FloatType)) => FloatType
                    case _ => throw TypeMismatchInExpression(ctx.getText)
                  }
             }
         } else        
          visit(ctx.exp())        
    }       

  override def visitTerminal(node:TerminalNode) = node.getText
}
trait AST
case class ProgramAST(decls:List[DeclAST]) extends AST
trait BodyMember extends AST
trait DeclAST
trait Stmt extends BodyMember
case class VarDeclAST(name:String,vtype:MCType) extends DeclAST with BodyMember
case class ParDeclAST(name:String,vtype:MCType) extends DeclAST
case class ManyVarDeclAST(vardecls:List[VarDeclAST]) extends DeclAST
case class MethodDeclAST(name:String,partype:List[ParDeclAST],rettype:MCType,body:List[BodyMember]) extends DeclAST
case class ClassDeclAST(name:String, body:List[BodyMember]) extends DeclAST

case class AssignAST(lhs:String,rhs:ExpAST) extends Stmt
case class CallStmtAST(name:String,args:List[ExpAST]) extends Stmt

trait ExpAST extends AST
case class BinaryAST(left:ExpAST,op:String,right:ExpAST) extends ExpAST
case class IntAST(value:Integer) extends ExpAST
case class FloatAST(value:Float) extends ExpAST
case class CallExpAST(name:String,args:List[ExpAST]) extends ExpAST
case class IdAST(name:String) extends ExpAST

class BuildAST extends MCBaseVisitor[Object] {
  def flatten(arg:List[DeclAST]):List[DeclAST] = arg match {
    case List() => List()
    case ManyVarDeclAST(l)::tail => l ++ flatten(tail)
    case h::tail => h :: flatten(tail)
  }
  override def visitProgram(ctx:MCParser.ProgramContext) = ProgramAST(flatten(ctx.clas_decl().asScala.map(visit).toList.asInstanceOf[List[DeclAST]]))
  
  override def visitClas_decl(ctx:MCParser.Clas_declContext) = {
    val name = ctx.IDENTIFIER(0).getText
    val body = visit(ctx.body()).asInstanceOf[List[BodyMember]]
    ClassDeclAST(name,body)
  }
  
  
  override def visitDecl(ctx: MCParser.DeclContext) = visit(ctx.getChild(0))    //if (ctx.var_dl() != null) visit(ctx.var_dl()) else visit(ctx.func_decl())
  
  override def visitVar_dl(ctx:MCParser.Var_dlContext) = visit(ctx.var_decl())
  
  override def visitVar_decl(ctx:MCParser.Var_declContext) = {
    val il = visit(ctx.idlist()).asInstanceOf[List[String]]
    val vtype = visit(ctx.mctype()).asInstanceOf[MCType]
    if (il.length == 1) VarDeclAST(il(0),vtype) else ManyVarDeclAST(il.map(x => VarDeclAST(x,vtype)))
  }
  
  override def visitIdlist(ctx:MCParser.IdlistContext) = ctx.IDENTIFIER().asScala.map(visit).toList
  
  override def visitFunc_decl(ctx: MCParser.Func_declContext) = {
    val name = ctx.IDENTIFIER().getText
    val partype = if (ctx.parlist() != null) visit(ctx.parlist()).asInstanceOf[List[ParDeclAST]] else List()
    val rettype = visit(ctx.mctype()).asInstanceOf[MCType]
    val body = visit(ctx.body()).asInstanceOf[List[BodyMember]]
    MethodDeclAST(name,partype,rettype,body)
  }
  
  override def visitMctype(ctx: MCParser.MctypeContext) = ctx.getChild(0).getText match {
    case "int" => IntType
    case "float" => FloatType
  }
  def var2par(lst:List[DeclAST]):List[ParDeclAST] = lst match {
    case List() => List()
    case VarDeclAST(n,t)::tail => ParDeclAST(n,t)::var2par(tail)
    case ManyVarDeclAST(l)::tail => var2par(l) ++ var2par(tail)
  }
  override def visitParlist(ctx: MCParser.ParlistContext) = 
    var2par(ctx.var_decl().asScala.map(visit).toList.asInstanceOf[List[DeclAST]])

  override def visitBody(ctx:MCParser.BodyContext) = ctx.mem().asScala.map(visit).toList
  
//  override def visitMem(ctx:MCParser.MemContext) = visit(ctx.getChild(0))//if (ctx.decl() != null) visit(ctx.var_dl()) else visit(ctx.stmt())
  
  //override def visitStmt(ctx:MCParser.StmtContext) = visit(ctx.getChild(0))//if (ctx.assign() != null) visit(ctx.assign()) else visit(ctx.call())
  
  override def visitAssign(ctx:MCParser.AssignContext) = {
    val id = ctx.IDENTIFIER().getText
    val exp = visit(ctx.exp()).asInstanceOf[ExpAST]
    AssignAST(id,exp)
  }
  override def visitCall(ctx:MCParser.CallContext) = {
    val id = ctx.IDENTIFIER().getText
    val args = if (ctx.explist() != null) visit(ctx.explist()).asInstanceOf[List[ExpAST]] else List()
    CallStmtAST(id,args)
  } 
  
  override def visitExplist(ctx:MCParser.ExplistContext) = ctx.exp().asScala.map(visit).toList
  
  override def visitExp(ctx:MCParser.ExpContext) = {
    val first = visit(ctx.term()).asInstanceOf[ExpAST]
    val lst = ctx.moreterm().asScala.map(visit).toList.asInstanceOf[List[(String,ExpAST)]]
    lst.foldLeft(first)((x,y) => y match {case (a,b) => BinaryAST(x,a,b)} )
  }
  
  override def visitMoreterm(ctx:MCParser.MoretermContext) = {
    val op = visit(ctx.getChild(0))
    val exp = visit(ctx.term()).asInstanceOf[ExpAST]
    (op,exp)
  }
  override def visitTerm(ctx:MCParser.TermContext) = {
    val first = visit(ctx.fact()).asInstanceOf[ExpAST]
    val lst = ctx.morefact().asScala.map(visit).toList.asInstanceOf[List[(String,ExpAST)]]
    lst.foldLeft(first)((x,y) => y match {case (a,b) => BinaryAST(x,a,b)} )
  }
  
  override def visitMorefact(ctx:MCParser.MorefactContext) = {
    val op = visit(ctx.getChild(0))
    val exp = visit(ctx.fact()).asInstanceOf[ExpAST]
    (op,exp)
  }  
  
  override def visitFact(ctx:MCParser.FactContext) = {
    if (ctx.IDENTIFIER() != null) {
      val id = ctx.IDENTIFIER().getText
      if (ctx.LBRAC() != null) {
        val args =if (ctx.explist() != null) visit(ctx.explist()).asInstanceOf[List[ExpAST]] else List()
        CallExpAST(id,args)
      } else IdAST(id)
    } else if (ctx.INTLIT() != null) IntAST(ctx.INTLIT().getText.toInt) 
           else if (ctx.FLOATLIT() != null) FloatAST(ctx.FLOATLIT().getText.toFloat)
                else visit(ctx.exp())
  }
  override def visitTerminal(node:TerminalNode) = node.getText

}
  