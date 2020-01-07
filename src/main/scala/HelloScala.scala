package hello
package code

abstract class Expr
case class Var(name: String) extends Expr {
  override def toString() = name
}
case class And(e1: Expr, e2: Expr) extends Expr {
  override def toString() = "(" + e1.toString() + " && " + e2.toString() + ")"
}
case class Not(e: Expr) extends Expr {
  override def toString() = "!" + e.toString()
}
case class Or(e1: Expr, e2: Expr) extends Expr {
  override def toString() = "(" + e1.toString() + " || " + e2.toString() + ")"
}
case class Iff(e1: Expr, e2: Expr) extends Expr {
  override def toString() = "(" + e1.toString() + " <==> " + e2.toString() + ")"
}



abstract class Stmt
case class IfElseStmt(e: Expr, t: Stmt, f: Stmt) extends Stmt {
  override def toString() =
    "if (" + e.toString() + ")" + " { " + t.toString() + " } else { " + f.toString() + " }"
}
case class CallStmt(f: String) extends Stmt {
  override def toString() =
    f + "()"
}

object HelloScala {
  def findFunctions(prog : Stmt) : Set[String] = {
    prog match {
      case CallStmt(f) => Set(f)
      case IfElseStmt(e, t, f) =>
        findFunctions(t) ++ findFunctions(f)
    }
  }
  def findVariablesInExpr(e : Expr) : Set[String] = {
    e match {
      case Var(n) => Set(n)
      case And(e1, e2) => findVariablesInExpr(e1) ++ findVariablesInExpr(e2)
      case Not(e) => findVariablesInExpr(e)
      case Or(e1, e2) => findVariablesInExpr(e1) ++ findVariablesInExpr(e2)
      case Iff(e1, e2) => findVariablesInExpr(e1) ++ findVariablesInExpr(e2)
    }
  }

  def findVariables(prog: Stmt) : Set[String] = {
    prog match {
      case CallStmt(f) => Set.empty
      case IfElseStmt(e, t, f) =>
        findVariablesInExpr(e) ++ findVariables(t) ++ findVariables(f)
    }
  }

  def convertToBooleanExp(s: Stmt): Expr = s match {
    case CallStmt(name) => Var(name)
    case IfElseStmt(e, t, f) => 
      Or(And(e, convertToBooleanExp(t)),And(Not(e), convertToBooleanExp(f)))
  }

  def evaluateExpr(e: Expr, m: Map[String, Boolean]): Boolean = e match {
    case Var(name) => m.get(name).get 
    case And(e1, e2) => evaluateExpr(e1, m) && evaluateExpr(e2, m)
    case Or(e1, e2) => evaluateExpr(e1, m) || evaluateExpr(e2, m)
    case Not(e1) => !evaluateExpr(e1, m)
    case Iff(e1, e2) => evaluateExpr(Or(And(e1, e2), And(Not(e1), Not(e2))), m)
  }

  def printTruthTable(e: Expr, vars: Set[String], m: Map[String, Boolean]): Unit = {
    if(!vars.isEmpty) {
      val variable = vars.head 
      printTruthTable( e, vars.tail, m + ( variable -> true)) 
      printTruthTable( e, vars.tail, m + ( variable -> false)) 
    }
    else {
      val ans = evaluateExpr(e, m)
      m.foreach{
         x => x match {
           case (varname, assignment) => print(varname + " = " + assignment + "\t")
         }
      }
      println(" Evaluates to " + ans)
    }
      
  }

  def main(args: Array[String]) {
    printf("hello, world!\n")
    val s1 = IfElseStmt(And(Not(Var("a")), Not(Var("b"))), 
                CallStmt("h"), 
                IfElseStmt(Not(Var("a")), CallStmt("g"), CallStmt("f")))

    val s2 = IfElseStmt(Var("a"), 
                CallStmt("f"), 
                IfElseStmt(Var("b"), CallStmt("g"), CallStmt("h")))

    println(s1.toString())
    println(s2.toString())

    val original = convertToBooleanExp(s1)
    val optimised = convertToBooleanExp(s2)

    println("s1 converted to Boolean Expression -> " + original)
    println("s2 converted to Boolean Expression -> " + optimised)

    val eqExp = Iff(original, optimised)
    val variables = findVariablesInExpr(eqExp)
    printTruthTable(eqExp, variables, Map.empty[String, Boolean])

  }
}
