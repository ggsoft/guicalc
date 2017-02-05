package calc

import scala.collection.mutable.Stack

abstract class Token {
  def pos: Int
  def lex: String
}

case class Action(pos: Int, lex: String, pri: Int) extends Token
case class Result(pos: Int, lex: String, rt: Result.ResultType) extends Token

object Result extends Enumeration {
  type ResultType = Value
  val rtString, rtInteger, rtDouble, rtUnknown = Value
}

object Bracket extends Enumeration {
  type Bracket = Value
  val btOpen, btClose, rtDouble, btNone = Value
}

object Calc {
  val operators="+-*/^"
  val spaces=" \n\r\t"
  val delimiters=operators+spaces+"()"
  var expr = ""
  var ind = 0
  var char = ' '
  var lex = ""
  var error = -1
  val actionStack = new Stack[Action]
  val resultStack = new Stack[Result]
  var opened = 0
  var bracket = Bracket.btNone
  
  val errorMsg = List (
    "Unknown error",          //0
    "Stopped by user",        //1
    "Invalid symbol",         //2
    "Invalid number",         //3
    "Invalid string",         //4
    "Invalid function",       //5
    "Mismatch of brackets",   //6
    "Invalid expression",     //7
    "Invalid argument"        //8
  )

  def isError = error > -1
  def errorStr = if ((error > -1) && (error < errorMsg.length))
    errorMsg(error)+": lexem="+lex+" line="+line(ind)+" position="+position(ind)
    else ""
  def stop(error: Int) = this.error=error

  def line(pos: Int): Int = {
    if ((pos<1) || (pos>expr.length)) 1
    else expr.substring(0,pos-1).count(_=='\n')+1
  }

  def position(pos: Int): Int = {
    val posLineBreak = expr.lastIndexOf('\n',pos-1)
    if (posLineBreak > -1) pos - posLineBreak else pos+1
  }

  def next = {
    if (ind < expr.length) ind+=1
    if (ind == expr.length) char=' '
    else char = expr.charAt(ind)
  }

  def priority(op: Char): Int = op match{
    case '+' | '-' => 1
    case '*' | '/' => 2
    case '^'  => 3  
    case _ => 0
  }

  def open = {
    if (bracket==Bracket.btClose) stop(7)
    else {
      bracket==Bracket.btOpen
      opened+=1
      actionStack.push(Action(ind,"(",0))
      next
    }
  }

  def close = {
    if (bracket==Bracket.btOpen) stop(7)
    else {
      bracket==Bracket.btClose
      opened-=1
      processOperators
      actionStack.pop
      next
    }
  }

  def nextNumber = {
    lex=""
    bracket==Bracket.btNone
    while((""+char).matches("[\\d\\.]")) {
      lex+=char
      next
    }
    if (delimiters.indexOf(char) > -1) {
      try{
        val rt =
          if (lex.matches("\\d*")) {
            lex.toInt
            Result.rtInteger
          }
          else {
            lex.toDouble
            Result.rtDouble
          }
        resultStack.push(Result(ind-lex.length,lex,rt))
      }
      catch { case _ : Throwable => stop(3)}
    }
    else stop(3)
  }

  def nextOperator = {
    bracket==Bracket.btNone
    processOperators
    actionStack.push(Action(ind,""+char,priority(char)))
    next
  }

  def overPriority(a: Action): Boolean = {
    if (isError) false
    else if (a.lex=="(") false
    else if (a.lex==")") true
    else a.pri >= priority(char)
  }
    
  def processOperators = {
    if (actionStack.isEmpty) stop(7)
    else while (overPriority(actionStack.top)) execOperator
  }
    
  def execOperator = {
    var unary = false
    if ((actionStack.isEmpty) || (resultStack.isEmpty)) stop(7)
    else {
      val action = actionStack.pop
      val result = resultStack.pop
      if (actionStack.isEmpty) stop(7)
      else {
        if (resultStack.isEmpty) {
          if ((actionStack.top.lex=="(")&&(actionStack.top.pos>result.pos)) stop(7)
          else unary = true
        }
        else if ((actionStack.top.lex=="(")&&(actionStack.top.pos>resultStack.top.pos)) unary=true
      }
      if (!isError) {
        if (unary) {
          if (action.pos<result.pos) action.lex match {
            case "+" => resultStack.push(result)
            case "-" => resultStack.push(minus(result))  
            case _ => stop(7)
          } else stop(7)    
        }
        else {
          action.lex match {
            case "+" => resultStack.push(add(resultStack.pop,result))
            case "-" => resultStack.push(substract(resultStack.pop,result))
            case "*" => resultStack.push(multiply(resultStack.pop,result))
            case "/" => resultStack.push(divide(resultStack.pop,result))
            case "^" => resultStack.push(power(resultStack.pop,result))              
            case _ => stop(7)
          }    
        }
      }      
    }
  }
  
  def isNumber(r: Result) = {
    (r.rt == Result.rtInteger) || (r.rt == Result.rtDouble)
  }
  
  def isDouble(r1: Result, r2: Result) = {
    (r1.rt == Result.rtDouble) || (r2.rt == Result.rtDouble)
  }
 
  def minus(r: Result) = Result(r.pos,if (r.lex.startsWith("-")) r.lex.substring(1) else "-"+r.lex,r.rt)
  
  def add(r1: Result,r2: Result) = {
    if ((isNumber(r1))&&(isNumber(r2))) {
      if (isDouble(r1,r2)) Result(r2.pos,""+(r1.lex.toDouble+r2.lex.toDouble),Result.rtDouble)
      else Result(r2.pos,""+(r1.lex.toInt+r2.lex.toInt),Result.rtInteger)
    }
    else {
     stop(8)
     null
    }
  }
  
  def substract(r1: Result,r2: Result) = {
    if ((isNumber(r1))&&(isNumber(r2))) {
      if (isDouble(r1,r2)) Result(r2.pos,""+(r1.lex.toDouble-r2.lex.toDouble),Result.rtDouble)
      else Result(r2.pos,""+(r1.lex.toInt-r2.lex.toInt),Result.rtInteger)
    }
    else {
     stop(8)
     null
    }
  }
  
  def multiply(r1: Result,r2: Result) = {
    if ((isNumber(r1))&&(isNumber(r2))) {
      if (isDouble(r1,r2)) Result(r2.pos,""+(r1.lex.toDouble*r2.lex.toDouble),Result.rtDouble)
      else Result(r2.pos,""+(r1.lex.toInt*r2.lex.toInt),Result.rtInteger)
    }
    else {
     stop(8)
     null
    }
  }

  def divide(r1: Result,r2: Result) = {
    if ((isNumber(r1))&&(isNumber(r2))) {
      Result(r2.pos,""+(r1.lex.toDouble/r2.lex.toDouble),Result.rtDouble)
    }
    else {
     stop(8)
     null
    }
  }
  
  def power(r1: Result,r2: Result) = {
    if ((isNumber(r1))&&(isNumber(r2))) {
      Result(r2.pos,""+(math.pow(r1.lex.toDouble,r2.lex.toDouble)),Result.rtDouble)
    }
    else {
     stop(8)
     null
    }
  }
  
  def calculate(expr: String) = {
    this.expr=expr
    error = -1
    actionStack.clear
    resultStack.clear
    ind = -1
    opened = 0
    open
    while ((ind<expr.length)&&(!isError)) {
      if (spaces.indexOf(char) != -1) next
      else if ((""+char).matches("\\d")) nextNumber
      else if (operators.indexOf(char) != -1) nextOperator
      else if (char=='(') open  
      else if (char==')') close  
      else stop(2)
    }
    if (!isError){
      close
      if (opened!=0) stop(6)
      else if ((!actionStack.isEmpty)||(resultStack.size!=1)) stop(7)
    }
  }
  
  def result = if (resultStack.size==1) resultStack.top.lex else ""

}
