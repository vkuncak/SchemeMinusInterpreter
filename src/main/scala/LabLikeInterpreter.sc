
object LabLikeInterpreter {
  type Data = Any


  // =================== Tokenizer (Lexer) ===============================
  
  class LispTokenizer(s: String) extends Iterator[String] {
    private var i = 0
    private def isDelimiter(ch: Char) = ch <= ' ' || ch == '(' || ch == ')'
    def hasNext: Boolean = {
      while (i < s.length && s.charAt(i) <= ' ') i += 1
      i < s.length
    }
    def next: String =
      if (hasNext) {
        val start = i
        if (isDelimiter(s.charAt(i))) i += 1
        else
          do i += 1
          while (i < s.length && !isDelimiter(s.charAt(i)))
        s.substring(start, i)
      } else sys.error("premature end of input")
  }

  // ============================ Parser ===============================

  def s2l(s:String) = string2lisp(s) // shorthand
  def string2lisp(s: String): Data = {
    val it = new LispTokenizer(s)
    def parseExpr(token: String): Data = {
      if (token == "(") parseList
      else if (token == ")") sys.error("unbalanced parentheses")
      else if (Character.isDigit(token.charAt(0))) Integer.parseInt(token)
      else Symbol(token)
    }

    def parseList: List[Data] = {
      val token = it.next
      if (token == ")") List() else parseExpr(token) :: parseList
    }
    parseExpr(it.next)
  }

  def lisp2string(x: Data): String = x match {
    case Symbol(name) =>
      name
    case xs: List[_] =>
      (xs map lisp2string).mkString("(", " ", ")")
    case _ =>
      x.toString
  }
  
    // Checked conversions ----------- -----------------------------------

  def asList(x: Data): List[Data] = x match {
    case xs: List[_] => xs
    case _           => sys.error("malformed list: " + x)
  }

  def paramName(x: Data): String = x match {
    case Symbol(name) => name
    case _            => sys.error("malformed parameter")
  }

  // We wrap lambdas inside a case class
  case class Lambda(f: List[Data] => Data)

  // Environments -------------------------------------------------------

  abstract class Environment {
    def lookup(n: String): Data
    def extend(name: String, v: Data) = new Environment {
      def lookup(n: String): Data =
        if (n == name) v else Environment.this.lookup(n)
    }
    def extendMulti(ps: List[String], vs: List[Data]): Environment = (ps, vs) match {
      case (List(), List())         => this
      case (p :: ps1, arg :: args1) => extend(p, arg).extendMulti(ps1, args1)
      case _                        => sys.error("wrong number of arguments")
    }
    def extendRec(name: String, expr: Environment => Data) = new Environment {
      def lookup(n: String): Data =
        if (n == name) expr(this)
        else Environment.this.lookup(n)
    }
  }
  object EmptyEnvironment extends Environment {
    def lookup(n: String): Data = sys.error("undefined: " + n)
  }

  var globalEnv = EmptyEnvironment
    .extend("=", Lambda(args => (args: @unchecked) match {
      case List(arg1, arg2) => if (arg1 == arg2) 1 else 0
    }))

    .extend("+", Lambda(args => (args: @unchecked) match {
      case List(arg1: Int, arg2: Int) => arg1 + arg2
    }))
    .extend("-", Lambda(args => (args: @unchecked) match {
      case List(arg1: Int, arg2: Int) => arg1 - arg2
    }))

    .extend("*", Lambda(args => (args: @unchecked) match {
      case List(arg1: Int, arg2: Int) => arg1 * arg2
    }))
    .extend("/", Lambda(args => (args: @unchecked) match {
      case List(arg1: Int, arg2: Int) => arg1 / arg2
    }))

    .extend("nil", Nil)
    .extend("cons", Lambda(args => (args: @unchecked) match {
      case List(arg1, arg2) => arg1 :: asList(arg2)
    }))

    .extend("car", Lambda(args => (args: @unchecked) match {
      case List(x :: xs) => x
    }))
    .extend("cdr", Lambda(args => (args: @unchecked) match {
      case List(x :: xs) => xs
    }))

    .extend("null?", Lambda(args => args match {
      case List(Nil) => 1
      case _         => 0
    }))

  def eval1(x: Data, env: Environment): Data = x match {
    case _: Int =>
      x
    case Symbol(name) =>
      env lookup name
    case 'val :: Symbol(name) :: expr :: rest :: Nil =>
      eval(rest, env.extend(name, eval(expr, env)))
    case 'if :: cond :: thenpart :: elsepart :: Nil =>
      if (eval(cond, env) != 0) eval(thenpart, env)
      else eval(elsepart, env)

    // syntactic sugar

    case 'and :: x :: y :: Nil =>
      eval('if :: x :: y :: 0 :: Nil, env)
    case 'or :: x :: y :: Nil =>
      eval('if :: x :: 1 :: y :: Nil, env)

    // def, quote, lambda and application

   	/* This version of def permanentaly modifies globalEnv
       if invoked from the top level */
    case 'def :: Symbol(name) :: body :: Nil => // definition GLOBAL
      if (env == globalEnv) {
        globalEnv = env.extendRec(name, env1 => eval(body, env1))
        "def " + name // just confirm we got the def
      } else
        sys.error("trying to add global definition in some inner scope")
    case 'def :: Symbol(name) :: body :: rest :: Nil => // GLOBAL or LOCAL
      if (env == globalEnv)
        globalEnv = env.extendRec(name, env1 => eval(body, env1))
      eval(rest, env.extendRec(name, env1 => eval(body, env1))) // evaluate
    case 'quote :: y :: Nil =>
      y
    case 'lambda :: params :: body :: Nil =>
      mkLambda(asList(params) map paramName, body, env)
    case operator :: operands =>
      try {
        apply(eval(operator, env), operands map (x => eval(x, env)))
      } catch {
        case ex: MatchError => sys.error("bad arguments for function " + operator)
      }
  }


  def mkLambda(ps: List[String], body: Data, env: Environment) =
    Lambda { args => eval(body, env.extendMulti(ps, args)) }


  def apply(f: Data, args: List[Data]) = f match {
    case Lambda(f) =>
      f(args)
    case _ =>
      sys.error("application of non-function " + f + " to arguments " + args)
  }

  // Evaluation with tracing
  def eval(x: Data, env: Environment): Data = {
    val prevexp = curexp
    curexp = x
    if (trace) {
      println(indentString.substring(0, indent) + "===> " + x)
      indent += 1
    }
    val result = eval1(x, env)
    if (trace) {
      indent -= 1
      println(indentString.substring(0, indent) + "<=== " + result)
    }
    curexp = prevexp
    result
  }
  // Diagnostics---------------------------------------------------
  var curexp: Data = null
  var trace: Boolean = false // set trace to false to turn off tracing
  var indent: Int = 0
  val indentString =
    "                                                              "
  def evaluate(x: Data): Data = eval(x, globalEnv)
  def evaluate(s: String): Data = evaluate(string2lisp(s))
  def main(args: Array[String]): Unit = {
  }
  def handle(e: => Any) = {
    try (e) catch {
        case _ => "Error during evaluation"
    }
  }
}

import LabLikeInterpreter._

// Examples for parsing and in general
string2lisp("(lambda (x) (+ (* x x) 1))")
eval(string2lisp("((lambda (x) (+ (* x x) 1)) 7)"), globalEnv)
evaluate("(def factorial (lambda (x) (if (= x 0) 1 (* x (factorial (- x 1))))) (factorial 6))")
evaluate("(def map (lambda (f l) (if (null? l) nil (cons (f (car l)) (map f (cdr l))))) (map (lambda (x) (* x x)) (cons 10 (cons 2 (cons 5 nil)))))")
    // '(1 2 3)
