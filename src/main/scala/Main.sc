
object Main {
  type Data = Any

  // =================== Tokenizer (Lexer) ================
  
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

  // ============================ Parser ==================

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

  // ========= Simple evaluators for explanation only ======
  
  def evalExpr(x: Data): Data = {
    x match {
      case i: Int => i
      case List('+, arg1, arg2) => (evalExpr(arg1), 
                                   evalExpr(arg2)) match {
        case (x1: Int, x2: Int) => x1 + x2
        case (v1, v2)           => sys.error("+ takes two integers, but was invoked with " + v1 + " and " + v2)
      }
      case List('*, arg1, arg2) => (evalExpr(arg1), evalExpr(arg2)) match {
        case (x1: Int, x2: Int) => x1 * x2
        case (v1, v2)           => sys.error("* takes two integers, but was invoked with " + v1 + " and " + v2)
      }
      case _ => sys.error("Did not know how to evaluate " + x)
    }
  }

  def evalSym(x: Data, env: Map[String, Data]): Data = {
    x match {
      case i: Int => i
      case Symbol(s) => env.get(s) match {
        case Some(v) => v
        case None    => sys.error("Could not find " + s + " in the environment.")
      }
      case List('+, arg1, arg2) => (evalSym(arg1, env), evalSym(arg2, env)) match {
        case (x1: Int, x2: Int) => x1 + x2
      }
      case List('*, arg1, arg2) => (evalSym(arg1, env), evalSym(arg2, env)) match {
        case (x1: Int, x2: Int) => x1 * x2
      }
    }
  }

  def evalFun(x: Data, env: Map[String, Data]): Data = {
    x match {
      case i: Int => i
      case Symbol(s) => env.get(s) match {
        case Some(v) => v
        case None => sys.error("Unknown symbol " + s)
      }
      case List('if, bE, trueCase, falseCase) =>
        if (evalFun(bE, env) != 0) evalFun(trueCase, env)
        else evalFun(falseCase, env)            
      case opE :: argsE => {
        val op = evalFun(opE, env).asInstanceOf[List[Data] => Data]
        val args: List[Data] = argsE.map((arg: Data) => evalFun(arg, env))
        op(args)
      }
    }
  }

  val plus = (args: List[Data]) => args match {
    case List(x: Int, y: Int) => x + y
    case _                    => sys.error("plus expects two integers, applied to " + args)
  }
  val times = (args: List[Data]) => args match {
    case List(x: Int, y: Int) => x * y
    case _                    => sys.error("times expects two integers, applied to " + args)
  }
  val minus = (args: List[Data]) => args match {
    case List(x: Int, y: Int) => x - y
    case _                    => sys.error("minus expects two integers, applied to " + args)
  }
  val equality = (args: List[Data]) => args match {
    case List(x, y) => if (x == y) 1 else 0
    case _          => sys.error("minus expects two values, applied to " + args)
  }

  val funEnv : Map[String,Data] = 
    Map("+" -> plus, "*" -> times, "-" -> minus, "=" -> equality)

  def evalVal(x: Data, env: Map[String, Data]): Data = {
    x match {
      case i: Int => i
      case Symbol(s) => env.get(s) match {
        case Some(v) => v
        case None    => sys.error("Unknown symbol " + s)
      }
      case List('if, bE, trueCase, falseCase) =>
        if (evalVal(bE, env) != 0) evalVal(trueCase, env)
        else evalVal(falseCase, env)
      case List('val, Symbol(s), expr, rest) => evalVal(rest, env + (s -> evalVal(expr, env)))
      case opE :: argsE => {
        val op = evalVal(opE, env).asInstanceOf[List[Data] => Data]
        val args: List[Data] = argsE.map((arg: Data) => evalVal(arg, env))
        op(args)
      }
    }
  }

  def evalLambda(x: Data, env: Map[String, Data]): Data = {
    x match {
      case i: Int => i
      case Symbol(s) => env.get(s) match {
        case Some(v) => v
        case None    => sys.error("Unknown symbol " + s)
      }
      case List('if, bE, trueCase, falseCase) =>
        if (evalVal(bE, env) != 0) evalVal(trueCase, env)
        else evalVal(falseCase, env)      
      case List('val, Symbol(s), expr, rest) => evalLambda(rest, env + (s -> evalLambda(expr, env)))
      case List('lambda, params: List[Data], body) =>
        ((args: List[Data]) => {
          val paramBinding = params.map(_.asInstanceOf[Symbol].name).zip(args)
          evalLambda(body, env ++ paramBinding)
        })
      case opE :: argsE => {
        val op = evalLambda(opE, env).asInstanceOf[List[Data] => Data]
        val args: List[Data] = argsE.map((arg: Data) => evalLambda(arg, env))
        op(args)
      }
    }
  }

  // = Recursive Evaluator where Environment is a Function =

  type Env = String => Option[Data]
  val recEnv : Env = ((id:String) => funEnv.get(id)) 
  def updateEnv(env : Env, bindings : List[(String,Data)]) : Env = bindings match {
    case Nil => env
    case (id,d)::rest => ((x:String) =>
      	if (x==id) Some(d)
      	else updateEnv(env,rest)(x))
  }
  def updateEnvRec(env: Env, s : String, expr : Data) : Env = {
    def newEnv : Env = ((id:String) =>
      if (id==s) Some(evalRec(expr, newEnv))
      else env(id)
    )
    newEnv
  }  

  def evalRec(x: Data, env: Env): Data = {
    x match {
      case i: Int => i
      case Symbol(s) => env(s) match {
        case Some(v) => v
        case None => sys.error("Unknown symbol " + s)
      }
      case List('lambda, params: List[Data], body) =>
        ((args: List[Data]) => {
          val paramBinding = params.map(_.asInstanceOf[Symbol].name).zip(args)
          evalRec(body, updateEnv(env, paramBinding))
        })
      case List('val, Symbol(s), expr, rest) =>
        evalRec(rest, updateEnv(env, List(s -> evalRec(expr, env))))
      case List('def, Symbol(s), expr, rest) => {
        evalRec(rest, updateEnvRec(env, s, expr))
      }
      case List('if, bE, trueCase, falseCase) =>
        if (evalRec(bE, env) != 0) evalRec(trueCase, env)
        else evalRec(falseCase, env)
      case opE :: argsE => {
        val op = evalRec(opE, env).asInstanceOf[List[Data] => Data]
        val args: List[Data] = argsE.map((arg: Data) => evalRec(arg, env))
        op(args)
      }
    }
  }
  
  def handle(e: => Any) = {
    try (e) catch {
        case _ => "Error during evaluation"
    }
  }
}
import Main._

// Example for parsing
string2lisp("(lambda (x) (+ (* x x) 1))")

// Interpreting: from simpler to more complex interpreter

// 1. evalExpr - expressions with constants
evalExpr(s2l("(+ 7 (* 2 5))"))

// 2. evalSym - expressions with symbols and the environment
evalSym(s2l("(+ 41 (* 2 q))"), Map("q" -> 10))

// 3. evalFun - evaluating function application
evalFun(List('=, 30, List('*, 2, 'q)), funEnv + ("q" -> 15))

// 4. evalVal - allowing val (non-recursive) definitions
evalVal(s2l("(val answer (+ 12 q) (* answer answer))"), funEnv + ("q" -> 30))

// 5. evalLambda - creating lambda expressions 
evalLambda(string2lisp("((lambda (x) (+ x x)) (* 3 z))"), funEnv + ("z" -> 15))

evalLambda(s2l("(val dup (lambda (x) (+ x x)) (dup (dup 7)))"),funEnv)

evalLambda(s2l(
"""
(val dup1 (lambda (x) (if (= x 10) 100 (+ x x)))
	(dup1 (dup1 10))
)
"""), funEnv)

evalLambda(s2l("""
  (val fact (lambda (n)
     ((lambda (fact)
        (fact fact n))
      (lambda (ft x)
      	(if  (= x 0) 1 (* x (ft ft (- x 1)))))
     ))
     (fact 6)
  )
"""), funEnv)
// This is like Y combinator that makes Y_F term, but we wrap self-application (x x) into a lambda so it does not get evaluated too soon, and we expand it afterwards

evalLambda(s2l("""
(val mkZ (lambda (f) 
    (val comb (lambda (x)
    		(f (lambda (v)  
    			   ((x x) v)
    			  )
    		 )
    )
    (comb comb)
  )
  )
  (val factorial 
      (lambda (fact) (lambda (x)
        (if  (= x 0) 
             1 
            (* x (fact (- x 1))))))
  ((mkZ factorial) 6)))
"""), funEnv)

// This works because factorial is never called recursively
handle(evalLambda(s2l("""
(val factorial
  (lambda (x)
    (if  (= x 0) 
          1 
          (* x (factorial (- x 1)))))  
  (factorial 0)
)
"""), funEnv))

// This lo longer works -- factorial is not known within body of factorial
handle(evalLambda(s2l("""
(val factorial
  (lambda (x)
    (if  (= x 0) 
          1 
          (* x (factorial (- x 1)))))  
  (factorial 1)
)
"""), funEnv))

