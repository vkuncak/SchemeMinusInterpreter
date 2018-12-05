object Main {
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

  // ==============Simple evaluators for explanation only ================
  
  def evalExpr(x: Data): Data = {
    x match {
      case i: Int => i
      case List('+, arg1, arg2) => (evalExpr(arg1), evalExpr(arg2)) match {
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
  val funEnv : Map[String,Data] = {
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

    Map("+" -> plus, "*" -> times, "-" -> minus, "=" -> equality)
  }

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

  // ========== Recursive Evaluator where Environment is a Function =============

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
  
  	// =================== Final Interpreter ============================= 
  
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
  var trace: Boolean = true // set trace to false to turn off tracing
  var indent: Int = 0
  val indentString =
    "                                                              "
  def evaluate(x: Data): Data = eval(x, globalEnv)
  def evaluate(s: String): Data = evaluate(string2lisp(s))

  def main(args: Array[String]): Unit = {

  }
}
