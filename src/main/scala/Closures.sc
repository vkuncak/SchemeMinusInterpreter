// A worksheet to test higher-order functions and classes

def incrementer(x:Int) = (y:Int) => x + y     
val inc = incrementer(100)                    
inc(3)                                        
inc(5)   
abstract class HasApply {
	def applyMe(arg: Int): Int
}
class IncrementerBody(envX: Int) extends HasApply {
	def applyMe(argY: Int):Int = envX + argY
}
def myIncrementer(x:Int) = new IncrementerBody(x)
var myInc = myIncrementer(100)
myInc.applyMe(3)
myInc.applyMe(5)
myInc = myIncrementer(10)
myInc.applyMe(3)
