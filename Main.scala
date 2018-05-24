package stringmatcher

import scala.util.parsing.combinator._

abstract class Tree

//S -> E$
//E -> T '|' E | T
//T -> F T | F
//F -> A '?' | A 
//A -> C | '(' E ')'
//C -> 0-9, a-z, .
case class E(l: Tree, r: Tree) extends Tree
case class T(l: Tree, r: Tree) extends Tree
case class F(t: Tree, q: Option[String]) extends Tree
case class A(t: Tree) extends Tree
case class C(Terminal: Char) extends Tree
case class NumberCase(Terminal: Int) extends Tree
case class EOS() extends Tree
case class PeriodCase(Terminal: String) extends Tree


object MyParser extends RegexParsers {
 
    def TParser: Parser[Tree] = FParser ~ TParser ^^ {case fvar ~ tvar => T(fvar, tvar)}  | FParser ^^ {fvar => T(fvar, EOS())}

    def FParser: Parser[Tree] = AParser ~ "?" ^^ {case avar ~ _ => F(avar, Some("?"))} | AParser ^^ {avar => F(avar, None)}

    def AParser: Parser[Tree] = "(" ~ EParser ~ ")" ^^ {case _ ~ evar ~ _ => A(evar)} | CParser ^^ {cvar => A(cvar)}  

    def EParser: Parser[Tree] = TParser ~ "|" ~ EParser ^^ {case tvar ~ _ ~ evar => E(tvar, evar)} | TParser ^^ {tvar => E(tvar, EOS())}

    def CParser: Parser[Tree] = "." ^^ {_ => PeriodCase(".")} | "^[A-Za-z]".r ^^ {myC => C(myC.charAt(0))} | "^[0-9]".r ^^ {myNum => NumberCase(myNum.toInt)}
    
     
}
object Main {
    var neededExp: E = E(EOS(), EOS())
    //var somebool: Boolean = false
    def evaluate(tree1: Tree, tree2: Tree): Boolean = {
        (tree1, tree2) match{
            case (E(exp1, exp2), E(exp3, exp4)) => if(exp2.isInstanceOf[EOS]){
                
                neededExp = E(exp3, exp4)
                evaluate(exp1, exp3)
            } else {
                
                neededExp = E(exp3, exp4)
                if(evaluate(exp1, exp3) == true && evaluate(exp2, E(exp3, exp4)) == true){
                    println("hehexd")
                    return false
                } else {
                    evaluate(exp1, exp3) | evaluate(exp2, E(exp3, EOS()))
                }
                                
            }
            case (T(exp1, exp2), T(exp3, exp4)) => if(!exp2.isInstanceOf[EOS] && !exp4.isInstanceOf[EOS]){
                if(exp4.isInstanceOf[EOS]){
                    println("killmeplz")
                    return false
                } else {
                if(evaluate(exp1, exp3) == true){
                    //println("should hit here")
                    evaluate(exp2, exp4)
                } else {
                    evaluate(exp1, exp3)
                }
            }
            } else if(exp2.isInstanceOf[EOS] && exp4.isInstanceOf[EOS]){
                // final objectt in tree/string
                evaluate(exp1, exp3)
            } else {
                if(!exp2.isInstanceOf[EOS] && exp4.isInstanceOf[EOS]){
                    evaluate(exp2, T(exp3, exp4))
                    // always false, GET PRANKED XD
                    //return false
                } else {
                    if(exp2.isInstanceOf[EOS] && !exp4.isInstanceOf[EOS]){
                       //println("pretty sure this is a problem" + exp1 + exp2 + exp3 + exp4) 
                        evaluate(exp1, neededExp)
                    } else {
                        println("??????")
                        return false
                    }
                }
               
            }
            case (F(exp1, exp2), F(exp3, exp4)) => if(exp2 != None){
                return true
           } else {
             evaluate(exp1, exp3)
            }
            
            case (A(exp1), A(exp2)) => if(exp1.isInstanceOf[E]){ //maybe need to check if one is of type E
                //We know something is encapsulated inside of a paren
                 
                //println(neededExp + " <------ needed expression" + "Other expression: " + exp1)
                evaluate(exp1, neededExp)
             }  else {
                //println("yeetyboi " + " A exp1: " + exp1 + " A exp 2 " + exp2)
                evaluate(exp1, exp2)
             }
            case (C(char1), C(char2)) => if(char1.equals(char2)){
                return true
            } else {
                return false
            }
            case (NumberCase(num1), NumberCase(num2)) => if(num1 == num2){
                return true
            } else {
                return false
            }
            case (PeriodCase(period), C(charryboi)) => return true
            case (A(exp1), C(char)) => evaluate(exp1, (C(char)))
            case (C(char), F(exp1, None)) => evaluate(A(C(char)), exp1)
            case (A(exp1), NumberCase(num)) => evaluate(exp1, NumberCase(num))
            case (NumberCase(num), F(exp1, None)) => evaluate(A(NumberCase(num)), exp1)
            case (C(char), NumberCase(num)) => return false
            case (NumberCase(num), C(char)) => return false
            case (PeriodCase(_), NumberCase(_)) => return false
            case (C(char), T(something, _)) => evaluate(T(F(A(C(char)), None), EOS()), T(something, EOS()))
            case (NumberCase(num), T(something, _)) => evaluate(T(F(A(NumberCase(num)), None), EOS()), T(something, EOS()))
            case (F(something, _), T(something1, _)) => evaluate(F(something, None), something1)
            case (F(exp1, _), E(exp2, exp3)) => evaluate(F(exp1, None), F(A(E(exp2, exp3)), None))
            case (C(char), E(exp1, exp2)) =>  if(exp2.isInstanceOf[EOS]){
                evaluate(C(char), exp1)
             } else {
                 return false;
             } 
            case (E(T(_, _), E(T(_, _),E(exp1, exp2))), E(exp3, exp4)) => evaluate(E(exp1, exp2), E(exp3, exp4))
            case (NumberCase(_), E(_, _)) => return false
            
        }
    }
    def main(args: Array[String]): Unit = {
        print("pattern? ")
        val input = scala.io.StdIn.readLine()
        val firstTree = MyParser.parse(MyParser.EParser, input)
        while(true){
                // parse
                print("string? ")
                val stringput = scala.io.StdIn.readLine()
                val secondTree = MyParser.parse(MyParser.EParser, stringput)
                (firstTree, secondTree) match {
                    case (MyParser.Success(tree1, _), MyParser.Success(tree2, _)) => 
                     val jeff = evaluate(tree1, tree2)
                     if(jeff == true){
                        println("match")
                    } else {
                        println("no match")
                    }
                    case _ => println("no match")
                  
                   
                }            
            
        }
    }
}