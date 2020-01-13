package part1

object GeneralRecap extends App {
  val aCondition: Boolean = false;
  var aVariable= 42;
  aVariable+=1

  val aConditionedVal = if(aCondition) 42 else 65

  val aCodeBlock = {

    if(aCondition) 74
    56

  }

  val theUnit = println("hello, Scala")

  def aFunction(x: Int) = x + 1

  println(aFunction(2))
  def factorial (n: Int, acc: Int) : Int =
    if(n<=0) acc
    else factorial(n-1, acc * n)

  class Animal
  class Dog extends Animal
  val aDog: Animal = new Dog

  trait Carnivore {
    def eat(a :Animal): Unit
  }

  class Crocodile extends Animal with Carnivore{
    override def eat(a: Animal): Unit = println("crunch")
  }


  val aCroc = new Crocodile

  aCroc eat aDog

  val aCarnivore = new Carnivore {
    override def eat(a: Animal): Unit = println("roar")
  }

  aCarnivore eat aDog

  abstract class MyList[+A]

  object MyList

  case class Person(name: String, age: Int)

  val aPotentialfailuere = try {
    throw new RuntimeException("whatr?")
  }catch {
    case e: Exception=> "waht"
  }
  finally {
    println("lalalala")
  }


  val incrementor = new Function1[Int,Int] {
    override def apply(v1: Int): Int = v1 + 1
  }

  val increm = incrementor(42)

  val anonymusIncrm = (x:Int)=> x + 1

  List(1,2,3).map(incrementor)

  val pairs = for {
    num <- List(1,2,3,4)
    char <- List('a','b')
  } yield num + "_" + char
  print(pairs)
  val anOption = Some(2)

  val aTry = try{
    //throw new RuntimeException()
  }

  val unknown = 2
  val order = unknown match {
    case 1 => "first"
    case 2 => "second"
    case _ => "unknown"
  }

  val bob = Person("Bob",22)
  val greeting = bob match {
    case Person(n, _) => s" hi, my name is $n"
    case _ => "sdf"
  }

  print(greeting)




}
