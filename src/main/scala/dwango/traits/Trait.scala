package dwango.traits

trait A {
  val foo: String
}

trait B {
  //  lazy val bar = foo + "World" // もしくは def bar でもよい

  protected def getBar(foo: String): String = foo + "World"
}

class C extends B with A {
  override val foo = "Hello"


  def printBar(): Unit = println(getBar(foo))
}

