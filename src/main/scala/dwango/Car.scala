package dwango

/**
  * Created by wada.yusuke on 2016/10/20.
  */
class Car(val engine: Int, val tire: String) {

  private def toStirng(): String = s"Engine is $engine, Tire is $tire"

  private[this] def getCompany(): String = "Classmethod, Inc"

}

object Car {

  def print(): Unit = println(new Car(65, "bridgewater").toStirng())

  //  def company(): Unit = println(getCompany) // コンパイルエラー

}