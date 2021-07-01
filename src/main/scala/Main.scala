import scala.util.Try

object Main extends App {
  println("Hello, World!")

// check if a number is even
 //def isEven(x:Int=>Int):Boolean= x%2==0


 
 val h=12==9//if(x%2==0) true else false//  x%2==0
// write this function  takes in an interger and takes whether it is even or not
  val isEvenVal= (x:Int)=>x%2==0

  //objects have fields and methods
  //function is an object
  //
  isEvenVal.apply(12)//isEvenVal(12)

  
  //val isEvenVal2= (x:Int)=

  //val isEvenDefToVal1= isEven _ 

  //List(12,2).map()
 

  val isEvenAnotherWay=  new Function1[Int,Boolean]{
    override def apply(x: Int): Boolean = x%2==0
  }


// model an address which has a street number, housenumber,and flatnumber
// model an entity that can be either a Person with  firstname, lastname and list of addresses
// or company with name,and headquarter(address)


case class Address(streetNumber:Int,houseNumber:Int,flatNumber:Int)

sealed trait Entity
case class Person(firstName:String,lastName:String, addresses:List[Address])  extends Entity

case class  Company(name:String, headquarter:Address) extends Entity


  // represent a currency with values 
  // EUR, USD,CAD, GBP


 sealed trait Currency
case object USD extends Currency
case object GBP extends Currency
case object CAD extends Currency
case object EUR extends Currency
case object FCFA extends Currency

def convertToCFA(currency:Currency):Int= currency match {
  case USD => 580
  case GBP =>600
  case CAD =>500
  case EUR =>590
 case FCFA => 7 

}

def convertToCFAIf(currency:Currency):Int={
  if(currency==USD) 580
 else if(currency==EUR) 590
 
 else if(currency==CAD)500 
 else 600
}
  

val result=convertToCFA(USD)


// count the size of the string// 
 //  def size(s:String):Int= ???//s.count(char=>char)

 // count how many letters in a string
   def countLetters(s:String):Int= s.count(char=>char.isLetter)

   countLetters("Hello123")
//count how many digits in a String
   def countDigits(s:String):Int= s.count(x=>x.isDigit)


   def stats(s:String,predicate: Char=>Boolean) :Int= s.count(predicate)

stats("123",x=>x.isDigit)

stats("123",x=>x.isLetter)
stats("123",x=>x.isUpper)
   
// count the size of the string
   //def size1(s:String):Int=   ??? //stats()

 // count how many letters in a string
   def countLetters1(s:String):Int=  stats(s,x=>x.isLetter)
//count how many digits in a String
   def countDigits1(s:String):Int=   stats("hello",x=>x.isDigit)

   def countUpperCaseLetters(s:String) =  stats(s,x=>x.isUpper)

   def isWhitespace(s:String)=stats(s,x=>x.isWhitespace) 




   // represent a mode with values Length or letters,or digits
   sealed trait Mode
   case object Length extends Mode
   case object Letters extends Mode
   case object Digits extends Mode

def predicateSelection(mode:Mode):Char=>Boolean=  mode match {
  case Length =>(x:Char)=>x.isLetter
  case Letters =>(x:Char)=>x.isLetter
  case Digits =>(x:Char)=>x.isDigit
}




val addTen=(x:Int)=>x+10

val square=(x:Int)=>x.toChar

 val combined=addTen.andThen(square)
 combined(12)





   // represent Json data structure

 
 
sealed trait Json
case class JNumber(value:Int) extends Json
case class JString(value:String) extends Json
case class JBoolean(value:Boolean) extends Json

case class JArray(value:List[Json]) extends Json

case class  JObject(value:Map[String,Json]) extends Json

case object JNull extends Json


object JArray{
  def apply(elems:Json*):JArray=JArray(elems.toList)
}
val data:Json= JObject (
Map(
"firstName"->JString("John"),
"lastName"->JString("Smith"),
"isAlive"->JBoolean(true),
"age"->JNumber(27),
"address"->JObject(
  Map(
    "streetAddress"->JString("21 2nd Street"),
    "city"->JString("New York"),
    "state"->JString("NY"),
    "postalCode"->JString("10021-3100")
  )
),
"phoneNumbers"->JArray(
  
JObject(
  Map(
    "type"->JString("home"),
    "number"-> JString("212 555-1234")
  )
),
JObject(
  Map(
    "type"->JString("office"),
    "number"-> JString("646 555-4567")
  )
),
JObject(
  Map(
    "type"->JString("factory"),
    "number"-> JString("646 555-4567")
  )
),
JString("Yaounde"),
JNumber(89)

//close the array here
),
"children"->JArray(List.empty[Json]),
"spouse"->JNull

)


)


/*
{
  "firstName": "John",
  "lastName": "Smith",
  "isAlive": true,
  "age": 27,
  "address": {
    "streetAddress": "21 2nd Street",
    "city": "New York",
    "state": "NY",
    "postalCode": "10021-3100"
  },
  "phoneNumbers": [
    {
      "type": "home",
      "number": "212 555-1234"
    },
    {
      "type": "office",
      "number": "646 555-4567"
    }
  ],
  "children": [],
  "spouse": null
}



**/






         // println(data)






          val test= JNumber(13)
 val test2= JArray(JString("This is a list"),JNumber(1333),JNull)

   def asString(data:Json):String = data match {
     case JNumber(value) => value.toString()
     case JString(value) => value
     case JBoolean(value) =>value.toString()
     case JArray(value) =>value.toString()
     case JObject(value) =>value.toString()
     case JNull =>null.toString()
   }


//println(asString(data))

  def isContainNegative(data:Json):Boolean=  data match {
    case JNumber(value)=> if (value<0 )   true  else false
    case JString(value)  => Try(value.toInt).fold(_=>false,_=>true)
    
    case JBoolean(value) =>false
    case JArray(value) => if(value.map(json=>isContainNegative(json)).filter(x=>x==true).length==0) false else true
    case JObject(value) =>if(value.toList.map(pair=>isContainNegative(pair._2)).filter(x=>x==true).length==0) false else true
    case JNull =>false
  }

val test3= JString("-23")


println(isContainNegative(test3))







        }







