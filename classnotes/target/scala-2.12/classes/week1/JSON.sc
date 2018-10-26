trait JSON

case class JNum(a:Int) extends JSON

case class JBool(a:Boolean) extends JSON

case class JSeq(json:List[JSON]) extends JSON

case class JObj(bindings: Map[String,JSON]) extends JSON

case object JNull extends JSON

case class JStr(a:String) extends JSON

val employee=JObj(Map(
  "firstName"->JStr("Dwarak"),
"lastName"-> JStr("Nath"),
  "nicknames"->JSeq(List(JStr("chucha"),JStr("chuch"))),
  "address"->JObj(Map(
    "address1"->JStr("91 ro dr"),
    "address2"->JStr("apt2"),
    "permanent"->JBool(true),
    "HomePhone"->JNull
  ))
))


def show(obj:JSON): String ={
  obj match {
    case JStr(str)=>"\""+str+"\""
    case JNum(num)=>num.toString
    case JBool(boolval)=>boolval.toString
    case JSeq(list)=> "[" + (list map show mkString ", ") +"]"
    case JObj(inner)=>
      val innerJson=inner map {
      case (k,v)=>"\""+k+"\"" +":"+ show(v)
    }
      "{" + (innerJson mkString ",") + "}"
    case JNull =>"null"
  }
}

println(show(employee))


// Partial Functions

val f: PartialFunction[List[Int],String] = {
  case Nil=>"one"
  case x :: y :: rest=>"two"
}

f.isDefinedAt(List(1,2,3))

val g: PartialFunction[List[Int],String]={
  case Nil=>"One"
  case x::rest=> rest match {
    case Nil => "two"
  }
}

g.isDefinedAt(List(1,2,3))

// Returns true as Partial function only applies to the outer pattern patching block

case class Book(title:String, authors :List[String])

val books=List(
  Book(title="Structure and Interpretation of Computer Programs",authors=List("Abelson,Herald", "Sussman, Gerald")),
  Book(title=" Introduction to Functional Programming", authors=List("Bird, Richard", "Wadler, Phil")),
  Book(title="Effective Java", authors=List("Joshua Bloch")),
  Book(title="Java Puzzlers", authors=List("Joshua Bloch", " Another"))
)

// Find titles of books whose author name is...

for( b <- books;
     author <- b.authors;
     if(author.startsWith("Bird"))
) yield b.title
println("Line 81")
books.flatMap(b=>b.authors.withFilter(au=>au.startsWith("Bird")).map(t=>b.title))
// find all books that have Program in the title

for(b <- books
    if((b.title indexOf "Program") >0))
  yield b.title


// Find the name of all the authors who have atleast two books

for {
  b1 <- books
  b2 <- books
  if b1 != b2
  a1 <- b1.authors
  a2 <- b2.authors
  if a1 == a2
} yield a1



