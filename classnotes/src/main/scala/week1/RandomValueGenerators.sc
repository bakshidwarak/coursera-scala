trait Generator[+T] {

  self=>


  def generate: T


  // instead of creating anonymous class for each we should be able to write it in a for expression
  // To do so we should define map, flatMap and filter functions in the generator
  def map[S](f:T=>S) : Generator[S]= new Generator[S] {
    override def generate = f(self.generate)
  }

  def flatMap[S](f:T=>Generator[S])= new Generator[S] {
    override def generate: S = f(self.generate).generate
  }
}

val integers= new Generator[Int] {
  val rand= new java.util.Random
  def generate=rand.nextInt()
}

val booleans= new Generator[Boolean] {
  def generate=integers.generate >0
}

def newbooleans= integers map ( x=>x>0)

val pairs= new Generator[(Int,Int)] {
  override def generate: (Int, Int) = (integers.generate,integers.generate)
}


trait Tree

case class Inner(left:Tree, right:Tree) extends Tree
case class Leaf(value:Int) extends Tree

def leafs : Generator[Leaf]={
  for(x<-integers) yield Leaf(x)
}

def inner: Generator[Inner]= for {
  l <- trees
  r <- trees
}yield Inner(l,r)



def trees:Generator[Tree]= for {
    isLeaf <- booleans
    tree <- if(isLeaf) leafs else inner
} yield tree


trees.generate


def streamRange(lo:Int,hi:Int):Stream[Int]={
  print(lo+ " ")
  if(lo>=hi) Stream.empty
  else Stream.cons(lo,streamRange(lo+1,hi))
}


streamRange(1,10).take(3).toList


def expr = {
  val x={ print("x");1}
  lazy val y= { print("y");2}
  def z={
    print("z");3
  }
  z+y+x+z+y+x //zyxzx
}

expr


def from(n:Int):Stream[Int]=n #:: from(n+1)

val natural=from(0)
val m4s= natural map (_*4)

(m4s take 100).toList


//Sieve of Eratosthenes

def sieve(s:Stream[Int]): Stream[Int]= s.head #:: sieve(s.tail filter(_%s.head !=0))

val primes=sieve(from(2))

primes.take(15).toList



def sqrtStream(x:Double) : Stream[Double]={
  def improve(guess:Double)=(guess+x/guess)/2
  lazy val guesses :Stream[Double]=1 #:: (guesses map improve)


  guesses
}

def isGoodEnough(guess:Double, x:Double)=math.abs((guess*guess-x)) < 0.0001

sqrtStream(4).filter(isGoodEnough(_,4.0)).take(10).toList
