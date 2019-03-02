import java.io._
//260677676 Dan Seol
//MATH 318 Assignment 1
//Question 4

object Relation {

  // Composition of relations for two given relations as sets
  def composition(S:  scala.collection.mutable.Set[Vector[Int]],T:  scala.collection.mutable.Set[Vector[Int]]):  scala.collection.mutable.Set[Vector[Int]] ={
    val mySeq1 = S.toSeq
    val mySeq2 = T.toSeq
    val U = scala.collection.mutable.Set[Vector[Int]]()
    var i = 0;
    var j=  0;
    for( i <- 0 until mySeq1.size){
      for( j <- 0 until mySeq2.size){
        if(mySeq1(i)(1) == mySeq2(j)(0)){
        U += Vector(mySeq1(i)(0),mySeq2(j)(1))
      }
      }
    }
    return U
  }
  def inverseR(S: scala.collection.mutable.Set[Vector[Int]]): scala.collection.mutable.Set[Vector[Int]] ={
    val mySeq1 = S.toSeq
    val U = scala.collection.mutable.Set[Vector[Int]]()
    for (i <- 0 until mySeq1.size){
      U += Vector(mySeq1(i)(1), mySeq1(i)(0))
    }
    return U
  }

  def main(args:Array[String]){

    //pairs of coordinates as 2D vectors
val pair1: Vector[Int] = Vector(1,2)
val pair2: Vector[Int] = Vector(2,3)
val pair3: Vector[Int] = Vector(3,1)
val pair4: Vector[Int] = Vector(4,4)


//Relation given for the problem
val R =  scala.collection.mutable.Set(pair1, pair2, pair3, pair4)
println("Our initial set R is composed of"+R)
// R composition R
val R2 = composition(R, R)

println("One composition returned "+R2)

//An array of R^n's
var ArrayofSets = new Array[scala.collection.mutable.Set[Vector[Int]]](50)
ArrayofSets(0) = R;
ArrayofSets(1) = R2;

//The n-th entry of this array will be R^n
for(i <- 2 until ArrayofSets.size){
  ArrayofSets(i) = composition(R, ArrayofSets(i-1))
}
// V will be my approximation for trcl(R)
var V: scala.collection.mutable.Set[Vector[Int]] = R
// W will be a set to check whether my number of iteration was enough to get trcl(R)
var W:  scala.collection.mutable.Set[Vector[Int]] = composition(R, ArrayofSets(49))
for(i <- 0 until ArrayofSets.size){
  V = V | ArrayofSets(i)
}

//\Union_{i=1}^50{R^i}
println("After 50 iterations, the finite subset of transitive closure for relation R is:"+V)
W = V | W
//Is \Union_{i=1}^50{R^i} == \Union_{i=1}^51{R^i}?
println("51th iteration -- same sets? " + (V sameElements W) )


//Is it reflexive?
val Reflexive =  scala.collection.mutable.Set(Vector(1,1), Vector(2,2), Vector(3,3), Vector(4,4))
println("Is our trcl(R) reflexive? "+ (Reflexive subsetOf V))
//Is it symmetric?
val Symmetric = scala.collection.mutable.Set(Vector(1,2), Vector(1,3), Vector(2,3))

val U: scala.collection.mutable.Set[Vector[Int]] = Symmetric | inverseR(Symmetric)
println("Is our trcl(R) symmetric? "+ (U subsetOf V))
}

//and by Question5  it should be transitive.
//Thus this is an equivalence relation.
//Part 2: {1,2,3} and {4}
}
