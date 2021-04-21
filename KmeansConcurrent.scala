import scala.collection.mutable.ArrayBuffer
// K-means clustering is a clustering algorithm that aims to partition n observations into k clusters.
    // There are 3 steps:
    // Initialization – K initial “means” (centroids) are generated at random
    // Assignment – K clusters are created by associating each observation with the nearest centroid
    // Update – The centroid of the clusters becomes the new mean

// Initialization stage

object KmeansConcurrent {
  def printMatrix(matrix: Array[ Array[Int]] , pointDimension: Int ): Unit={
    for (i <- 0 to matrix.length -1){
      for ( j<-0 until pointDimension-1){
           print( matrix(i)(j) + " ")     
          }
          println      
    }
  }
  def printMatrixBuffer(matrix: ArrayBuffer[ Array[Int]] , pointDimension: Int ): Unit={
    for (i <- 0 to matrix.length -1){
      for ( j<-0 until pointDimension-1){
           print( matrix(i)(j) + " ")     
          }
          println      
    }
  }

  def printBufferTuple(matrix: ArrayBuffer[(Int, Double)] ): Unit={
    for (i <- 0 to matrix.length -1){
      println(matrix(i)._1 + ","+ matrix(i)._2)  
    }
  }
 
  def generateRandomArray(pointDimension: Int, limit: Int ): Array[Int]={      
    var randomArray = new Array[Int](0)
      // Define random generator
      val r = scala.util.Random
      for (i <- 1 to pointDimension){
        var number = r.nextInt(limit)
        //println(number)
        randomArray = randomArray:+ number
      }
     randomArray   
  }

  def pointsPopulation(populationSize: Int, limit: Int, pointDimension: Int  ): Array[ Array[Int]]={
       var matrixOfPoints = Array.ofDim[Int](populationSize, pointDimension)
      val r = scala.util.Random

       for (i <- 0 to populationSize -1){
         for (j <- 0 to pointDimension -1){
            var number = r.nextInt(limit)
            matrixOfPoints(i)(j) =  number

         }
      }
      matrixOfPoints
  }    

  def chooseCentroids(matrixOfPoints:Array[ Array[Int]], k: Int, pointDimension:Int): Array[ Array[Int]]={
    var matrixOfCentroids = Array.ofDim[Int](k, pointDimension)
      val r = scala.util.Random
      for (i <- 0 to k-1){
        var n = r.nextInt(matrixOfPoints.length)
        var centroid = matrixOfPoints(n)
        matrixOfCentroids(i) = centroid      
      }
      matrixOfCentroids
  }


   def partialDistance(p: Array[Int],  q: Array[Int], s: Int, f: Int, minLength: Int): Double = {
    var dist = 0.0
    var i = s
    // l = Length of the interval 
    var l = f - s
    // minLength is the limit to decide whether exec serial or concurrent
    if(l < minLength){
        // Serial
        while(i< f){
            val d = p(i) - q(i)
            dist = d*d
            i = i + 1
        }  // return dist
        dist 
    }else{
        // Concurrent using recursivity
        val middle = s + (f-s)/2
        var firstMiddle = partialDistance(p, q, s, middle, 10)
        var secondMiddle = partialDistance(p, q, middle, f, 10)
        var r = firstMiddle + secondMiddle
        // return r
        r         
        // or
        // val (x, y) = parallel(partialDistance(p,q, s, middle), partialDistance(p, q, middle, f) )
        // x+y
    }
   }

   def euclDistance( p: Array[Int],  q: Array[Int] ): Double = {
       partialDistance(p, q, 0, p.length, 10)    
   }

   def nearestCentroid(p: Array[Int], c: Array[Array[Int]]): (Int, Double) = {
    //    p : punto, c: centroides  
    var nearestDistance = euclDistance(p, c(0))
    var nearestCentroid = 0
    var i = 1

    while (i < c.length){
        val di = euclDistance(p, c(i))
        if (di < nearestDistance){
            nearestDistance = di
            nearestCentroid = i
        }
        i = i+1
    }
    (nearestCentroid, nearestDistance)
   }


  def main(args: Array[String]): Unit = {
    //10 elements, numbers (1, 100)
    val popuS = 15
    val limit = 100
    val pointD = 10
    val k = 3
    var pointsP = pointsPopulation(popuS, limit, pointD  )
    val centroidsMatrix = chooseCentroids(pointsP, k, pointD)
    var nearestToCentroidClassification =  ArrayBuffer[(Int, Double)]()
    var nearestCentroidDistanceAcum = Array.ofDim[Double](k)
    var howManyPointsBelongToCentroid = Array.ofDim[Int](k)
    var averageDistanceToCentroid = Array.ofDim[Double](k)
    
    println(" Matrix of points")
    printMatrix(pointsP, pointD)
    println(" Matrix of centroids")
    printMatrix(centroidsMatrix, pointD)
    // calculate the nearest centroid for each point in pointsPopulation (pointsP)
    for (i <- 0 to pointsP.length -1){
      var neCe = nearestCentroid( pointsP(i), centroidsMatrix)
      println(neCe)
      nearestToCentroidClassification += neCe
    }
      // printBufferTuple(nearestToCentroidClassification)
     for( i <- 0 to nearestToCentroidClassification.length -1 ){
       var j = nearestToCentroidClassification(i)._1 // Obtengo la posicion en la que debo sumar
       nearestCentroidDistanceAcum(j) = nearestCentroidDistanceAcum(j) + nearestToCentroidClassification(i)._2
       howManyPointsBelongToCentroid(j) = howManyPointsBelongToCentroid(j) + 1
     }
     println(nearestCentroidDistanceAcum.mkString(" "))
     println(howManyPointsBelongToCentroid.mkString(" "))

    // Calculate the average - error average
    for( i <- 0 to nearestCentroidDistanceAcum.length -1 ){
     averageDistanceToCentroid(i) = nearestCentroidDistanceAcum(i) / howManyPointsBelongToCentroid(i).toDouble
    }

     
  }

}
        