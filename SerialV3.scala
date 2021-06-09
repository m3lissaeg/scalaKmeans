import scala.collection.mutable.ArrayBuffer
import java.util.concurrent.ForkJoinPool;
import java.util.concurrent.ForkJoinTask;
import java.util.concurrent.RecursiveTask;
import java.util.concurrent.ForkJoinWorkerThread
import scala.io.Source

import scala.util.DynamicVariable
// K-means clustering is a clustering algorithm that aims to partition n observations into k clusters.
    // There are 3 steps:
    // Initialization – K initial “means” (centroids) are generated at random
    // Assignment – K clusters are created by associating each observation with the nearest centroid
    // Update – The centroid of the clusters becomes the new mean

// Initialization stage
object SerialV3 {

  // K-means 
  def printMatrixDouble(matrix: Array[ Array[Double]]  ): Unit={
    for (i <- 0 to matrix.length -1){
      for ( j<-0 until pointD -1){
           print( matrix(i)(j) + " ")     
          }
          println      
    }
  }

  def printArrayDouble(a: Array[ Double]  ): Unit={
    for (i <- 0 to a.length -1){
      print( a(i) + " ")  
    }
  }
  
  def printArrayInt(a: Array[Int]  ): Unit={
    for (i <- 0 to a.length -1){
      print( a(i) + " ")  
    }
  }


  def printMatrixBuffer(matrix: ArrayBuffer[ Array[Int]] ): Unit={
    for (i <- 0 to matrix.length -1){
      for ( j<-0 until pointD-1){
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
 
  def divideArrays(a: Array[Double], b: Array[Int]): Array[Double] ={
    var r =Array.ofDim[Double](a.length)
     for( i <- 0 to a.length -1 ){
     r(i) = a(i) / b(i).toDouble
    }
    r
  }

  def divideArrayByInt(a: Array[Double], b: Int): Array[Double]={
    var r = a.map(_ /b.toDouble)
    r
  }

  def pointsPopulation(populationSize: Int, limit: Int, pointDimension: Int  ): Array[ Array[Double]]={
      var matrixOfPoints = Array.ofDim[Double](populationSize, pointDimension)
      var i = 0
      val bufferedSource = Source.fromFile("dataset.txt")

      for (line <- bufferedSource.getLines) {
        matrixOfPoints(i) = line.split(" ").map(_.toDouble).toArray
        i +=1
      }
      bufferedSource.close
      
      matrixOfPoints
  }    

  def chooseCentroids(matrixOfPoints:Array[ Array[Double]] ): Array[ Array[Double]]={
    var matrixOfCentroids = Array.ofDim[Double](k, pointD)
      val r = scala.util.Random
      for (i <- 0 to k-1){
        var n = r.nextInt(matrixOfPoints.length)
        var centroid = matrixOfPoints(n)
        matrixOfCentroids(i) = centroid      
      }
      matrixOfCentroids
  }

   def euclDistance( p: Array[Double],  q: Array[Double] ): Double = {
       var dist = 0.0
       for( i <- 0 to p.length -1 ){
           val d = p(i) - q(i)
            dist += d*d
       }
       math.sqrt(dist)
   }

   def classificate(p: Array[Double], c: Array[Array[Double]]): (Int, Double)={
       // c: centroids matrix
       // p : point to be classificated

        var nearestDistance = euclDistance(p, c(0))
        var nearestCentroid = 0
        var i = 1

        while(i < c.length){
            val di = euclDistance(p, c(i))
            if (di < nearestDistance){
                nearestDistance = di
                nearestCentroid = i
            }
            i = i+1
        }
        (nearestCentroid, nearestDistance)

   }

   def partialClassificate(c: Array[Array[Double]], s: Int, e: Int, minLength: Int): ArrayBuffer[(Int, Double)] = {
       // c : matrix of centroids 
    var dist = 0.0
    var i = s
    // l = Length of the interval 
    var l = e - s

    // minLength is the limit to decide whether exec serial or concurrent
    if(l < minLength){
        // Serial
        var acum = ArrayBuffer[(Int, Double)]()
        while(i< e){
            var classif = classificate(pointsP(i), c)
            acum += classif
            i += 1
        }
        acum  
         
    }else{
        // Using recursivity
        val middle = s + (e-s)/2
        var x = partialClassificate(c, s, middle, minLength)
        var y = partialClassificate(c, middle+1, e, minLength)
        var z = ArrayBuffer[(Int, Double)]()
        var t = 0
        while(t< x.length){
            z += x(t)
            t += 1
        }
        var j = 0
        while(j< y.length){
            z += y(j)
            j += 1
        }
        z

    }
   }


   def nearestToCentroidClassificationF(centroidsMatrix: Array[ Array[Double]]): ArrayBuffer[(Int, Double)]={
    //    centroidsMatrix : matriz de centroides 
    var neCeClass = partialClassificate(centroidsMatrix, 0, pointsP.length, 20)
    neCeClass
   }


  // Esta funcion actualiza los valores para los centroides. Devuleve una matriz de centroides
  def updateCentroidsMatrix(howManyPointsBelongToCentroid: Array[Int], nearestToCentroidClassification: ArrayBuffer[(Int, Double)] ): Array[ Array[Double]]  ={
    var newCentroids = Array.ofDim[Double](k, pointD-1)
    var acumCoord = Array.ofDim[Double](k, pointD-1)

    println("Pasa por aqui")
    for( i <- 0 to pointsP.length -2){
      var j = nearestToCentroidClassification(i)._1 // a que cluster pertenece el punto

      for( s <- 0 to pointD -2 ){
        acumCoord(j)(s) += pointsP(i)(s).toDouble
      }
    }
    // printMatrixDouble(acumCoord )
    for( i <- 0 to k -1 ){
    newCentroids(i) = divideArrayByInt(acumCoord(i), howManyPointsBelongToCentroid(i)) 
    }
    newCentroids
  }


  def totalError( nearestCentroidDistanceAcum: Array[Double] , howManyPointsBelongToCentroid:  Array[Int] ): Double={
    var acum = 0.0
    var averageDistanceToCentroid = divideArrays( nearestCentroidDistanceAcum, howManyPointsBelongToCentroid ) 
    
    for( i <- 0 to averageDistanceToCentroid.length -1 ){
     acum += averageDistanceToCentroid(i)
    }
    acum
  }

  // Global  variables
    
  val popuS = 20000
  val limit = 100
  val pointD = 20
  val k = 3
  var pointsP = pointsPopulation(popuS, limit, pointD)
  val epsilon = 0.00000001

  def main(args: Array[String]): Unit = {
    var centroidsMatrix = chooseCentroids(pointsP)
    // println(" Matrix of points")
    // printMatrixDouble(pointsP)
    println(" Matrix of centroids")
    // printMatrixDouble(centroidsMatrix)
    // println(pointsP.length)

    var sseAnt = 0.0
    var done = false
    // Iterations  
    while(!done){

      var nearestCentroidDistanceAcum = Array.ofDim[Double](k)
      var howManyPointsBelongToCentroid = Array.ofDim[Int](k)    
      // calculate the nearest centroid for each point in pointsPopulation (pointsP)
      var nearestToCentroidClassification = nearestToCentroidClassificationF(centroidsMatrix)
      // printBufferTuple(nearestToCentroidClassification)
      
      for( i <- 0 to nearestToCentroidClassification.length -1 ){
        var j = nearestToCentroidClassification(i)._1 // Obtengo la posicion en la que debo sumar
        nearestCentroidDistanceAcum(j) += nearestToCentroidClassification(i)._2
        howManyPointsBelongToCentroid(j) += 1
      }
      // println("Acum de dist  mas cercanas al centroide " + nearestCentroidDistanceAcum.mkString(" "))
      // println("Cuantos puntos pertenecen al centroide " + howManyPointsBelongToCentroid.mkString(" "))

      // Calculate the average - error average
      var sse = totalError( nearestCentroidDistanceAcum, howManyPointsBelongToCentroid )
      println("Average error sum")
      println(sse)      

      centroidsMatrix = updateCentroidsMatrix(howManyPointsBelongToCentroid, nearestToCentroidClassification)
      println("New Centroids")
      // printMatrixDouble(centroidsMatrix)
      println("-------------------------------------------------")

      var error = (sse - sseAnt).abs

      if (error < epsilon){
        done = true
      }
      sseAnt = sse
      
    }
  }

}
        