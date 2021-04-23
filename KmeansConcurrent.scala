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

  def printMatrixDouble(matrix: Array[ Array[Double]] , pointDimension: Int ): Unit={
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
  def generateRandomArray(pointDimension: Int, limit: Int ): Array[Double]={      
    var randomArray = new Array[Double](0)
      // Define random generator
      val r = scala.util.Random
      for (i <- 1 to pointDimension){
        var number = r.nextInt(limit).toDouble
        //println(number)
        randomArray = randomArray:+ number
      }
     randomArray   
  }

  def pointsPopulation(populationSize: Int, limit: Int, pointDimension: Int  ): Array[ Array[Double]]={
       var matrixOfPoints = Array.ofDim[Double](populationSize, pointDimension)
      val r = scala.util.Random

       for (i <- 0 to populationSize -1){
         for (j <- 0 to pointDimension -1){
            var number = r.nextInt(limit).toDouble
            matrixOfPoints(i)(j) =  number

         }
      }
      matrixOfPoints
  }    

  def chooseCentroids(matrixOfPoints:Array[ Array[Double]], k: Int, pointDimension:Int): Array[ Array[Double]]={
    var matrixOfCentroids = Array.ofDim[Double](k, pointDimension)
      val r = scala.util.Random
      for (i <- 0 to k-1){
        var n = r.nextInt(matrixOfPoints.length)
        var centroid = matrixOfPoints(n)
        matrixOfCentroids(i) = centroid      
      }
      matrixOfCentroids
  }


   def partialDistance(p: Array[Double],  q: Array[Double], s: Int, f: Int, minLength: Int): Double = {
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

   def euclDistance( p: Array[Double],  q: Array[Double] ): Double = {
       partialDistance(p, q, 0, p.length, 10)    
   }

   def nearestCentroid(p: Array[Double], c: Array[Array[Double]]): (Int, Double) = {
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
  // Esta funcion actualiza los valores para los centroides. Devuleve una matriz de centroides
  def updateCentroidsMatrix(k: Int, howManyPointsBelongToCentroid: Array[Int], nearestToCentroidClassification: ArrayBuffer[(Int, Double)], pointsP: Array[ Array[Double]], pointD: Int): Array[ Array[Double]]  ={
    var newCentroids = Array.ofDim[Double](k, pointD)
    var acumCoord = Array.ofDim[Double](k, pointD)

    for( i <- 0 to pointsP.length -1 ){
      var j = nearestToCentroidClassification(i)._1 // a que cluster pertenece el punto

      for( s <- 0 to pointD -1 ){
        acumCoord(j)(s) = acumCoord(j)(s) + pointsP(i)(s).toDouble
      }
    }
    // println("Coordenadas Sumatoria antes de promedio")
    // printMatrixDouble(acumCoord, pointD)
    for( i <- 0 to k -1 ){
    newCentroids(i) = divideArrayByInt(acumCoord(i), howManyPointsBelongToCentroid(i)) 
    }
    newCentroids
  }

  def nearestToCentroidClassificationF(centroidsMatrix: Array[ Array[Double]]): ArrayBuffer[(Int, Double)]={
    // calculate the nearest centroid for each point in pointsPopulation (pointsP)
    var nearestToCentroidClassification =  ArrayBuffer[(Int, Double)]()
    for (i <- 0 to pointsP.length -1){
      var neCe = nearestCentroid( pointsP(i), centroidsMatrix)
      println(neCe)
      nearestToCentroidClassification += neCe
    }
    nearestToCentroidClassification
  }

  def totalError( nearestCentroidDistanceAcum: Array[Double] , howManyPointsBelongToCentroid:  Array[Int] ): Double={
    var acum = 0.0
    var averageDistanceToCentroid = Array.ofDim[Double](k)
    for( i <- 0 to nearestCentroidDistanceAcum.length -1 ){
     averageDistanceToCentroid = divideArrays( nearestCentroidDistanceAcum, howManyPointsBelongToCentroid ) 
    }
    for( i <- 0 to averageDistanceToCentroid.length -1 ){
     acum = acum + averageDistanceToCentroid(i)
    }
    acum
  }

  // Global  variables
    //10 elements, numbers (1, 100)
    val popuS = 15
    val limit = 100
    val pointD = 10
    val k = 3
    var pointsP = pointsPopulation(popuS, limit, pointD  )
    val epsilon = 2.0

  def main(args: Array[String]): Unit = {
    var centroidsMatrix = chooseCentroids(pointsP, k, pointD)
    var nearestCentroidDistanceAcum = Array.ofDim[Double](k)
    var howManyPointsBelongToCentroid = Array.ofDim[Int](k)    
    println(" Matrix of points")
    printMatrixDouble(pointsP, pointD)
    println(" Matrix of centroids")
    printMatrixDouble(centroidsMatrix, pointD)

    var sseAnt = 0.0
    var done = false
    // Iterations  
    while(!done){
      // calculate the nearest centroid for each point in pointsPopulation (pointsP)
      var nearestToCentroidClassification = nearestToCentroidClassificationF(centroidsMatrix)
        // printBufferTuple(nearestToCentroidClassification)
      for( i <- 0 to nearestToCentroidClassification.length -1 ){
        var j = nearestToCentroidClassification(i)._1 // Obtengo la posicion en la que debo sumar
        nearestCentroidDistanceAcum(j) = nearestCentroidDistanceAcum(j) + nearestToCentroidClassification(i)._2
        howManyPointsBelongToCentroid(j) = howManyPointsBelongToCentroid(j) + 1
      }
      println(nearestCentroidDistanceAcum.mkString(" "))
      println(howManyPointsBelongToCentroid.mkString(" "))

      // Calculate the average - error average
      var sse = totalError( nearestCentroidDistanceAcum, howManyPointsBelongToCentroid )
      println("Average error sum")
      println(sse)      
      centroidsMatrix = updateCentroidsMatrix(k, howManyPointsBelongToCentroid, nearestToCentroidClassification, pointsP, pointD)
      println("New Centroids")
      printMatrixDouble(centroidsMatrix, pointD)
      println("-------------------------------------------------")

      var error = (sse - sseAnt).abs

      if (error < epsilon){
        done = true
      }
      sseAnt = sse
      
    }
  }

}
        