import scala.collection.mutable.ArrayBuffer, scala.util.DynamicVariable, scala.io.Source

object KMeansSerialMap{
   val popuS = 300
   val limit = 100
   val pointD = 4
   val k = 3

    def pointToString(p: Array[Double]): String ={
    val pf = p.mkString(", ")
    pf
  }

  def pointsPopulation(): Array[ Array[Double]]={
      var aux = Array.ofDim[Double](popuS, pointD)
      val random = scala.util.Random
      var matrixOfPoints = aux.map(r => r.map(c => random.nextInt(limit).toDouble))
      matrixOfPoints
  }    

  def euclDistance( p: Array[Double],  q: Array[Double] ): Double = {
      var dist = 0.0
      for((pi, qi) <- p zip q){
        val d = pi - qi
        dist += (d*d)
      }
      math.sqrt(dist)
  }

  def nearestCentroid(p: Array[Double], c: Array[Array[Double]]): Int = {
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
    nearestCentroid
  }

  def chooseCentroids(matrixOfPoints:Array[ Array[Double]] ): Array[ Array[Double]]={
    var matrixOfCentroids = Array.ofDim[Double](k, pointD)
    val r = scala.util.Random
    matrixOfCentroids.view.zipWithIndex.foreach{
      case (v,i) => {
        var n = r.nextInt(matrixOfPoints.length)
        var centroid = matrixOfPoints(n)
        matrixOfCentroids(i) = centroid  
      }       
    }
    matrixOfCentroids
  }
  
  def calculateMatrixMean( m: Array[Array[Double]] ): Array[Double]={
    var acumCoord = Array.ofDim[Double](pointD)
    var pointsBelongToCluster = m.length

    for( i <- 0 to pointsBelongToCluster -1 ){
      acumCoord.view.zipWithIndex.foreach{
        case (v,j) => acumCoord(j) += m(i)(j)
      }
    }
    var newCe = acumCoord.map(x => x/pointsBelongToCluster.toDouble)
    newCe
  }

  def updateCentroidsMatrix( cl: Map[Int, Array[Array[Double]]] ): Array[Array[Double]]={
    var newCentroids = Array.ofDim[Double](k, pointD)
    newCentroids.view.zipWithIndex.foreach{
      case (v,i) => newCentroids(i) = calculateMatrixMean(cl(i))         
    }
    newCentroids
  }

  def error(c: Array[Double], d: Array[Array[Double]]): Double={
    var e = d.map(s => euclDistance(c, s) )
    e.fold(0.0)((a, b) => a + b) / d.length.toDouble
  }

  def calculateError( cl: Map[Int, Array[Array[Double]]] , centroids: Array[Array[Double]]): Double ={
    val sse = cl.map(b => error(centroids(b._1), b._2))
    sse.fold(0.0)((a, b) => a + b)    
  }

  def main()(args: Array[String]): Unit = {
    //    val data = csvReader.readIris()
    var sseAnt = 0.0
    val epsilon = 0.00000001
    var sse = 10.0
    var data = pointsPopulation()
    var centroids = chooseCentroids(data)
    println("Centroids:")
    centroids.foreach(p => println(pointToString(p)))
    println("------------------")
    var clusters = data.groupBy(x => nearestCentroid(x, centroids))

    while((sseAnt-sse).abs > epsilon){
      sse = calculateError(clusters, centroids)
      centroids = updateCentroidsMatrix(clusters)
      println("New Centroids:")
      centroids.foreach(p => println(pointToString(p)))
      clusters = data.groupBy(x => nearestCentroid(x, centroids))

      sseAnt = sse
      sse = calculateError(clusters, centroids)
      println("Error:" + sse)
    }
  }

}