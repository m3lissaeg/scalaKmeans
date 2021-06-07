object PointsPupulation{

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
            println( a(i) + " ")     
      }
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
  val popuS = 5
  val limit = 100
  val pointD = 10
  var pointsP = pointsPopulation(popuS, limit, pointD)

  def main(args: Array[String]): Unit = {
      printMatrixDouble(pointsP)
  }
}