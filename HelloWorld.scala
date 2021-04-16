// K-means clustering is a clustering algorithm that aims to partition n observations into k clusters.
    // There are 3 steps:
    // Initialization – K initial “means” (centroids) are generated at random
    // Assignment – K clusters are created by associating each observation with the nearest centroid
    // Update – The centroid of the clusters becomes the new mean

// Initialization stage

object HelloWorld {
 
  def generateRandomArray(size: Int, limit: Int ): Array[Int]={
      
    var randomArray = new Array[Int](0)
      // Define random generator
      val r = scala.util.Random
      for (i <- 1 to size){
        var number = r.nextInt(limit)
        //println(number)
        randomArray = randomArray:+ number
      }
     randomArray   

   }


  def main(args: Array[String]): Unit = {
    //10 elements, numbers (1, 100)
    val size = 10
    val limit = 100
    var pointP = generateRandomArray(size, limit)
    
    // Return the generated array:
    println(pointP.mkString(" "))
  }

}
        