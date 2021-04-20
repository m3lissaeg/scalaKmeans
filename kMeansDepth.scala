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

   def partialDistance(p: Array[Int],  q: Array[Int], s: Int, f: Int, minLength: Int, depth: Int, maxDepth: Int): Double = {
    var dist = 0.0
    var i = s
    // l = Length of the interval 
    l = f - s

    if( depth >= maxDepth){
        // Sequential
        while(i< f){
            val d = p(i) - q(i)
            dist = d*d
            i = i + 1
        }  // return dist
        dist 
    }else{
        // Concurrent using recursivity
        val middle = s + (f-s)/2
        var firstMiddle = partialDistance(p, q, s, m)
        var secondMiddle = partialDistance(p, q, m, f)
        val (x, y) = parallel(partialDistance(p,q, s, m, depth+1, maxDepth), partialDistance(p, q, m, f, depth+1, maxDepth) )
        x+y
    }
   }

   def euclDistance( p: Array[Int],  q: Array[Int] ): Double = {
       partialDistance(p, q, 0, p.length, 0, 5)    
   }


  def main(args: Array[String]): Unit = {
    //10 elements, numbers (1, 100)
    val size = 10
    val limit = 100
    var pointP = generateRandomArray(size, limit)
    // println(pointP.mkString(" "))


    
  }

}
        