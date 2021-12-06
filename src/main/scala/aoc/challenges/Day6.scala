package aoc.challenges

class Day6 extends Challenge {

  override def part1(input: String): String = {
    val ITERATIONS = 80
    val result = evolvePopulation(input, ITERATIONS)
    result.toString
  }

  override def part2(input: String): String = {
    val ITERATIONS = 256
    val result = evolvePopulation(input, ITERATIONS)
    result.toString
  }

  private def evolvePopulation(input: String, iterations: Int): Long = {
    var populationBuckets = Array.fill(7)(0L)
    var newFishBuckets = Array.fill(2)(0L)
    input.split(",").map(_.toInt).foreach(v => populationBuckets(v) += 1)

    for (_ <- Range(0, iterations)) {
      val newFishToSpawn = populationBuckets(0)
      var previousVal = populationBuckets.last
      populationBuckets(populationBuckets.size - 1) = populationBuckets.head
      for (i <- Range(0, populationBuckets.size - 1).reverse) {
        val saved = populationBuckets(i)
        populationBuckets(i) = previousVal
        previousVal = saved
      }

      populationBuckets(populationBuckets.size - 1) += newFishBuckets.head
      newFishBuckets(0) = newFishBuckets.last
      newFishBuckets(newFishBuckets.size - 1) = newFishToSpawn
    }
    populationBuckets.sum + newFishBuckets.sum
  }
}
