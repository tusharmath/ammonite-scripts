// scala 2.13.3
// ammonite 2.2.0

import scala.util.Random

val FAIRNESS        = 50 // between 0-100
val POPULATION      = 1000_000
val HIGH_PERFORMERS = 1 // between 0-100

case class Person private (skill: Int, luck: Int) {
  // assuming success is a function of skill and luck
  def success: Int = (skill * FAIRNESS + luck * (100 - FAIRNESS)) / 100
}
object Person                                     {
  def apply() = new Person(Random.between(0, 100), Random.between(0, 100))
}

// Create random profiles
val people = (0 to POPULATION).map(_ => Person())

// Calculate average luck
val avgLuck = people.map(_.luck).sum / people.length

// Calculate average skill
val avgSkill = people.map(_.skill).sum / people.length

// Pick top 1% performers
val top10Perf = people.sortBy(-_.success).slice(0, (people.length * HIGH_PERFORMERS / 100).toInt)

// Calculate average luck of successful people
val avgLuckSuccess = top10Perf.map(_.luck).sum / top10Perf.length

// Calculate average skill of successful people
val avgSkillSuccess = top10Perf.map(_.skill).sum / top10Perf.length

// Calculate % of people who were successful but with below average luck
val unluckySuccessfulPeople = top10Perf.filter(_.luck < avgLuck).length * 100 / top10Perf.length

println(s"Avg        Luck: ${avgLuck}%, Skill: ${avgSkill}%")
println(s"Successful Luck: ${avgLuckSuccess}%, Skill: ${avgSkillSuccess}%")
println(s"Unlucky successful population: ${unluckySuccessfulPeople}%")
