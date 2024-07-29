import scala.io.StdIn

object q2{

  def validateInput(name: String, marks: Int, totalMarks: Int): (Boolean, Option[String]) = {
    if (name.isEmpty) {
      (false, Some("Name cannot be empty"))
    } else if (marks < 0 || marks > totalMarks) {
      (false, Some("Marks must be positive and cannot exceed total possible marks"))
    } else {
      (true, None)
    }
  }

  def getStudentInfoWithRetry(): (String, Int, Int, Double, Char) = {
    var isValid = false
    var errorMessage: Option[String] = None

    var name: String = ""
    var marks: Int = 0
    var totalMarks: Int = 0

    while (!isValid) {
      println("Enter student's name:")
      name = StdIn.readLine()
      println("Enter marks:")
      marks = StdIn.readInt()
      println("Enter total possible marks:")
      totalMarks = StdIn.readInt()

      val validation = validateInput(name, marks, totalMarks)
      isValid = validation._1
      errorMessage = validation._2

      if (!isValid) {
        println(s"Invalid input: ${errorMessage.get}")
      }
    }

    val percentage = (marks.toDouble / totalMarks) * 100
    val grade = percentage match {
      case p if p >= 90 => 'A'
      case p if p >= 75 => 'B'
      case p if p >= 50 => 'C'
      case _ => 'D'
    }

    (name, marks, totalMarks, percentage, grade)
  }

  def printStudentRecord(record: (String, Int, Int, Double, Char)): Unit = {
    val (name, marks, totalMarks, percentage, grade) = record
    println(s"Student Name: $name")
    println(s"Marks Obtained: $marks out of $totalMarks")
    println(f"Percentage: $percentage%.2f%%")
    println(s"Grade: $grade")
  }

  def main(args: Array[String]): Unit = {
    val studentInfo = getStudentInfoWithRetry()
    printStudentRecord(studentInfo)
  }
}
