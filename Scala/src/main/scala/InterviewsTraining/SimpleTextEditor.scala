import java.io._
import java.math._
import java.security._
import java.text._
import java.util.concurrent._
import java.util.function._
import java.util.regex._
import java.util.stream._
import scala.concurrent._
import scala.io._
import scala.math._
import scala.sys._
import scala.util.matching._
import scala.reflect._

object SimpleTextEditor {

  sealed trait Operation {

    def applyTo(current: String, previous: Option[String] = None): String
  }

  case class AppendOperation(toAppend: String) extends Operation {

    def applyTo(current: String, previous: Option[String] = None): String =
      current ++ toAppend
  }

  case class DeleteOperation(charsToDelete: Int) extends Operation {

    def applyTo(current: String, previous: Option[String] = None): String =
      current.dropRight(charsToDelete)
  }

  case class ShowOperation(index: Int) extends Operation {

    def applyTo(current: String, previous: Option[String] = None): String =
      println(current.lift(index - 1).fold("")(_.toString))
      current
  }

  case object RevertOperation extends Operation {

    def applyTo(current: String, previous: Option[String] = None): String =
      previous.getOrElse(current)
  }

  private val parseTransformation: String => Operation = {
    case s"1 $string" => AppendOperation(string)
    case s"2 $numberOfChars" => DeleteOperation(numberOfChars.toInt)
    case s"3 $index" => ShowOperation(index.toInt)
    case "4" => RevertOperation
  }

  def applyTransformations(input: String, operations: List[String]): String = {
    operations
      .map(parseTransformation)
      .foldLeft((input, Option.empty[String]))((acc, operation) =>
        val newVersion = operation.applyTo(acc._1, acc._2)
        (newVersion, Some(acc._1))
      )
      ._1
  }
}

object SimpleTextEditorApp {

  private val ops = List("1 fg", "3 6", "2 5", "4", "3 7", "4", "3 4")
  private val inputString = "abcde"

  def main(args: Array[String]): Unit = {
    println(SimpleTextEditor.applyTransformations(inputString, ops))
  }
}
