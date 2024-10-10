// https://leetcode.com/problems/minimum-add-to-make-parentheses-valid/

object MinimumAddToMakeParenthesisValid:

  def minAddToMakeValid(s: String): Int =
    var open = 0
    var uncloseable = 0
    s.foreach(char =>
      if char == '(' then
        open += 1
      else if char == ')' then
        if open > 0 then open -= 1
        else uncloseable += 1
      else
        ()
    )
    uncloseable + open
  
object MinimumAddToMakeParenthesisValidApp extends App:

  {
    val res = MinimumAddToMakeParenthesisValid.minAddToMakeValid("())")
    println(res)
  }
  {
    val res = MinimumAddToMakeParenthesisValid.minAddToMakeValid("(((")
    println(res)
  }
