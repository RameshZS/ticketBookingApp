import java.util.regex.{Matcher, Pattern}

class InputCheck {
  /*check the username is only contains alphabet*/
  def checkValidString(name:String):Boolean = name.forall(_.isLetter);

  /*check the phoneumber is only digits*/
  def checkNumber(number:String):Boolean = number.forall(_.isDigit);

  /*check the password using regex pattern*/
  def checkPassword(pwd:String):Boolean = {
    var pattern:Pattern = Pattern.compile("^(?=.*[0-9])(?=.*[a-z])(?=.*[A-Z])(?=.*[@#$%^&+=])(?=\\S+$).{8,}$");
    var matcher :Matcher = pattern.matcher(pwd);
    var checkPwd:Boolean=matcher.find();
    return checkPwd;
  }
}
