import scala.io.StdIn
import java.util.regex.{Matcher, Pattern}
import scala.io.StdIn._
import play.api.libs.json._
import scala.collection.mutable.ListBuffer
import scala.collection.IterableOnce.iterableOnceExtensionMethods

class TicketBooking extends InputCheck {
  var ticketTypeList=List("business","premium","economy");
  var ticketRates=Array(4999,3599,1988);
  var dbHandler=new DBHandler();
  var operationState=true;
  var loginUserId=0;

  /*Home page option*/
  def selectOperation(){
    while(operationState){
      println("Welcome to ticket booking app");
      println("Press key 1 : Signup");
      println("Press key 2 : Login");
      println("select your operation:")
      var getUserOption=readLine();
      if(checkNumber(getUserOption)){
        var userOption=Integer.parseInt(getUserOption);
        if(userOption==1){
          signupOperation();
        }
        else if(userOption==2) {
          loginOperation();
        }
        else{
          println("Please press correct key")
        }
      }
      else{
        println("Please enter only digits")
      }
    }
  }

  /*signup operation(with userinput validation)*/
  def signupOperation(){
    println("      \t\tSignup \uD83D\uDCDD")
    var userName=readLine("\uD83D\uDC64\tEnter your name :");
    if(checkValidString(userName)){
      var phoneNumber=readLine("Enter your phone Number :");
      if(checkNumber(phoneNumber) && (phoneNumber.length()>=10 && phoneNumber.length()<15)){
       var number=phoneNumber.toLong;
        var password=readLine("\uD83D\uDD11\tEnter your password:");
        if(checkPassword(password) && password.length==8){
          if(dbHandler.addUserData(userName,number,password)){
            loginOperation();
          }
          else{
            println("User account is already exists")
          }
        }
        else{
          println("Your password must contains 8 characters " +
            "\nAt least use one lowercase " +
            "\nAt least use one uppercase" +
            "\nAt least use one digit" +
            "\nAt least one special character");
        }
      }
      else{
        println("Please use only digits");
      }
    }
    else{
      println("Please use only alphabets");
    }
  }

  /*login operation*/
  def loginOperation(){
    operationState=false
    println("     \t\tLogin \uD83D\uDD13")
    var userNum=readLine("\uD83D\uDCF1\tEnter your number :");
    if(checkNumber(userNum)){
      var userPassword=readLine("\uD83D\uDD11\tEnter your password :");
      var phoneNumber=userNum.toLong;
      if(dbHandler.userExists(phoneNumber,userPassword)){
        var loginUser=dbHandler.getUserData(phoneNumber);
        loginUserId=Integer.parseInt(loginUser("id").toString());
        println("       \t\tPROFILE \uD83D\uDC64")
        println("\nName:"+loginUser("name")+"\t\t\tPhone Number:"+loginUser("phoneNumber"));
        ticketBookingMenu()
      }
      else{
        println("User account is not found")
        operationState=true;
      }
    }
    else{
      println("Your phone number should contains only digits")
      operationState=true;
    }
  }

  /*After login we show the options what they need*/
  def ticketBookingMenu(){
    var state=true;
    while (state){
      println("\npress 1:TicketBooking" +
        "\n\npress 2:My Booking" +
        "\n\npress 3:Cancel the ticket"+
        "\n\npress 4:Exit");
        var getUserOperation=readLine("\nChoose your operation:");
        if(checkNumber(getUserOperation)){
        var userOption=Integer.parseInt(getUserOperation);
        if(userOption==1){
          enterYourJourneyPlace();
        }
        else if(userOption==2) {
         myBooking()
        }
        else if(userOption==3){
          cancelTheTicket();
        }
        else if(userOption==4){
         state=false;
        }
        else{
          println("Please press correct key")
        }
      }
      else{
        println("Please enter only digits")
      }
    }
  }

  /* get the travel place from the user*/
  def enterYourJourneyPlace(){
    println("\n\t\tWelcome to airline Booking");
    var flightChoseState=true;
      println("\n Note:Within TamilNadu flights are available")
      var fromCity=readLine("From :")
      if(checkValidString(fromCity)){
        var toCity=readLine("To :")
        if(checkValidString(toCity)){
          showTheAvailableFlights(fromCity.toLowerCase(),toCity.toLowerCase())
        }
        else{
          println("Please give the correct city name")
        }
      }
      else{
        println("Please give the alphabets & proper city name")
      }
  }

 /*show the available flights for the travel place*/
  def showTheAvailableFlights(fromCity:String,toCity:String){
    var flight=dbHandler.getFlightDetails(fromCity,toCity)
    var flight_id_list=ListBuffer[String]();
    var x=0;
    println("\n  Flight_id     \t\t Flight_Name       \t\tTime           \t\t\t\tDay     \t\tAvailable_seat_count")
    for(x <- 1 to flight.size){
      var flight_map=flight(x.toString())
      println("\n  "+flight_map("flight_id")+"  \t\t\t\t\t"+flight_map("flight_name")+"   \t\t"+flight_map("time")+"     \t\t"
      +flight_map("day")+"     \t\t"+flight_map("seats"))
      flight_id_list+=flight_map("flight_id").toString()
    }
    choseYourFlight(flight_id_list);
  }

  /*chose the flight for book the tickets*/
  def choseYourFlight(flight_id_list:ListBuffer[String]){
    var choseFlightId=readLine("Enter the given Flight id :")
    if(checkNumber(choseFlightId)){
      if(flight_id_list.contains(choseFlightId)){
        getTicketDetails(Integer.parseInt(choseFlightId));
      }
      else {
        println("Your flight id is not available")
      }
    }
    else{
      println("Please enter the number")
    }
  }


  /*get the tickettype and how many tickets from the user and we show the remaining tickets*/
  def getTicketDetails(choseFlightId:Int){
    println("These are Ticket types: \n *Business=Rs.4999 \n *Economy=Rs.1988 \n *Premium=Rs.3599");
    var ticketType= readLine("\nTicket type :");
    ticketType=ticketType.toLowerCase();
    if(ticketTypeList.contains(ticketType)){
      var totalSeat=dbHandler.getSeatCount(choseFlightId);
      var count=readLine("\nNo of passenger :");
      if(checkNumber(count)){
        var passengerCount=Integer.parseInt(count)
        if(passengerCount<totalSeat){
          getPassengerDetails(passengerCount,ticketType,choseFlightId);
        }
        else{
          println("\nOnly "+totalSeat+" seats available,Please give the less passenger count")
        }
      }
      else {
        println("\nPlease give the number & only")
      }
    }
    else{
      println("\nPlease give the proper ticket type name");
    }
  }

  /*get the passenger name and age*/
  def getPassengerDetails(count: Int,ticketType:String,choseFlightId:Int){
    var a =0;
    for(a <- 1 to count){
      var passengerName=readLine("\nEnter passenger "+a+" Name :");
      if(checkValidString(passengerName)){
        var passengerAge=readLine("\nEnter "+passengerName+" age :");
        if(checkNumber(passengerAge)){
          var age=Integer.parseInt(passengerAge);
          dbHandler.addPassenger(passengerName,age,loginUserId,choseFlightId);
          ticketOffer(ticketType,age);
          if(a==count){
            println("\nYour ticket has been booked have a safe journey")
          }
        }
      }
      else {
        println("\nyour name must contains only alphabets");
      }
    }
  }

  /*show the booking tickets*/
  def myBooking():ListBuffer[String]={
    var bookingDetails=dbHandler.getTicketDetails(loginUserId);
    var passenger_id=ListBuffer[String]()
    var total_price=0D;
    println("     \t\tMy Bookings")
    var x=0;
    if(bookingDetails.size>1){
      println("\npassenger_id     \t\t\tName   \t\t\tAge \t\t\tTicket type   \t\t\tOffer price    \t\tTicket Price  \t\t\tflight Name     \t\t\t\t\tfrom   \t\t\tto   \t\t\t\t\ttime");
      for( x <- 1 to bookingDetails.size){
        var singleData=bookingDetails(x.toString())
        passenger_id+=singleData("passenger_id").toString();
        println("\n\t\t"+singleData("passenger_id")+"   \t\t\t"+singleData("Name")+"   \t\t"+singleData("Age")+"     \t\t\t"+singleData("TicketType")+
          "   \t\t\t\t\t"+singleData("OfferPrice")+"  \t\t\t\t"+singleData("TicketPrice")+"    \t\t\t\t\t"+singleData("flight_name")+
          "   \t\t\t\t\t"+singleData("from_city")
          +"  \t\t"+singleData("to_city")+"  \t\t\t"+singleData("time"))
        total_price+=Integer.parseInt(singleData("TicketPrice").toString)
      }
      println("\n                       \t\t\t\t \t\t\t\t Total price="+total_price)
    }
    else{
     println("So far, You have not book anytickets")
    }
    return  passenger_id;
  }

  /*set the ticket price what they select type of ticket*/
  def ticketOffer(ticketType:String,age:Int){
    var ticketRate=0;
    var x=0;
    for( x <- 0 to (ticketTypeList.length-1)){
      if(ticketType.equals(ticketTypeList(x))){
        ticketRate=ticketRates(x);
      }
    }
    totalTicketRate(ticketType,ticketRate,age);
  }

  /*calculate the ticket price*/
  def totalTicketRate(ticketType:String,ticketRate:Double,age:Int):Double = {
    var offerPrice=0D;
    var totalAmount=0D;
    if(age<=10){
      offerPrice=(ticketRate*30/100)
      totalAmount=ticketRate-offerPrice;
    }
    else if(age>=60){
      offerPrice=(ticketRate*15/100)
      totalAmount=ticketRate-offerPrice;
    }
    else{
      totalAmount=ticketRate;
    }
    dbHandler.addTicketDetails(loginUserId,ticketType,offerPrice,totalAmount);
    totalAmount;
  }

  /*canel the ticket*/
  def cancelTheTicket(){
    var userList=myBooking();
    var passengerId=readLine("Enter passenger Id:")
    if(checkNumber(passengerId)){
      if(userList.contains(passengerId)){
        dbHandler.deleteTicket(Integer.parseInt(passengerId));
      }
      else{
        println("user not found")
      }
    }
    else {
      println("please enter only number")
    }
  }

}
