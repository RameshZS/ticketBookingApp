import play.api.libs.json.Format.GenericFormat
import java.sql.{Connection, DriverManager, PreparedStatement, ResultSet, SQLException, Statement}
import play.api.libs.json._;
import scala.collection.mutable._;

class DBHandler {
  var con: Connection =null;
  Class.forName("com.mysql.cj.jdbc.Driver")
  val url = "jdbc:mysql://localhost:3306/ticketBooking"
  val user = "root"
  val password = ""
  private var stmt: PreparedStatement=null;
  val db_connection:Connection = DriverManager.getConnection(url,user, password)
  private var result:ResultSet = null;
  var isAvailable=false;
  var signupState=false;

  /*after signup we add the user details in DB */
  def addUserData(name:String,phoneNum:Long,pwd:String):Boolean = {
    signupState=true;
    if(!(userExists(phoneNum,pwd))) {
      try {
        stmt = db_connection.prepareStatement("insert into user_data(userName,phoneNumber,password) VALUES(?,?,?)");
        stmt.setString(1, name);
        stmt.setLong(2, phoneNum);
        stmt.setString(3, pwd);
        stmt.execute();
        println("Signup successfully");
        signupState=false;
        return true;
      }
      catch {
        case e: SQLException => {
          e.printStackTrace()
        };
      }
    }
      return false;
  }

  /*check the user is already customer in this app and check signin user also*/
  def userExists(phoneNum:Long,pwd:String):Boolean={
    try {
      stmt=db_connection.prepareStatement("select * from user_data where phone_number=?");
      stmt.setLong(1,phoneNum);
      stmt.execute();
      result=stmt.getResultSet();
      while(result.next()) {
        if(signupState){
          if(result.getLong(3)==phoneNum || result.getString(4).equals(pwd) ) {
            isAvailable = true;
          }
          else {
            isAvailable=false;
          }
        }
        else{
          if((result.getLong(3)==phoneNum) && (result.getString(4).equals(pwd))){
            isAvailable=true;
          }
        }
      }
    }
    catch{
      case e: SQLException => {
        e.printStackTrace()
      };
    }
    return isAvailable;
  }

  /*after login get the user data from the db*/
  def getUserData(phoneNumber:Long):Map[String,Any]={
    var userData=Map[String,Any]();
   try{
     stmt=db_connection.prepareStatement("select * from user_data where phone_number=?");
     stmt.setLong(1,phoneNumber);
     stmt.execute();
     result=stmt.getResultSet();
     while(result.next()){
        userData +=("id"->result.getInt(1),"name"->result.getString(2),"phoneNumber"->result.getLong(3));
     }
   }
    catch {
      case e:SQLException => {
       e.printStackTrace()
      }
    }
    return userData;
  }

  /*add passenger for the flight ticket booking*/
  def addPassenger(passengerName:String,passengerAge:Int,userId:Int,flight_id:Int){
    decreaseSeatCount(flight_id);
    try {
      stmt=db_connection.prepareStatement("insert into passenger_details(user_id,passenger_name,age,flight_id) VALUES(?,?,?,?)")
      stmt.setInt(1,userId);
      stmt.setString(2,passengerName);
      stmt.setInt(3,passengerAge)
      stmt.setInt(4,flight_id);
      stmt.execute();
    }
    catch {
      case e:SQLException=>{
      e.printStackTrace()
      }
    }
  }

  /* get the remaning seat count for user choose particular flight*/
  def getSeatCount(flight_id:Int): Int ={
    var availableSeat=0;
    try {
      stmt=db_connection.prepareStatement("SELECT seat_availability FROM flight_details WHERE flight_id=?");
      stmt.setInt(1,flight_id)
      stmt.execute();
      result=stmt.getResultSet();
        while(result.next()){
          availableSeat=result.getInt(1);
        }
    }
    catch {
      case e:SQLException=>{
       e.printStackTrace()
      }
    }
    return availableSeat;
  }

  /*add ticket type and price details*/
  def addTicketDetails(loginUserId:Int,ticketType:String,offerPrice:Double,ticketPrice:Double){
    try{
      stmt=db_connection.prepareStatement("insert into ticket_price_detail(user_id,ticket_type,offer_price,ticket_price) VALUES(?,?,?,?)")
      stmt.setInt(1,loginUserId)
      stmt.setString(2,ticketType)
      stmt.setDouble(3,offerPrice)
      stmt.setDouble(4,ticketPrice);
      stmt.execute()
    }
    catch {
      case e:SQLException => {
        e.printStackTrace()
      }
    }
  }

 /*after passenger book the ticker decrease the ticket count*/
 def decreaseSeatCount(flight_id: Int){
   var seatCount=getSeatCount(flight_id);
   seatCount-=1;
  try{
     stmt=db_connection.prepareStatement("UPDATE flight_details SET seat_availability=? WHERE flight_id=?")
     stmt.setInt(1,seatCount);
     stmt.setInt(2,flight_id);
     stmt.executeUpdate();
  }
   catch {
     case e:SQLException =>{
       e.printStackTrace();
     }
   }
 }

  /*get ticket and flight deatils*/
  def getTicketDetails(userId:Int):Map[String,Map[String,Any]]={
    var detailsMap=Map[String,Map[String,Any]]()
    var index=1;
    try {
      stmt=db_connection.prepareStatement("select p.passenger_id,p.passenger_name,p.age,t.ticket_type,t.offer_price,t.ticket_price,f.flight_name,f.from_city,f.to_city,f.time,f.flight_id from passenger_details p,ticket_price_detail t,flight_details f WHERE p.passenger_id=t.passenger_id AND p.flight_id=f.flight_id AND p.user_id=?");
      stmt.setInt(1,userId);
      stmt.execute();
      result=stmt.getResultSet();
      while (result.next()){
        detailsMap(index.toString())=Map("passenger_id"->result.getInt(1),
                                        "Name"->result.getString(2),
                                        "Age"->result.getInt(3),
                                        "TicketType"->result.getString(4),
                                        "OfferPrice"->result.getInt(5),
                                        "TicketPrice"->result.getInt(6),
                                        "flight_name"->result.getString(7),
                                        "from_city"->result.getString(8),
                                         "to_city"->result.getString(9),
                                         "time"->result.getString(10),
                                          "flight_id"->result.getInt(11))

        index+=1;
      }
    }
    catch {
      case e:SQLException => {
        e.printStackTrace()
      }
    }
    return detailsMap;
  }

  /*get the available destination flight details*/
  def getFlightDetails(from:String,to:String):Map[String,Map[String,Any]]={
    var flight_details=Map[String,Map[String,Any]]()
    var index=1;
    try {
      stmt=db_connection.prepareStatement("select * from flight_details WHERE from_city=? AND to_city=?")
      stmt.setString(1,from)
      stmt.setString(2,to);
      stmt.execute();
      result=stmt.getResultSet
      while (result.next()){
        flight_details(index.toString())=Map("flight_id"->result.getInt(1),"flight_name"->result.getString(2),"day"->result.getString(5),"time"->result.getString(6),"seats"->result.getInt(7))
        index+=1;
      }
    }
    catch {
      case e:SQLException =>{
        e.printStackTrace()
      }
    }
    return flight_details
  }

  /* user cancel the ticket we delete the passenger details*/
  def deleteTicket(passenger_id:Int){
    increaseSeatCount(passenger_id);
   try {
     stmt=db_connection.prepareStatement("DELETE p.*,t.* FROM passenger_details p,ticket_price_detail t WHERE p.passenger_id=? AND t.passenger_id=?")
     stmt.setInt(1,passenger_id)
     stmt.setInt(2,passenger_id)
     stmt.execute();
    }
    catch {
      case e:SQLException => {
        e.printStackTrace()
      }
    }
  }

  /*use cancel the ticket increase the user ticket*/
  def increaseSeatCount(passenger_id:Int){
    var flight_id=getFlightId(passenger_id);
    println(flight_id)
    var seatCount=getSeatCount(flight_id);
    seatCount+=1;
    try{
      stmt=db_connection.prepareStatement("UPDATE flight_details SET seat_availability=? WHERE flight_id=?")
      stmt.setInt(1,seatCount);
      stmt.setInt(2,flight_id);
      stmt.executeUpdate();
    }
    catch {
      case e:SQLException =>{
        e.printStackTrace();
      }
    }
  }

  /*get the flight id for increase the saet count*/
   def getFlightId(passenger_id: Int):Int ={
     var flight_id=0;
     try{
       stmt=db_connection.prepareStatement("select flight_id from passenger_details WHERE passenger_id=?")
       stmt.setInt(1,passenger_id)
       stmt.execute();
       result=stmt.getResultSet();
       while (result.next()){
         flight_id=result.getInt(1);
       }
     }
     catch {
       case e:SQLException =>{
         e.printStackTrace()
       }
     }
     return flight_id;
   }
}
