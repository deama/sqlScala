import slick.jdbc.MySQLProfile.api._
import slick.jdbc.MySQLProfile.api._
import scala.concurrent.{Await, Future}
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.duration.Duration
import scala.util.{Failure, Success}

class People( tag:Tag ) extends Table[(Int, String, String, Int)](tag, "PEOPLE")
{
  def id = column[Int]("PER_ID", O.PrimaryKey, O.AutoInc)
  def fName = column[String]("PER_FNAME")
  def lName = column[String]("PER_LNAME")
  def age = column[Int]("PER_AGE")
  def * = (id, fName, lName, age)
}

object Main extends App
{
  val db = Database.forConfig("mysqlDB")

  val peopleTable = TableQuery[People]

  val dropPeopleCmd = DBIO.seq(peopleTable.schema.drop)
  val initPeopleCmd = DBIO.seq(peopleTable.schema.create)


  dropDB()
  /*
  println("1..."); Thread.sleep(100)
  println("2..."); Thread.sleep(100)
  println("3..."); Thread.sleep(100)
  println("4..."); Thread.sleep(100)
  println("5..."); Thread.sleep(100)
  println("6..."); Thread.sleep(100)
  println("7..."); Thread.sleep(100)
  println("8..."); Thread.sleep(100)
  println("9..."); Thread.sleep(100)
  println("10..."); Thread.sleep(100)
  println("11..."); Thread.sleep(100)
  println("12..."); Thread.sleep(100)
  println("13..."); Thread.sleep(100)
  println("14..."); Thread.sleep(100)
  println("15..."); Thread.sleep(100)
  println("16..."); Thread.sleep(100)
  println("17..."); Thread.sleep(100)
  println("18..."); Thread.sleep(100)
  println("19..."); Thread.sleep(100)
  println("20..."); Thread.sleep(100)
  */
  //println("...")
  //println(peopleTable)

  //Thread.sleep(3000)
  //searchById(1)
  //deleteById(1)
  //updateById(1,"Phillip")
  Thread.sleep(3000)
  updateById(1, "Phillip")








  Thread.sleep(5000)


  def updateById(id:Int, fName:String) :Unit =
  {
    val queryFuture = Future
    {
      val q = for{ p <- peopleTable if p.id === id } yield p.fName
      db.run( q.update(fName) )
    }

    Await.result(queryFuture, Duration.Inf).andThen
    {
      case Success(_) => None//db.close()
      case Failure(error) =>
      {
        println( "error: " + error.getMessage() )
      }
    }
  }

  def deleteById(id:Int) :Unit =
  {
    val queryFuture = Future
    {
      db.run(peopleTable.filter(_.id === id).delete)
    }

    Await.result(queryFuture, Duration.Inf).andThen
    {
      case Success(_) => None//db.close()
      case Failure(error) =>
      {
        println( "error: " + error.getMessage() )
      }
    }
  }

  def searchById(id:Int) :Unit =
  {
    val queryFuture = Future
    {
      db.run(peopleTable.filter(_.id === id).result).map(_.foreach
      {
        case( id, fName, lName, age ) => println(s" $id $fName $lName $age" )
      })
    }

    Await.result(queryFuture, Duration.Inf).andThen
    {
      case Success(re) => None//db.close()
      case Failure(error) =>
      {
        println( "error: " + error.getMessage() )
      }
    }
  }

  def dropDB() :Unit =
  {
    val dropFuture = Future{db.run(dropPeopleCmd)}

    Await.result(dropFuture, Duration.Inf).andThen
    {
      case Success(_) => initPeople()
      case Failure(error) =>
      {
        println( "failed: " + error.getMessage )
        initPeople()
      }
    }
  }

  def initPeople() :Unit =
  {
    val setupFuture = Future
    {
      db.run(initPeopleCmd)
    }

    Await.result(setupFuture, Duration.Inf).andThen
    {
      case Success(_) => runQuery()
      case Failure(error) =>
      {
        println( "failed: " + error.getMessage )
      }
    }
  }

  def runQuery() :Unit =
  {
    val insertPeople = Future
    {
      val query = peopleTable ++= Seq(
        (10, "Jack", "Wood", 36),
        (20, "Bob", "Pump", 26)
      )

      println(query.statements.head)
      db.run(query)
    }

    Await.result(insertPeople, Duration.Inf).andThen
    {
      case Success(_) => listPeople()
      case Failure(error) => println("oopsy: " + error.getMessage)
    }
  }

  def listPeople() :Unit =
  {
    val queryFuture = Future
    {
      db.run(peopleTable.result).map(_.foreach
      {
        case( id, fName, lName, age ) => println(s" $id $fName $lName $age" )
      })
    }

    Await.result(queryFuture, Duration.Inf).andThen
    {
      case Success(_) => None//db.close()
      case Failure(error) =>
      {
        println( "error: " + error.getMessage() )
      }
    }
  }
}