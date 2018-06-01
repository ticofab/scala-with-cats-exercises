package chapter4

import cats.data.Reader
import cats.syntax.all._

object ReaderStuff {

  type DbReader[A] = Reader[Db, A]

  def checkLogin(userId: Int,
                 password: String): DbReader[Boolean] = {
    for {
      maybeUsername <- findUsername(userId)
      passwordCorrect <- maybeUsername.map(username => checkPassword(username, password)).getOrElse(false.pure[DbReader])
    } yield passwordCorrect
  }

  def findUsername(userId: Int): DbReader[Option[String]] = Reader[Db, Option[String]](db => db.usernames.get(userId))


  def checkPassword(username: String,
                    password: String): DbReader[Boolean] = Reader[Db, Boolean](db => db.passwords.get(username).contains(password))

  case class Db(usernames: Map[Int, String],
                passwords: Map[String, String])

}

