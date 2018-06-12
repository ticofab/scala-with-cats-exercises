package chapter8

import cats.instances.list._
import cats.syntax.functor._
import cats.syntax.traverse._
import cats.{Applicative, Id, Monad}

import scala.concurrent.Future

// modeled as trait so to stub it in unit tests
trait UptimeClient[F[_]] {
  def getUptime(hostname: String): F[Int]
}

class UptimeService[F[_] : Applicative](client: UptimeClient[F]) {
  def getTotalUptime(hostnames: List[String]): F[Int] =
    hostnames.traverse(client.getUptime).map(_.sum)
}

trait RealUptimeClient extends UptimeClient[Future] {
  override def getUptime(hostname: String): Future[Int]
}

trait FakeUptimeClient extends UptimeClient[Id] {
  override def getUptime(hostname: String): Id[Int]
}

class TestUptimeClient(hosts: Map[String, Int]) extends FakeUptimeClient {
  override def getUptime(hostname: String): Id[Int] = Monad[Id].pure(hosts.getOrElse(hostname, 0))
}

