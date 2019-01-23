package services

import java.util.concurrent.atomic.AtomicReference
import java.util.concurrent.{BlockingQueue, LinkedBlockingQueue}

import model.{CheckQuery, RuleMatch}
import play.api.Logger

import scala.concurrent.{Future, Promise}
import scala.util.control.NonFatal
import scala.util.{Failure, Success, Try}

trait IValidatorInstancePool {
  def check(req: CheckQuery): Future[List[RuleMatch]]
  def shutdown(): Future[Unit]
}

class ValidatorInstancePool(factory: IValidatorFactory, noOfThreads: Int = 1) extends IValidatorInstancePool {
  private val queue = new LinkedBlockingQueue[(CheckQuery, Promise[List[RuleMatch]])]()

  // Try to shutdown as soon as possible without processing the rest of the queue
  private val shutdownPromise = new AtomicReference[Option[Promise[Unit]]](None)

  override def check(req: CheckQuery): Future[List[RuleMatch]] = {
    val ret = Promise[List[RuleMatch]]()
    queue.add((req, ret))

    ret.future
  }

  override def shutdown(): Future[Unit] = {
    val ret = Promise[Unit]
    shutdownPromise.set(Some(ret))

    ret.future
  }

  for {i <- 0 until noOfThreads} {
    val threadName = s"validator-${factory.getName}-$i"
    Logger.info(s"Creating new thread: $threadName")
    new Thread(new ValidatorInstanceManager(factory, queue, shutdownPromise), threadName).start()
  }
}

class ValidatorInstanceManager(factory: IValidatorFactory, queue: BlockingQueue[(CheckQuery, Promise[List[RuleMatch]])],
                               shutdownPromise: AtomicReference[Option[Promise[Unit]]]) extends Runnable {

  val validator = Try(factory.createInstance())

  override def run(): Unit = {
    while (shutdownPromise.get().isEmpty) {
      val (request, ret) = queue.take()
      Logger.info(s"Processing validation request in thread: ${Thread.currentThread.getName}")
      val startTime = System.currentTimeMillis()
      validator match {
        case Success(tool) =>
          try {
            val response = tool.check(request)
            val finishTime = System.currentTimeMillis()
            Logger.info(s"Validation request for thread: ${Thread.currentThread.getName} done in ${finishTime - startTime}ms")
            ret.success(response)
          } catch {
            case NonFatal(err) =>
              ret.failure(err)
          }

        case Failure(err) =>
          ret.failure(err)
      }
    }

    // Shutdown
    validator match {
      case Success(tool) =>
        try {
          // Any shutdown logic goes here
          shutdownPromise.get().foreach(_.success(()))
        } catch {
          case NonFatal(err) =>
            shutdownPromise.get().foreach(_.failure(err))
        }

      case Failure(err) =>
        shutdownPromise.get().map(_.failure(err))
    }
  }
}


