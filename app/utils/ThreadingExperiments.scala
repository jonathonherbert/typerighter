package utils

import java.util.concurrent.ConcurrentHashMap

import scala.concurrent.{ExecutionContext, Future, Promise, blocking}

case class ValidatorConfig()
case class ValidatorRequest(category: String)
case class ValidatorResponse()

trait Validator {
  // this is your brain on blocking calls
  def handle(request: ValidatorRequest): ValidatorResponse
}

trait ValidatorFactory {
  def create(config: ValidatorConfig): Validator
}

class ThreadingExperiments(factory: ValidatorFactory, configs: Map[String, ValidatorConfig])(implicit ec: ExecutionContext) {
  private val validators = new ConcurrentHashMap[String, Promise[Validator]]()

  configs.foreach { case(category, config) =>
    val holder = Promise[Validator]()
    validators.put(category, holder)

    holder.completeWith(Future {
      blocking {
        factory.create(config)
      }
    })
  }

  def handle(request: ValidatorRequest): Future[ValidatorResponse] = {
    Option(validators.get(request.category)) match {
      case Some(validator) =>
        validator.future.map { v =>
          blocking {
            v.handle(request)
          }
        }

      case None =>
        Future.failed(new IllegalStateException(s"Unknown category ${request.category}"))
    }
  }

  def updateConfig(category: String, config: ValidatorConfig): Future[Unit] = {
    Future {
      blocking {
        factory.create(config)
      }
    }.map { validator =>
      validators.put(category, Promise.successful(validator))

      ()
    }
  }
}
