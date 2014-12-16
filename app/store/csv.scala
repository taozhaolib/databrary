package store

import scala.concurrent.Future
import play.api.libs.iteratee._
import macros._
import macros.async._
import site._
import models._

object CSV{

  implicit val executionContext = context.foreground


  def volume(vol: Volume): Future[String] = {

    vol.records.flatMap{r => 
      vol.containers.flatMap{
        c => c.mapAsync(_.records).map{cont => 

          val cr = c.zip(cont)
        }
      }
    }

  }


}

