package de.bwhc.mtb.data.entry.query.proxy



import scala.util.Either

import scala.concurrent.{
  ExecutionContext,
  Future
}

import de.bwhc.mtb.data.entry.impl.{
  QueryServiceProxy,
  QueryServiceProxyProvider
}

import de.bwhc.mtb.query.api.{
  QueryService,
  DataOps
}


class QueryServiceWrapperProvider extends QueryServiceProxyProvider
{

  def getInstance: QueryServiceProxy = {

    val queryService = QueryService.getInstance.get

    new QueryServiceWrapper(queryService)

  }  

}


class QueryServiceWrapper
(
  private val queryService: DataOps
)
extends QueryServiceProxy
{


  def process(
    cmd: QueryServiceProxy.Command
  )(
    implicit ec: ExecutionContext
  ): Future[Either[String,QueryServiceProxy.Response]] = {

    import QueryServiceProxy.Command._ 
    import QueryServiceProxy.Response._ 
    
    cmd match {

      //-----------------------------------------------------------------------
      case Upload(mtbfile) => {

        for {
          r <- queryService ! DataOps.Command.Upload(mtbfile)
        } yield r.map(_ => Imported)

      }

      //-----------------------------------------------------------------------
      case Delete(patId) => {

        for {
          r <- queryService ! DataOps.Command.Delete(patId)
        } yield r.map(_ => Deleted)

      }

    }

  }

}
