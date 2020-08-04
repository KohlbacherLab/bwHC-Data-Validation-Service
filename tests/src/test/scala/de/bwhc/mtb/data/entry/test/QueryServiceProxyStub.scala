package de.bwhc.mtb.data.entry.test



import scala.util.{Either,Left,Right}

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


class QueryServiceProxyStubProvider extends QueryServiceProxyProvider
{

  def getInstance: QueryServiceProxy = {
    new QueryServiceProxyStub
  }  

}


class QueryServiceProxyStub
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

      case Upload(mtbfile) => Future.successful(Right(Imported))

      case Delete(patId) => Future.successful(Right(Deleted))

    }

  }

}
