package de.bwhc.mtb.data.entry.test


import scala.util.{
  Either,
  Left,
  Right
}
import scala.concurrent.{
  ExecutionContext,
  Future
}
import cats.data.{
  EitherNel,
  IorNel,
  NonEmptyList
}
import de.bwhc.mtb.dtos.{
  Patient,
  MTBFile,
  ZPM
}
import de.bwhc.mtb.views.MTBFileView
import de.bwhc.mtb.query.api._



class FakeQueryServiceProvider extends QueryServiceProvider
{

  def getInstance: QueryService = {
    new FakeQueryService
  }  

}


class FakeQueryService
extends QueryService
{


  override def process(
    cmd: DataOps.Command
  )(
    implicit ec: ExecutionContext
  ): Future[Either[String,DataOps.Response]] = {

    import DataOps.Command._
    import DataOps.Response._
    import java.time.Instant.now
    import cats.syntax.either._

    cmd match {

      case Upload(mtbfile) =>
        Future.successful(
          Created(
            mtbfile.patient.id,
            Snapshot(
              Snapshot.Id("dummy"),
              now,
              mtbfile
            )
          )
          .asRight[String]
        )


      case Delete(id)   =>
        Future.successful(
          Deleted(id).asRight[String]
        )

    }

  }


 
  // Methods below never used in tests here

  override def patients(
    implicit ec: ExecutionContext
  ): Future[Iterable[Patient]] = ???

  override def mtbFile(
    patId: Patient.Id
  )(
    implicit ec: ExecutionContext
  ): Future[Option[Snapshot[MTBFile]]] = ???


  override def mtbFileHistory(
    patId: Patient.Id
  )(
    implicit ec: ExecutionContext
  ): Future[Option[History[MTBFile]]] = ???


  override def resultsOf(
    query: PeerToPeerRequest[Query.Parameters]
  )(
    implicit ec: ExecutionContext
  ): Future[Iterable[Snapshot[MTBFile]]] = ???


  override def process(
    req: PeerToPeerRequest[MTBFileParameters]
  )(
    implicit ec: ExecutionContext
  ): Future[Option[Snapshot[MTBFile]]] = ???

  override def getLocalQCReport(
    request: PeerToPeerRequest[Map[String,String]]
  )(
    implicit ec: ExecutionContext
  ): Future[Either[String,LocalQCReport]] = ???


  override def compileGlobalQCReport(
    implicit
    querier: Querier,
    ec: ExecutionContext
  ): Future[IorNel[String,GlobalQCReport]] = ???

  override def peerStatusReport(
    implicit ec: ExecutionContext
  ): Future[PeerStatusReport] = ???

  override def process(
    cmd: QueryOps.Command
  )(
    implicit
    querier: Querier,
    ec: ExecutionContext
  ): Future[IorNel[String,Query]] = ???

  override def get(
    id: Query.Id
  )(
    implicit ec: ExecutionContext
  ): Future[Option[Query]] = ???


  override def resultSummaryOf(
    query: Query.Id,
  )(
    implicit ec: ExecutionContext
  ): Future[Option[ResultSummary]] = ???


  override def patientsFrom(
    query: Query.Id,
  )(
    implicit ec: ExecutionContext
  ): Future[Option[Iterable[PatientView]]] = ???


  override def mtbFileFrom(
    query: Query.Id,
    patId: Patient.Id
  )(
    implicit ec: ExecutionContext
  ): Future[Option[MTBFile]] = ???


  override def mtbFileViewFrom(
    query: Query.Id,
    patId: Patient.Id
  )(
    implicit ec: ExecutionContext
  ): Future[Option[MTBFileView]] = ???

  override def ngsSummariesFrom(
    query: Query.Id,
  )(
    implicit ec: ExecutionContext
  ): Future[Option[Iterable[NGSSummary]]] = ???


  override def variantsOfInterestOf(
    query: Query.Id,
  )(
    implicit ec: ExecutionContext
  ): Future[Option[VariantsOfInterest]] = ???


  override def therapyRecommendationsFrom(
    query: Query.Id,
  )(
    implicit ec: ExecutionContext
  ): Future[Option[Iterable[TherapyRecommendationSummary]]] = ???


  override def molecularTherapiesFrom(
    query: Query.Id,
  )(
    implicit ec: ExecutionContext
  ): Future[Option[Iterable[MolecularTherapySummary]]] = ???


  override def savedQueriesOf(
    querier: Querier
  )(
    implicit ec: ExecutionContext
  ): Future[Iterable[SavedQueryInfo]] = ???


  override def retrieveMTBFileSnapshot(
    patId: Patient.Id,
    snpId: Option[Snapshot.Id],
    site: Option[ZPM]
  )(
    implicit
    querier: Querier,
    ec: ExecutionContext
  ): Future[Either[String,Option[MTBFileView]]] = ???


   def preparedQueries(
     implicit
     querier: Querier,
     ec: ExecutionContext
   ): Future[Either[String,Seq[PreparedQuery]]] = ???

   def preparedQuery(
     id: PreparedQuery.Id
   )(
     implicit
     querier: Querier,
     ec: ExecutionContext
   ): Future[Either[String,Option[PreparedQuery]]] = ???

   def process(
     cmd: PreparedQuery.Command
   )(
     implicit
     querier: Querier,
     ec: ExecutionContext
   ): Future[EitherNel[String,PreparedQuery]] = ???
   
   def compileGlobalMedicationDistribution(
     implicit
     querier: Querier,
     ec: ExecutionContext
   ): Future[IorNel[String,ReportingAliases.GlobalMedicationDistributionReport]] = ???

   def compileGlobalPatientTherapies(
     medication: Option[de.bwhc.mtb.dtos.Medication.Coding]
   )(
     implicit
     querier: Querier,
     ec: ExecutionContext
   ): Future[IorNel[String,GlobalReport[Seq[PatientTherapies]]]] = ???

   def compileGlobalTumorEntityDistribution(
     medication: Option[de.bwhc.mtb.dtos.Medication.Coding]
   )(
     implicit
     querier: Querier,
     ec: ExecutionContext
   ): Future[IorNel[String,ReportingAliases.GlobalTumorEntityDistributionReport]] = ???

   def compileLocalMedicationDistributionFor(
     request: PeerToPeerRequest[Report.Filters]
   )(
     implicit ec: ExecutionContext
   ): Future[Either[NonEmptyList[String],ReportingAliases.LocalMedicationDistributionReport]] = ???

   def compileLocalTumorEntityDistributionFor(
     request: PeerToPeerRequest[Report.Filters]
   )(
     implicit ec: ExecutionContext
   ): Future[Either[NonEmptyList[String],ReportingAliases.LocalTumorEntityDistributionReport]] = ???

   def compilePatientTherapies(
     request: PeerToPeerRequest[Report.Filters]
   )(
     implicit ec: ExecutionContext
   ): Future[Either[NonEmptyList[String],LocalReport[Seq[PatientTherapies]]]] = ???



}
