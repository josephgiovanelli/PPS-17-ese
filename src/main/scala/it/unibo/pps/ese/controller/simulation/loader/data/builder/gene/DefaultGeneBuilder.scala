package it.unibo.pps.ese.controller.simulation.loader.data.builder.gene

import it.unibo.pps.ese.controller.simulation.loader
import it.unibo.pps.ese.controller.simulation.loader.data.DefaultGeneData.{CompleteDefaultGeneData, PartialDefaultGeneData}
import it.unibo.pps.ese.controller.simulation.loader.data.{DefaultGeneData, PartialAlleleData}
import it.unibo.pps.ese.controller.simulation.loader.data.builder.{AlleleBuilder, BuilderStatus, ValidStatusGenericBuilder}
import it.unibo.pps.ese.controller.simulation.loader.data.builder.exception.CompleteBuildException
import it.unibo.pps.ese.controller.simulation.loader.data.builder.gene.GeneStatus.{DefaultGene, DefaultGeneTemplate, EmptyGene, ValidGene}

import scala.reflect.runtime.universe._
import scala.util.{Failure, Success, Try}

trait DefaultGeneBuilder[S <: GeneStatus] extends BuildableGeneBuilder[S, DefaultGene, PartialDefaultGeneData, CompleteDefaultGeneData] {
  type RET[A <: S] = DefaultGeneBuilder[A]
  def setDefaultInfo(defaultGene: it.unibo.pps.ese.controller.simulation.loader.DefaultGene): DefaultGeneBuilder[S with DefaultGeneTemplate]
}

object DefaultGeneBuilder {

  def apply(): DefaultGeneBuilder[EmptyGene] = DefaultGeneBuilder[EmptyGene](None, None, Map(), Iterable())

  private def apply[T <: GeneStatus: TypeTag](id: Option[String], name: Option[String], properties: Map[String, Class[_]], alleles: Iterable[AlleleBuilder[_]]): DefaultGeneBuilder[T] =
    new DefaultGeneBuilderImpl[T](id, name, properties, alleles)
      with ValidStatusGenericBuilder[T , DefaultGene, PartialDefaultGeneData, CompleteDefaultGeneData, ValidGene]

  private[this] class DefaultGeneBuilderImpl[T <: GeneStatus](id: Option[String],
                                                name: Option[String],
                                                properties: Map[String, Class[_]],
                                                alleles: Iterable[AlleleBuilder[_]])
                                               (implicit val status: TypeTag[T])
    extends GenericGeneBuilderImpl[T](id, name, properties, alleles) with DefaultGeneBuilder[T] { self =>

    override def newInstance[NT <: GeneStatus](id: Option[String], name: Option[String], properties: Map[String, Class[_]],
                                               alleles: Iterable[AlleleBuilder[_]])(implicit tt: TypeTag[NT]): DefaultGeneBuilder[NT] = {
      DefaultGeneBuilder[NT](id, name, properties, alleles)
    }

    override def setDefaultInfo(defaultGene: loader.DefaultGene): DefaultGeneBuilder[T with DefaultGeneTemplate] = {
      DefaultGeneBuilder(id, Some(defaultGene.name), defaultGene.properties, alleles)
    }

    override def build(): PartialDefaultGeneData = {
      tryCompleteBuild() match {
        case Success(value) =>
          value
        case Failure(_) =>
          new DefaultGeneDataImpl(id, name.get, properties, alleles.map(_.build()))
      }
    }

    override def tryCompleteBuild(): Try[CompleteDefaultGeneData] = {
      status.tpe match {
        case t if t <:< typeOf[DefaultGene] =>
          val check = completeGeneRequirements
          val exc = check._1 ++: checkProperties
          if(exc.isEmpty) {
            Success(new DefaultGeneDataImpl(id, name.get, properties, check._2) with CompleteDefaultGeneData)
          } else {
            Failure(check._1.get)
          }
        case _ =>
          Failure(checkProperties.get)
      }
    }
  }

  private[gene] class DefaultGeneDataImpl[A <: PartialAlleleData](override val getId: Option[String],
                                                    override val name: String,
                                                    _getProperties: Map[String, Class[_]],
                                                    _getAlleles: Iterable[A]) extends DefaultGeneData[A] {
    override val getProperties: Option[Map[String, Class[_]]] = if(_getProperties.isEmpty) None else Some(_getProperties)
    //TODO to set?????
    override val getAlleles: Option[Set[A]] = if(_getAlleles.isEmpty) None else Some(_getAlleles.toSet)
  }
}