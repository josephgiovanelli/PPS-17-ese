package it.unibo.pps.ese.controller.simulation.loader.data.builder.gene

import it.unibo.pps.ese.controller.simulation.loader
import it.unibo.pps.ese.controller.simulation.loader.data.DefaultGeneData.{CompleteDefaultGeneData, PartialDefaultGeneData}
import it.unibo.pps.ese.controller.simulation.loader.data.{DefaultGeneData, PartialAlleleData}
import it.unibo.pps.ese.controller.simulation.loader.data.builder._
import it.unibo.pps.ese.controller.simulation.loader.data.builder.gene.GeneStatus.{DefaultGene, DefaultGeneTemplate, EmptyGene, ValidGene}

import scala.reflect.runtime.universe._
import scala.util.{Failure, Success, Try}
import it.unibo.pps.ese.utils.DefaultValidable.ValidableByDisequality._

/** Builder that can build a PartialDefaultGeneData as partial data instance and a CompleteDefaultGeneData as complete data
  * instance
  *
  * @tparam S Builder's current status
  */
trait DefaultGeneBuilder[S <: GeneStatus] extends GeneBuilder[S] with GenericBuilder[S, DefaultGene, PartialDefaultGeneData, CompleteDefaultGeneData] {
  type RET[A <: S] = DefaultGeneBuilder[A]

  /** Set multiple builder's properties starting from [[it.unibo.pps.ese.controller.simulation.loader.DefaultGene]]
    *
    * @param defaultGene Default gene data
    * @return New builder with updated param and status
    */
  def setDefaultInfo(defaultGene: it.unibo.pps.ese.controller.simulation.loader.DefaultGene): DefaultGeneBuilder[S with DefaultGeneTemplate]
}

/** Factory object for [[it.unibo.pps.ese.controller.simulation.loader.data.builder.gene.DefaultGeneBuilder]]*/
object DefaultGeneBuilder {

  /** Create a new empty [[it.unibo.pps.ese.controller.simulation.loader.data.builder.gene.DefaultGeneBuilder]]
    *
    * @return An empty [[it.unibo.pps.ese.controller.simulation.loader.data.builder.gene.DefaultGeneBuilder]]
    */
  def apply(): DefaultGeneBuilder[EmptyGene] = new DefaultGeneBuilderImpl[EmptyGene](None, None, Map(), Iterable())

  private[this] class DefaultGeneBuilderImpl[T <: GeneStatus](_id: Option[String],
                                                              _name: Option[String],
                                                              _properties: Map[String, Class[_]],
                                                              _alleles: Iterable[AlleleBuilder[_]])
                                               (implicit val status: TypeTag[T])
    extends GenericGeneBuilderImpl[T](_id, _name, _properties, _alleles) with DefaultGeneBuilder[T]
      with BaseGenericBuilder[T , DefaultGene, PartialDefaultGeneData, CompleteDefaultGeneData]
      with ValidStatusGenericBuilder[T , DefaultGene, PartialDefaultGeneData, CompleteDefaultGeneData, ValidGene]{

    override def newInstance[NT <: GeneStatus](id: Option[String], name: Option[String], properties: Map[String, Class[_]],
                                               alleles: Iterable[AlleleBuilder[_]])(implicit tt: TypeTag[NT]): DefaultGeneBuilder[NT] = {
      new DefaultGeneBuilderImpl[NT](id, name, properties, alleles)
    }

    override def setDefaultInfo(defaultGene: loader.DefaultGene): DefaultGeneBuilder[T with DefaultGeneTemplate] = {
      new DefaultGeneBuilderImpl(_id, Some(defaultGene.name), defaultGene.properties, _alleles)
    }

    override def tryCompleteBuild(): Try[CompleteDefaultGeneData] = {
      status.tpe match {
        case t if t <:< typeOf[DefaultGene] =>
          val check = completeGeneRequirements()
          val exc = check._1 ++: checkProperties
          if(exc.isEmpty) {
            Success(new DefaultGeneDataImpl(_id, _name.get, _properties, check._2) with CompleteDefaultGeneData)
          } else {
            Failure(check._1.get)
          }
        case _ =>
          Failure(checkProperties().get)
      }
    }

    override protected def buildPartialInstance(): PartialDefaultGeneData = {
      new DefaultGeneDataImpl(_id, _name.get, _properties, _alleles.map(_.build()))
    }
  }

  private[gene] class DefaultGeneDataImpl[A <: PartialAlleleData](_getId: Option[String],
                                                                  override val name: String,
                                                                  _getProperties: Map[String, Class[_]],
                                                                  _getAlleles: Iterable[A]) extends DefaultGeneData[A] {
    import BuildersValidationImplicits._

    override val getProperties: Option[Map[String, Class[_]]] = _getProperties.boxToValidOption()
    override val getAlleles: Option[Iterable[A]] = _getAlleles.boxToValidOption()
    override def getId: Option[String] = _getId.normalize()
  }
}