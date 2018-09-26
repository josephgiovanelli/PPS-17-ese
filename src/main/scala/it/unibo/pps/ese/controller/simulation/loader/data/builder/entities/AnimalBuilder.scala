package it.unibo.pps.ese.controller.simulation.loader.data.builder.entities

import it.unibo.pps.ese.controller.simulation.loader.AnimalStructuralProperties
import it.unibo.pps.ese.controller.simulation.loader.data.AnimalData.{CompleteAnimalData, PartialAnimalData}
import it.unibo.pps.ese.controller.simulation.loader.data.CustomGeneData.{CompleteCustomGeneData, PartialCustomGeneData}
import it.unibo.pps.ese.controller.simulation.loader.data.DefaultGeneData.{CompleteDefaultGeneData, PartialDefaultGeneData}
import it.unibo.pps.ese.controller.simulation.loader.data._
import it.unibo.pps.ese.controller.simulation.loader.data.builder.{BaseBuildableGenericBuilder, GenericBuilder, ValidStatusGenericBuilder}
import it.unibo.pps.ese.controller.simulation.loader.data.builder.entities.EntityStatus._
import it.unibo.pps.ese.controller.simulation.loader.data.builder.exception.{CompleteBuildException, InvalidParamValueBuildException}
import it.unibo.pps.ese.controller.simulation.loader.data.builder.gene.{CustomGeneBuilder, DefaultGeneBuilder}
import it.unibo.pps.ese.model.genetics.entities.QualityType

import scala.reflect.runtime.universe._
import scala.util.{Failure, Success, Try}
import it.unibo.pps.ese.utils.DefaultValidable.ValidableByDisequality._

trait AnimalBuilder[T <: EntityStatus] extends EntityBuilder[T] with GenericBuilder[T, FullAnimal, PartialAnimalData, CompleteAnimalData]{
  override type RET[A <: T] = AnimalBuilder[A]
  def setTypology(typology: String): AnimalBuilder[T with AnimalWithTypology]
  def addStructuralChromosome(structuralChromosome: Iterable[CustomGeneBuilder[_]]): AnimalBuilder[T with AnimalWithStructChromosome]
  def addRegulationChromosome(regulationChromosome: Iterable[DefaultGeneBuilder[_]]): AnimalBuilder[T with AnimalWithRegChromosome]
  def addSexualChromosome(sexualChromosome: Iterable[DefaultGeneBuilder[_]]): AnimalBuilder[T with AnimalWithSexChromosome]
}

object AnimalBuilder {

  def apply(): AnimalBuilder[EmptyEntity] = new AnimalBuilderImpl[EmptyEntity](None, None, None, None, None, Seq(), Seq(), Seq())

  private[this] class AnimalBuilderImpl[T <: EntityStatus](name: Option[String],
                                             geneLength: Option[Int],
                                             alleleLength: Option[Int],
                                             reign: Option[String],
                                             typology: Option[String],
                                             structuralChromosome: Iterable[CustomGeneBuilder[_]],
                                             regulationChromosome: Iterable[DefaultGeneBuilder[_]],
                                             sexualChromosome: Iterable[DefaultGeneBuilder[_]])
                                            (implicit val status: TypeTag[T])
    extends EntityBuilderImpl[T](name, geneLength, alleleLength, reign) with AnimalBuilder[T]
      with BaseBuildableGenericBuilder[T , FullAnimal, PartialAnimalData, CompleteAnimalData]
      with ValidStatusGenericBuilder[T , FullAnimal, PartialAnimalData, CompleteAnimalData, ValidEntity] {


    def setTypology(typology: String): AnimalBuilder[T with AnimalWithTypology] =
      new AnimalBuilderImpl(name, geneLength, alleleLength, reign, Some(typology), structuralChromosome, regulationChromosome,
        sexualChromosome)

    def addStructuralChromosome(structuralChromosome: Iterable[CustomGeneBuilder[_]]): AnimalBuilder[T with AnimalWithStructChromosome] = {
      new AnimalBuilderImpl(name, geneLength, alleleLength, reign, typology, structuralChromosome, regulationChromosome,
        sexualChromosome)
    }

    def addRegulationChromosome(regulationChromosome: Iterable[DefaultGeneBuilder[_]]): AnimalBuilder[T with AnimalWithRegChromosome] = {
      new AnimalBuilderImpl(name, geneLength, alleleLength, reign, typology, structuralChromosome, regulationChromosome,
        sexualChromosome)
    }

    def addSexualChromosome(sexualChromosome: Iterable[DefaultGeneBuilder[_]]): AnimalBuilder[T with AnimalWithSexChromosome] = {
      new AnimalBuilderImpl(name, geneLength, alleleLength, reign, typology, structuralChromosome, regulationChromosome,
        sexualChromosome)
    }

    override def newInstance[NT <: EntityStatus](name: Option[String], geneLength: Option[Int],
                                                 alleleLength: Option[Int], reign: Option[String])
                                                (implicit tt: TypeTag[NT]): AnimalBuilder[NT] = {
      new AnimalBuilderImpl(name, geneLength, alleleLength, reign, typology, structuralChromosome, regulationChromosome,
        sexualChromosome)
    }

    def tryCompleteBuild(): Try[CompleteAnimalData] = {
      status.tpe match {
        case t if t <:< typeOf[FullAnimal] =>
          val check = checkComplete()
          val exception = checkProperties ++: check._1
          if(exception.isEmpty) {
            val ret: CompleteAnimalData = new AnimalDataImpl[CompleteCustomGeneData, CompleteDefaultGeneData](name.get, geneLength, alleleLength,
              reign, typology, check._2, check._3, check._4) with CompleteAnimalData
            Success(ret)
          } else {
            Failure(exception.get)
          }
        case t if t <:< typeOf[ValidEntity] =>
          Failure(CompleteBuildException("Animal: " + name + "must have all fields set"))
      }
    }

    private def checkComplete(): (Option[CompleteBuildException], Iterable[CompleteCustomGeneData], Iterable[CompleteDefaultGeneData], Iterable[CompleteDefaultGeneData]) ={
      var exception: Option[CompleteBuildException] = None
      val structTries: Iterable[Try[CompleteCustomGeneData]] = structuralChromosome.map(_.tryCompleteBuild())
      val struct: Iterable[CompleteCustomGeneData] = structTries.collect({case Success(value) => value})
      if(struct.size != structuralChromosome.size) {
        exception = exception ++: CompleteBuildException("Animal: "+ name.get +" | All structural chromosome's genes must be complete",
          structTries.collect({case Failure(value: CompleteBuildException) => value}))
      }
      val effectedProperties = struct.flatMap(_.conversionMap.values.flatMap(_.keySet)).toSet
      if(!(effectedProperties == AnimalStructuralProperties.elements.map(_.name))) {
        var exceptions: Seq[CompleteBuildException] = Seq()
        val notEffected = AnimalStructuralProperties.elements.map(_.name) -- effectedProperties
        exceptions = exceptions ++ notEffected.map(e => CompleteBuildException("Quality: " + e + " not effected"))
        val extraEffected = effectedProperties -- AnimalStructuralProperties.elements.map(_.name)
        exceptions = exceptions ++ extraEffected.map(e => CompleteBuildException("Quality: " + e + " effected, but not exists or isn't accessible"))
        exception = exception ++: CompleteBuildException("Animal: "+ name.get +" | Regulation chromosome mus effect all and only animal's base qualities", exceptions)
      }
      val regTries: Iterable[Try[CompleteDefaultGeneData]] = regulationChromosome.map(_.tryCompleteBuild())
      val reg: Iterable[CompleteDefaultGeneData] = regTries.collect({case Success(value) => value})
      if(reg.size != regulationChromosome.size) {
        exception = exception ++: CompleteBuildException("Animal: "+ name.get +" | All regulation chromosome's genes must be complete",
          regTries.collect({case Failure(value: CompleteBuildException) => value}))
      }
      val sexTries: Iterable[Try[CompleteDefaultGeneData]] = sexualChromosome.map(_.tryCompleteBuild())
      val sex: Iterable[CompleteDefaultGeneData] = sexTries.collect({case Success(value) => value})
      if(sex.size != sexualChromosome.size) {
        exception = exception ++: CompleteBuildException("Animal: "+ name.get +" | All sexual chromosome's genes must be complete",
          sexTries.collect({case Failure(value: CompleteBuildException) => value}))
      }
      (exception, struct, reg, sex)
    }

    override def checkProperties: Option[CompleteBuildException] = {
      var exception = super.checkProperties
      if(!typology.isValid())
        exception = exception ++: InvalidParamValueBuildException("Animal: " + name.getOrElse(""), "typology", typology)
      if(!structuralChromosome.isValid())
        exception = exception ++: InvalidParamValueBuildException("Animal: " + name.getOrElse(""), "structuralChromosome", structuralChromosome)
      if(!regulationChromosome.isValid())
        exception = exception ++: InvalidParamValueBuildException("Animal: " + name.getOrElse(""), "regulationChromosome", regulationChromosome)
      if(!sexualChromosome.isValid())
        exception = exception ++: InvalidParamValueBuildException("Animal: " + name.getOrElse(""), "sexualChromosome", sexualChromosome)
      exception
    }

    override protected def buildPartialInstance(): PartialAnimalData =
      new AnimalDataImpl(name.get, geneLength, alleleLength, reign, typology, structuralChromosome.map(_.build()),
        regulationChromosome.map(_.build()), sexualChromosome.map(_.build()))
  }

  private class AnimalDataImpl[C<:PartialCustomGeneData, D<:PartialDefaultGeneData](_name: String,
                                                                                    _getGeneLength: Option[Int],
                                                                                    _getAlleleLength: Option[Int],
                                                                                    _getReign: Option[String],
                                                                                    _getTypology: Option[String],
                                                                                    _getStructuralChromosome: Iterable[C],
                                                                                    _getRegulationChromosome: Iterable[D],
                                                                                    _getSexualChromosome: Iterable[D])
    extends EntityDataImpl(_name, _getGeneLength, _getAlleleLength, _getReign) with AnimalData[C, D] {
    import it.unibo.pps.ese.controller.simulation.loader.data.builder.BuildersValidationImplicits._

    def getTypology: Option[String] = _getTypology.normalize()
    val getStructuralChromosome: Option[Iterable[C]] = _getStructuralChromosome.boxToValidOption()
    val getRegulationChromosome: Option[Iterable[D]] = _getRegulationChromosome.boxToValidOption()
    val getSexualChromosome: Option[Iterable[D]] = _getSexualChromosome.boxToValidOption()
  }
}
