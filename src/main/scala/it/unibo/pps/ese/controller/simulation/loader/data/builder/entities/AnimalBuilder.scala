package it.unibo.pps.ese.controller.simulation.loader.data.builder.entities

import it.unibo.pps.ese.controller.simulation.loader.data.AnimalData.{CompleteAnimalData, PartialAnimalData}
import it.unibo.pps.ese.controller.simulation.loader.data.CustomGeneData.{CompleteCustomGeneData, PartialCustomGeneData}
import it.unibo.pps.ese.controller.simulation.loader.data.DefaultGeneData.{CompleteDefaultGeneData, PartialDefaultGeneData}
import it.unibo.pps.ese.controller.simulation.loader.data._
import it.unibo.pps.ese.controller.simulation.loader.data.builder.{GenericBuilder, ValidStatusGenericBuilder}
import it.unibo.pps.ese.controller.simulation.loader.data.builder.entities.EntityStatus._
import it.unibo.pps.ese.controller.simulation.loader.data.builder.exception.{CompleteBuildException, InvalidParamValueBuildException}
import it.unibo.pps.ese.controller.simulation.loader.data.builder.gene.{CustomGeneBuilder, DefaultGeneBuilder}

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

  def apply(): AnimalBuilder[EmptyEntity] = AnimalBuilder[EmptyEntity](None, None, None, None, None, Seq(), Seq(), Seq())

  private def apply[T <: EntityStatus: TypeTag](name: Option[String],
                                       geneLength: Option[Int],
                                       alleleLength: Option[Int],
                                       reign: Option[String],
                                       typology: Option[String],
                                       structuralChromosome: Iterable[CustomGeneBuilder[_]],
                                       regulationChromosome: Iterable[DefaultGeneBuilder[_]],
                                       sexualChromosome: Iterable[DefaultGeneBuilder[_]]): AnimalBuilder[T] =
    new AnimalBuilderImpl[T](name, geneLength, alleleLength, reign, typology, structuralChromosome,
      regulationChromosome, sexualChromosome) with ValidStatusGenericBuilder[T , FullAnimal, PartialAnimalData, CompleteAnimalData, ValidEntity]

  private class AnimalBuilderImpl[T <: EntityStatus](name: Option[String],
                                             geneLength: Option[Int],
                                             alleleLength: Option[Int],
                                             reign: Option[String],
                                             typology: Option[String],
                                             structuralChromosome: Iterable[CustomGeneBuilder[_]],
                                             regulationChromosome: Iterable[DefaultGeneBuilder[_]],
                                             sexualChromosome: Iterable[DefaultGeneBuilder[_]])
                                            (implicit val status: TypeTag[T]) extends EntityBuilderImpl[T](name, geneLength, alleleLength, reign) with AnimalBuilder[T] {


    def setTypology(typology: String): AnimalBuilder[T with AnimalWithTypology] =
      AnimalBuilder(name, geneLength, alleleLength, reign, Some(typology), structuralChromosome, regulationChromosome,
        sexualChromosome)

    def addStructuralChromosome(structuralChromosome: Iterable[CustomGeneBuilder[_]]): AnimalBuilder[T with AnimalWithStructChromosome] = {
      AnimalBuilder(name, geneLength, alleleLength, reign, typology, structuralChromosome, regulationChromosome,
        sexualChromosome)
    }

    def addRegulationChromosome(regulationChromosome: Iterable[DefaultGeneBuilder[_]]): AnimalBuilder[T with AnimalWithRegChromosome] = {
      AnimalBuilder(name, geneLength, alleleLength, reign, typology, structuralChromosome, regulationChromosome,
        sexualChromosome)
    }

    def addSexualChromosome(sexualChromosome: Iterable[DefaultGeneBuilder[_]]): AnimalBuilder[T with AnimalWithSexChromosome] = {
      AnimalBuilder(name, geneLength, alleleLength, reign, typology, structuralChromosome, regulationChromosome,
        sexualChromosome)
    }

    override def newInstance[NT <: EntityStatus](name: Option[String], geneLength: Option[Int],
                                                 alleleLength: Option[Int], reign: Option[String])
                                                (implicit tt: TypeTag[NT]): AnimalBuilder[NT] = {
      AnimalBuilder(name, geneLength, alleleLength, reign, typology, structuralChromosome, regulationChromosome,
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

    def build(): PartialAnimalData = {
      tryCompleteBuild() match {
        case Success(value) =>
          value
        case Failure(_) =>
          new AnimalDataImpl(name.get, geneLength, alleleLength, reign, typology, structuralChromosome.map(_.build()),
            regulationChromosome.map(_.build()), sexualChromosome.map(_.build()))
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
  }

  private class AnimalDataImpl[C<:PartialCustomGeneData, D<:PartialDefaultGeneData](val name: String,
                                                                                    val getGeneLength: Option[Int],
                                                                                    val getAlleleLength: Option[Int],
                                                                                    val getReign: Option[String],
                                                                                    val getTypology: Option[String],
                                                                                    _getStructuralChromosome: Iterable[C],
                                                                                    _getRegulationChromosome: Iterable[D],
                                                                                    _getSexualChromosome: Iterable[D]) extends AnimalData[C, D] {
    val getStructuralChromosome: Option[Iterable[C]] = if(_getStructuralChromosome.isEmpty) None else Some(_getStructuralChromosome)
    val getRegulationChromosome: Option[Iterable[D]] = if(_getRegulationChromosome.isEmpty) None else Some(_getRegulationChromosome)
    val getSexualChromosome: Option[Iterable[D]] = if(_getSexualChromosome.isEmpty) None else Some(_getSexualChromosome)
  }
}
