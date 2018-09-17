package it.unibo.pps.ese.controller.loader.data.builder

import it.unibo.pps.ese.controller.loader.data.AnimalData.{CompleteAnimalData, PartialAnimalData}
import it.unibo.pps.ese.controller.loader.data.CustomGeneData.{CompleteCustomGeneData, PartialCustomGeneData}
import it.unibo.pps.ese.controller.loader.data.DefaultGeneData.{CompleteDefaultGeneData, PartialDefaultGeneData}
import it.unibo.pps.ese.controller.loader.data._
import it.unibo.pps.ese.controller.loader.data.builder.AnimalBuilder.AnimalStatus
import it.unibo.pps.ese.controller.loader.data.builder.AnimalBuilder.AnimalStatus._
import it.unibo.pps.ese.controller.loader.data.builder.exception.CompleteBuildException
import it.unibo.pps.ese.controller.loader.data.builder.gene.{CustomGeneBuilder, DefaultGeneBuilder}

import scala.reflect.runtime.universe._
import scala.util.{Failure, Success, Try}

trait AnimalBuilder[T <: AnimalStatus] {
  def setName(name: String): AnimalBuilder[T with AnimalWithName]
  def setGeneLength(geneLength: Int): AnimalBuilder[T with AnimalWithGeneLength]
  def setAlleleLength(alleleLength: Int): AnimalBuilder[T with AnimalWithAlleleLength]
  def setReign(reign: String): AnimalBuilder[T with AnimalWithReign]
  def setTypology(typology: String): AnimalBuilder[T with AnimalWithTypology]
  def addStructuralChromosome(structuralChromosome: Iterable[CustomGeneBuilder[_]]): AnimalBuilder[T with AnimalWithStructChromosome]
  def addRegulationChromosome(regulationChromosome: Iterable[DefaultGeneBuilder[_]]): AnimalBuilder[T with AnimalWithRegChromosome]
  def addSexualChromosome(sexualChromosome: Iterable[DefaultGeneBuilder[_]]): AnimalBuilder[T with AnimalWithSexChromosome]
  def tryBuildComplete(): Try[CompleteAnimalData]
  def buildComplete(implicit ev: T =:= FullAnimal): CompleteAnimalData
  def build(): PartialAnimalData
}

object AnimalBuilder {

  def apply(): AnimalBuilder[EmptyAnimal] = new AnimalBuilderImpl[EmptyAnimal](None, None, None, None, None, Seq(), Seq(), Seq())

  private class AnimalBuilderImpl[T <: AnimalStatus](name: Option[String],
                                             geneLength: Option[Int],
                                             alleleLength: Option[Int],
                                             reign: Option[String],
                                             typology: Option[String],
                                             structuralChromosome: Iterable[CustomGeneBuilder[_]],
                                             regulationChromosome: Iterable[DefaultGeneBuilder[_]],
                                             sexualChromosome: Iterable[DefaultGeneBuilder[_]])
                                            (implicit val status: TypeTag[T]) extends AnimalBuilder[T]{

    def setName(name: String): AnimalBuilder[T with AnimalWithName] =
      new AnimalBuilderImpl(Some(name), geneLength, alleleLength, reign, typology, structuralChromosome, regulationChromosome,
        sexualChromosome)

    def setGeneLength(geneLength: Int): AnimalBuilder[T with AnimalWithGeneLength] =
      new AnimalBuilderImpl(name, Some(geneLength), alleleLength, reign, typology, structuralChromosome, regulationChromosome,
        sexualChromosome)

    def setAlleleLength(alleleLength: Int): AnimalBuilder[T with AnimalWithAlleleLength] =
      new AnimalBuilderImpl(name, geneLength, Some(alleleLength), reign, typology, structuralChromosome, regulationChromosome,
        sexualChromosome)

    def setReign(reign: String): AnimalBuilder[T with AnimalWithReign] =
      new AnimalBuilderImpl(name, geneLength, alleleLength, Some(reign), typology, structuralChromosome, regulationChromosome,
        sexualChromosome)

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

    def tryBuildComplete(): Try[CompleteAnimalData] = {
      status.tpe match {
        case t if t <:< typeOf[FullAnimal] =>
          val check = checkComplete()
          if(check._1.isEmpty) {
            val ret: CompleteAnimalData = new AnimalDataImpl[CompleteCustomGeneData, CompleteDefaultGeneData](name.get, geneLength, alleleLength,
              reign, typology, check._2, check._3, check._4) with CompleteAnimalData
            Success(ret)
          } else {
            Failure(check._1.get)
          }
        case t if t <:< typeOf[ValidAnimal] =>
          Failure(new CompleteBuildException("Animal: " + name + "must have all fields set"))
      }
    }

    def build(): PartialAnimalData = {
      //require(status.tpe <:< st.tpe)
      tryBuildComplete() match {
        case Success(value) =>
          value
        case Failure(_) =>
          new AnimalDataImpl(name.get, geneLength, alleleLength, reign, typology, structuralChromosome.map(_.build()),
            regulationChromosome.map(_.build()), sexualChromosome.map(_.build()))
      }
    }

    def buildComplete(implicit ev: T =:= FullAnimal): CompleteAnimalData = {
      tryBuildComplete() match {
        case Success(value) =>
          value
        case Failure(exception) =>
          throw exception
      }
    }

    private def checkComplete(): (Option[CompleteBuildException], Iterable[CompleteCustomGeneData], Iterable[CompleteDefaultGeneData], Iterable[CompleteDefaultGeneData]) ={
      var exception: Option[CompleteBuildException] = None
      val structTries: Iterable[Try[CompleteCustomGeneData]] = structuralChromosome.map(_.tryCompleteBuild())
      val struct: Iterable[CompleteCustomGeneData] = structTries.collect({case Success(value) => value})
      if(struct.size != structuralChromosome.size) {
        exception = exception ++: new CompleteBuildException("Gene: "+ name +" | All structural chromosome's genes must be complete",
          structTries.collect({case Failure(value: CompleteBuildException) => value}))
      }
      val regTries: Iterable[Try[CompleteDefaultGeneData]] = regulationChromosome.map(_.tryCompleteBuild())
      val reg: Iterable[CompleteDefaultGeneData] = regTries.collect({case Success(value) => value})
      if(reg.size != regulationChromosome.size) {
        exception = exception ++: new CompleteBuildException("Gene: "+ name +" | All regulation chromosome's genes must be complete",
          regTries.collect({case Failure(value: CompleteBuildException) => value}))
      }
      val sexTries: Iterable[Try[CompleteDefaultGeneData]] = sexualChromosome.map(_.tryCompleteBuild())
      val sex: Iterable[CompleteDefaultGeneData] = sexTries.collect({case Success(value) => value})
      if(sex.size != sexualChromosome.size) {
        exception = exception ++: new CompleteBuildException("Gene: "+ name +" | All sexual chromosome's genes must be complete",
          sexTries.collect({case Failure(value: CompleteBuildException) => value}))
      }
      (exception, struct, reg, sex)
    }
  }

  sealed trait AnimalStatus
  object AnimalStatus {
    sealed trait EmptyAnimal extends AnimalStatus
    sealed trait AnimalWithName extends AnimalStatus
    sealed trait AnimalWithGeneLength extends AnimalStatus
    sealed trait AnimalWithAlleleLength extends AnimalStatus
    sealed trait AnimalWithReign extends AnimalStatus
    sealed trait AnimalWithTypology extends AnimalStatus
    sealed trait AnimalWithStructChromosome extends AnimalStatus
    sealed trait AnimalWithRegChromosome extends AnimalStatus
    sealed trait AnimalWithSexChromosome extends AnimalStatus

    type ValidAnimal = EmptyAnimal with AnimalWithName
    type AnimalWithBaseInfo = ValidAnimal with AnimalWithGeneLength with AnimalWithAlleleLength
      with  AnimalWithReign with AnimalWithTypology
    type AnimalTemplate = AnimalWithBaseInfo with AnimalWithStructChromosome
    type FullAnimal = AnimalTemplate with AnimalWithRegChromosome with AnimalWithSexChromosome
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
