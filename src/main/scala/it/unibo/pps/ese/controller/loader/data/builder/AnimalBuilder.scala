package it.unibo.pps.ese.controller.loader.data.builder

import it.unibo.pps.ese.controller.loader.data.AnimalData.{CompleteAnimalData, PartialAnimalData}
import it.unibo.pps.ese.controller.loader.data.CustomGeneData.{CompleteCustomGeneData, PartialCustomGeneData}
import it.unibo.pps.ese.controller.loader.data.DefaultGeneData.{CompleteDefaultGeneData, PartialDefaultGeneData}
import it.unibo.pps.ese.controller.loader.data._
import it.unibo.pps.ese.controller.loader.data.builder.AnimalBuilder.AnimalStatus
import it.unibo.pps.ese.controller.loader.data.builder.AnimalBuilder.AnimalStatus._

import scala.reflect.runtime.universe._

trait AnimalBuilder[T <: AnimalStatus] {
  def setName(name: String): AnimalBuilder[T with AnimalWithName]
  def setGeneLength(geneLength: Int): AnimalBuilder[T with AnimalWithGeneLength]
  def setAlleleLength(alleleLength: Int): AnimalBuilder[T with AnimalWithAlleleLength]
  def setReign(reign: String): AnimalBuilder[T with AnimalWithReign]
  def setTypology(typology: String): AnimalBuilder[T with AnimalWithTypology]
  def addStructuralChromosome(structuralChromosome: Iterable[GeneBuilder[_]]): AnimalBuilder[T with AnimalWithStructChromosome]
  def addRegulationChromosome(regulationChromosome: Iterable[GeneBuilder[_]]): AnimalBuilder[T with AnimalWithRegChromosome]
  def addSexualChromosome(sexualChromosome: Iterable[GeneBuilder[_]]): AnimalBuilder[T with AnimalWithSexChromosome]
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
                                             structuralChromosome: Iterable[_ <: PartialCustomGeneData],
                                             regulationChromosome: Iterable[_ <: PartialDefaultGeneData],
                                             sexualChromosome: Iterable[_ <: PartialDefaultGeneData])
                                            (implicit val status: TypeTag[T]) extends AnimalBuilder[T]{
    //TODO use copy method?
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

    def addStructuralChromosome(structuralChromosome: Iterable[GeneBuilder[_]]): AnimalBuilder[T with AnimalWithStructChromosome] = {
      new AnimalBuilderImpl(name, geneLength, alleleLength, reign, typology, structuralChromosome.map(_.buildCustom), regulationChromosome,
        sexualChromosome)
    }

    def addRegulationChromosome(regulationChromosome: Iterable[GeneBuilder[_]]): AnimalBuilder[T with AnimalWithRegChromosome] = {
      new AnimalBuilderImpl(name, geneLength, alleleLength, reign, typology, structuralChromosome, regulationChromosome.map(_.buildDefault),
        sexualChromosome)
    }

    def addSexualChromosome(sexualChromosome: Iterable[GeneBuilder[_]]): AnimalBuilder[T with AnimalWithSexChromosome] = {
      new AnimalBuilderImpl(name, geneLength, alleleLength, reign, typology, structuralChromosome, regulationChromosome,
        sexualChromosome.map(_.buildDefault))
    }

    def buildComplete(implicit ev: T =:= FullAnimal): CompleteAnimalData = {
      val check = checkComplete()
      check._1.foreach(throw _)
      new AnimalDataImpl(name.get, geneLength, alleleLength, reign, typology, check._2, check._3, check._4) with FullAnimalData[CompleteCustomGeneData, CompleteDefaultGeneData]
    }

    private def checkComplete(): (Option[Exception], Iterable[CompleteCustomGeneData], Iterable[CompleteDefaultGeneData], Iterable[CompleteDefaultGeneData]) ={
      //TODO concat like list Nil :: ecc...
      var exception: Exception = null
      val struct: Iterable[CompleteCustomGeneData] = structuralChromosome.flatMap({
        case c: CompleteCustomGeneData =>
          Some(c)
        case _ =>
          println("Wrong struct")
          None
      })
      if(struct.size != structuralChromosome.size) {
        exception = new IllegalStateException()
      }
      val reg: Iterable[CompleteDefaultGeneData] = regulationChromosome.flatMap({
        case c: CompleteDefaultGeneData =>
          Some(c)
        case _ =>
          println("Wrong reg")
          None
      })
      if(reg.size != regulationChromosome.size)
        exception = new IllegalStateException()
      val sex: Iterable[CompleteDefaultGeneData] = sexualChromosome.flatMap({
        case c: CompleteDefaultGeneData =>
          Some(c)
        case _ =>
          println("Wrong sex")
          None
      })
      if(sex.size != sexualChromosome.size)
        exception = new IllegalStateException()
      if(exception == null) {
        (None, struct, reg, sex)
      } else {
        (Some(exception), Seq(), Seq(), Seq())
      }
    }

    def build(): PartialAnimalData = {
      //require(status.tpe <:< st.tpe)
      status.tpe match {
        case t if t <:< typeOf[FullAnimal] =>
          val check = checkComplete()
          if(check._1.isEmpty) {
            new AnimalDataImpl[CompleteCustomGeneData, CompleteDefaultGeneData](name.get, geneLength, alleleLength, reign, typology, check._2, check._3, check._4)
              with FullAnimalData[CompleteCustomGeneData, CompleteDefaultGeneData]
          } else {
            new AnimalDataImpl(name.get, geneLength, alleleLength, reign, typology, structuralChromosome,
              regulationChromosome, sexualChromosome)
          }
        case t if t <:< typeOf[ValidAnimal] =>
          new AnimalDataImpl(name.get, geneLength, alleleLength, reign, typology, structuralChromosome,
            regulationChromosome, sexualChromosome)
      }
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
