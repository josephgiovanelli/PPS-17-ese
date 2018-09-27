package it.unibo.pps.ese.controller.simulation.loader.data.builder.entities

import it.unibo.pps.ese.controller.simulation.loader._
import it.unibo.pps.ese.controller.simulation.loader.data.AnimalData.{CompleteAnimalData, PartialAnimalData}
import it.unibo.pps.ese.controller.simulation.loader.data.CustomGeneData.{CompleteCustomGeneData, PartialCustomGeneData}
import it.unibo.pps.ese.controller.simulation.loader.data.DefaultGeneData.{CompleteDefaultGeneData, PartialDefaultGeneData}
import it.unibo.pps.ese.controller.simulation.loader.data.GeneData.CompleteGeneData
import it.unibo.pps.ese.controller.simulation.loader.data._
import it.unibo.pps.ese.controller.simulation.loader.data.builder.{BaseGenericBuilder, GenericBuilder, ValidStatusGenericBuilder}
import it.unibo.pps.ese.controller.simulation.loader.data.builder.entities.EntityStatus._
import it.unibo.pps.ese.controller.simulation.loader.data.builder.exception.{CompleteBuildException, InvalidParamValueBuildException}
import it.unibo.pps.ese.controller.simulation.loader.data.builder.gene.{CustomGeneBuilder, DefaultGeneBuilder, GeneBuilder}

import scala.reflect.runtime.universe._
import scala.util.{Failure, Success, Try}
import it.unibo.pps.ese.utils.DefaultValidable.ValidableByDisequality._

/** Builder that can build a PartialAnimalData as partial data instance and a CompleteAnimalData as complete data
  * instance
  *
  * @tparam T Builder's current status
  */
trait AnimalBuilder[T <: EntityStatus] extends EntityBuilder[T] with GenericBuilder[T, FullAnimal, PartialAnimalData, CompleteAnimalData]{
  override type RET[A <: T] = AnimalBuilder[A]
  /** Set animal's typology
    *
    * @param typology Animal's typology
    * @return New builder with updated param and status
    */
  def setTypology(typology: String): AnimalBuilder[T with AnimalWithTypology]
  /** Add genes to animal's structural chromosome
    *
    * @param structuralChromosome Animal's custom genes as builders
    * @return New builder with updated param and status
    */
  def addStructuralChromosome(structuralChromosome: Iterable[CustomGeneBuilder[_]]): AnimalBuilder[T with AnimalWithStructChromosome]
  /** Add genes to animal's regulation chromosome
    *
    * @param regulationChromosome Animal's default genes as builders
    * @return New builder with updated param and status
    */
  def addRegulationChromosome(regulationChromosome: Iterable[DefaultGeneBuilder[_]]): AnimalBuilder[T with AnimalWithRegChromosome]
  /** Add genes to animal's sexual chromosome
    *
    * @param sexualChromosome Animal's default genes as builders
    * @return New builder with updated param and status
    */
  def addSexualChromosome(sexualChromosome: Iterable[DefaultGeneBuilder[_]]): AnimalBuilder[T with AnimalWithSexChromosome]
}

/** Factory object for [[it.unibo.pps.ese.controller.simulation.loader.data.builder.entities.AnimalBuilder]]*/
object AnimalBuilder {

  /** Create a new empty [[it.unibo.pps.ese.controller.simulation.loader.data.builder.entities.AnimalBuilder]]
    *
    * @return An empty [[it.unibo.pps.ese.controller.simulation.loader.data.builder.entities.AnimalBuilder]]
    */
  def apply(): AnimalBuilder[EmptyEntity] = new AnimalBuilderImpl[EmptyEntity](None, None, None, None, None, Seq(), Seq(), Seq())

  private[this] class AnimalBuilderImpl[T <: EntityStatus](_name: Option[String],
                                                           _geneLength: Option[Int],
                                                           _alleleLength: Option[Int],
                                                           _reign: Option[String],
                                                           _typology: Option[String],
                                                           _structuralChromosome: Iterable[CustomGeneBuilder[_]],
                                                           _regulationChromosome: Iterable[DefaultGeneBuilder[_]],
                                                           _sexualChromosome: Iterable[DefaultGeneBuilder[_]])
                                            (implicit val status: TypeTag[T])
    extends EntityBuilderImpl[T](_name, _geneLength, _alleleLength, _reign) with AnimalBuilder[T]
      with BaseGenericBuilder[T , FullAnimal, PartialAnimalData, CompleteAnimalData]
      with ValidStatusGenericBuilder[T , FullAnimal, PartialAnimalData, CompleteAnimalData, ValidEntity] {


    def setTypology(typology: String): AnimalBuilder[T with AnimalWithTypology] =
      new AnimalBuilderImpl(_name, _geneLength, _alleleLength, _reign, Some(typology), _structuralChromosome, _regulationChromosome,
        _sexualChromosome)

    def addStructuralChromosome(structuralChromosome: Iterable[CustomGeneBuilder[_]]): AnimalBuilder[T with AnimalWithStructChromosome] = {
      new AnimalBuilderImpl(_name, _geneLength, _alleleLength, _reign, _typology, structuralChromosome ++ _structuralChromosome, _regulationChromosome,
        _sexualChromosome)
    }

    def addRegulationChromosome(regulationChromosome: Iterable[DefaultGeneBuilder[_]]): AnimalBuilder[T with AnimalWithRegChromosome] = {
      new AnimalBuilderImpl(_name, _geneLength, _alleleLength, _reign, _typology, _structuralChromosome, regulationChromosome ++ _regulationChromosome,
        _sexualChromosome)
    }

    def addSexualChromosome(sexualChromosome: Iterable[DefaultGeneBuilder[_]]): AnimalBuilder[T with AnimalWithSexChromosome] = {
      new AnimalBuilderImpl(_name, _geneLength, _alleleLength, _reign, _typology, _structuralChromosome, _regulationChromosome,
        sexualChromosome ++ _sexualChromosome)
    }

    override def newInstance[NT <: EntityStatus](name: Option[String], geneLength: Option[Int],
                                                 alleleLength: Option[Int], reign: Option[String])
                                                (implicit tt: TypeTag[NT]): AnimalBuilder[NT] = {
      new AnimalBuilderImpl(name, geneLength, alleleLength, reign, _typology, _structuralChromosome, _regulationChromosome,
        _sexualChromosome)
    }

    def tryCompleteBuild(): Try[CompleteAnimalData] = {
      status.tpe match {
        case t if t <:< typeOf[FullAnimal] =>
          val check = checkComplete()
          val exception = checkProperties ++: check._1
          if(exception.isEmpty) {
            val ret: CompleteAnimalData = new AnimalDataImpl[CompleteCustomGeneData, CompleteDefaultGeneData](_name.get, _geneLength, _alleleLength,
              _reign, _typology, check._2, check._3, check._4) with CompleteAnimalData
            Success(ret)
          } else {
            Failure(exception.get)
          }
        case t if t <:< typeOf[ValidEntity] =>
          Failure(CompleteBuildException("Animal: " + _name + "must have all fields set"))
      }
    }

    private def checkComplete(): (Option[CompleteBuildException], Iterable[CompleteCustomGeneData], Iterable[CompleteDefaultGeneData], Iterable[CompleteDefaultGeneData]) ={
      var exception: Option[CompleteBuildException] = None
      val checkStruct = checkChromosome(_structuralChromosome, "structural")
      exception = exception ++: checkStruct._1
      val struct = checkStruct._2.asInstanceOf[Iterable[CompleteCustomGeneData]]
      val effectedProperties = struct.flatMap(_.conversionMap.values.flatMap(_.keySet)).toSet
      if(!(effectedProperties == AnimalStructuralProperties.elements.map(_.name))) {
        var exceptions: Seq[CompleteBuildException] = Seq()
        val notEffected = AnimalStructuralProperties.elements.map(_.name) -- effectedProperties
        exceptions = exceptions ++ notEffected.map(e => CompleteBuildException("Quality: " + e + " not effected"))
        val extraEffected = effectedProperties -- AnimalStructuralProperties.elements.map(_.name)
        exceptions = exceptions ++ extraEffected.map(e => CompleteBuildException("Quality: " + e + " effected, but not exists or isn't accessible"))
        exception = exception ++: CompleteBuildException("Animal: "+ _name.get +" | Regulation chromosome mus effect all and only animal's base qualities", exceptions)
      }
      val regCheck = checkDefaultChromosome(_regulationChromosome, "regulation", RegulationDefaultGenes.elements.toSeq.toSet)
      exception = exception ++: regCheck._1
      val sexCheck = checkDefaultChromosome(_sexualChromosome, "sexual", SexualDefaultGenes.elements.toSeq.toSet)
      exception = exception ++: sexCheck._1
      (exception, struct, regCheck._2, sexCheck._2)
    }

    private def checkChromosome(chromosome: Iterable[GeneBuilder[_]], chromosomeName: String): (Option[CompleteBuildException], Iterable[CompleteGeneData]) = {
      var exception: Option[CompleteBuildException] = None
      val tries: Iterable[Try[CompleteGeneData]] = chromosome.map(_.tryCompleteBuild())
      val complete: Iterable[CompleteGeneData] = tries.collect({case Success(value) => value})
      if(complete.size != chromosome.size) {
        exception = exception ++: CompleteBuildException("Animal: "+ _name.get +" | All " + chromosomeName + " chromosome's genes must be complete",
          tries.collect({case Failure(value: CompleteBuildException) => value}))
      }
      if(complete.size != complete.toSet.size)
        exception = exception ++: CompleteBuildException("Animal: "+ _name.get +" | " + chromosomeName.capitalize + " chromosome contains duplicated genes")
      (exception, complete)
    }

    private def checkDefaultChromosome(chromosome: Iterable[DefaultGeneBuilder[_]], chromosomeName: String,
                                       defaultElements: Set[DefaultGene]): (Option[CompleteBuildException], Iterable[CompleteDefaultGeneData]) = {
      var exception: Option[CompleteBuildException] = None
      val check = checkChromosome(chromosome, chromosomeName)
      exception = exception ++: check._1
      val complete = check._2.asInstanceOf[Iterable[CompleteDefaultGeneData]]
      val chromosomeElements = complete.map(c => (c.name, c.properties)).toSet
      val expectedChromosomeElements = defaultElements.map(g => (g.name, g.properties))
      if(chromosomeElements != expectedChromosomeElements) {
        var exceptions: Seq[CompleteBuildException] = Seq()
        exceptions = exceptions ++ (expectedChromosomeElements -- chromosomeElements).map(expected =>
          CompleteBuildException("Gene with " + expected._1 + " name and " + expected._2.keySet + " properties missing"))
        exceptions = exceptions ++ (chromosomeElements -- expectedChromosomeElements).map(notExpected =>
          CompleteBuildException("Gene with " + notExpected._1 + " name and " + notExpected._2.keySet + " is not a default gene"))
        exception = exception ++: CompleteBuildException("Animal: "+ _name.get +" | " + chromosomeName.capitalize +
          " chromosome must contain only predefined genes", exceptions)
      }
      val genesWithNotCorrectId = complete.filter(!_.getId.forall(_.length == _geneLength.getOrElse(0)))
      if(genesWithNotCorrectId.nonEmpty) {
        val exceptions = genesWithNotCorrectId.map(g => CompleteBuildException("Gene " + g.name + " has incorrect id: " + g.id))
        exception = exception ++: CompleteBuildException("Animal: "+ _name.get +" | " + chromosomeName.capitalize +
          " chromosome must contain only genes with id's length " + _geneLength, exceptions)
      }
      val allelesWithNotCorrectId = complete.flatMap(_.alleles).filter(a => !(a.id.length == _alleleLength.getOrElse(0)))
      if(allelesWithNotCorrectId.nonEmpty) {
        val exceptions = allelesWithNotCorrectId.map(a => CompleteBuildException("Gene " + a.gene + " Allele " + a.id + " has incorrect id: " + a.id))
        exception = exception ++: CompleteBuildException("Animal: "+ _name.get +" | " + chromosomeName.capitalize +
          " chromosome must contain only genes with allele's id's length " + _alleleLength, exceptions)
      }
      (exception, complete)
    }

    override def checkProperties: Option[CompleteBuildException] = {
      var exception = super.checkProperties
      if(!_reign.contains(Reigns.ANIMALS.code))
        exception = exception ++: InvalidParamValueBuildException("Animal: " + _name.getOrElse(""), "reign", _reign)
      if(!_typology.isValid(t => Typologies.elements.map(_.code).contains(t)))
        exception = exception ++: InvalidParamValueBuildException("Animal: " + _name.getOrElse(""), "typology", _typology)
      if(!_structuralChromosome.isValid())
        exception = exception ++: InvalidParamValueBuildException("Animal: " + _name.getOrElse(""), "structuralChromosome", _structuralChromosome)
      if(!_regulationChromosome.isValid())
        exception = exception ++: InvalidParamValueBuildException("Animal: " + _name.getOrElse(""), "regulationChromosome", _regulationChromosome)
      if(!_sexualChromosome.isValid())
        exception = exception ++: InvalidParamValueBuildException("Animal: " + _name.getOrElse(""), "sexualChromosome", _sexualChromosome)
      exception
    }

    override protected def buildPartialInstance(): PartialAnimalData =
      new AnimalDataImpl(_name.get, _geneLength, _alleleLength, _reign, _typology, _structuralChromosome.map(_.build()),
        _regulationChromosome.map(_.build()), _sexualChromosome.map(_.build()))
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

    val getTypology: Option[String] = _getTypology.normalize()
    val getStructuralChromosome: Option[Iterable[C]] = _getStructuralChromosome.boxToValidOption()
    val getRegulationChromosome: Option[Iterable[D]] = _getRegulationChromosome.boxToValidOption()
    val getSexualChromosome: Option[Iterable[D]] = _getSexualChromosome.boxToValidOption()
  }
}
