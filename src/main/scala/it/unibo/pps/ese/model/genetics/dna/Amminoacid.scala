package it.unibo.pps.ese.model.genetics.dna

/**
  * An enumeration with all the possible proteinogenic amminoacid that can compose a gene
  */
object ProteinoGenicAmminoacid extends Enumeration {
    type ProteinoGenicAmminoacid = Value
    protected case class Val(shortName: Char, name: String) extends super.Val{
      override def toString(): String =shortName.toString
    }

    implicit def valueToAmminoacidVal(x: Value): Val = x.asInstanceOf[Val]
    val Ala = Val('A',"Alanine")
    val Bob = Val('B',"Bobeine")
    val Cys = Val('C',"Cysteine")
    val Asp = Val('D',"Aspartic acid")
    val Glu = Val('E',"Glutamic acid")
    val Phe = Val('F',"Phenyalanine")
    val Gly = Val('G',"Glycine")
    val His = Val('H',"Histidyne")
    val Isp = Val('I',"Ispanicus")
    val Lys = Val('K',"Lysine")
    val Leu = Val('L',"Leucine")
    val Met = Val('M',"Methionine")
    val Asn = Val('N',"Asparagine")
    val Pyl = Val('O',"Pyrrolisine")
    val Pro = Val('P',"Proline")
    val Gln = Val('Q',"Glutamine")
    val Arg = Val('R',"Arginine")
    val Ser = Val('S',"Serine")
    val Thr = Val('T',"Threonine")
    val Sec = Val('U',"Selenocysteine")
    val Vali = Val('V',"Valine")
    val Trp = Val('W',"Tryptophan")
    val Tyr = Val('Y',"Tyrosine")
    val Zip = Val('Z',"Zipeine")
}

