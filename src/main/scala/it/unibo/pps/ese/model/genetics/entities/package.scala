package it.unibo.pps.ese.model.genetics

package object entities {
  implicit class RichQuality (q1 :QualityType) {
    def |->|(v:Double):(QualityType,Quality) = q1 -> Quality(v,q1)
  }
}
