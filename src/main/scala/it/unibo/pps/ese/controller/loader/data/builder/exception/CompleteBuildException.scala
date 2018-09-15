package it.unibo.pps.ese.controller.loader.data.builder.exception

class CompleteBuildException(val problems: String*) extends Exception(problems.mkString("\n")) {

  def +:(exception: CompleteBuildException): CompleteBuildException= {
    new CompleteBuildException(exception.problems ++ problems:_*)
  }

  def :+(exception: CompleteBuildException): CompleteBuildException= {
    new CompleteBuildException(problems ++ exception.problems:_*)
  }
  
}