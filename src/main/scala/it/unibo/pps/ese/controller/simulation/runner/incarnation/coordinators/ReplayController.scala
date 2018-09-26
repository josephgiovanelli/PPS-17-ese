package it.unibo.pps.ese.controller.simulation.runner.incarnation.coordinators

import it.unibo.pps.ese.controller.simulation.runner.incarnation.watchers.Stalker
import it.unibo.pps.ese.model.dataminer.datamodel.ReadOnlyEntityRepository
import it.unibo.pps.ese.view.sections.statistics.ReplayView

/**
  * APIs contained in this Trait allow to manage the functionalities related to the Replay function
  */
trait ReplayController {

  /**
    * Initialize the Replay Controller, linking the target entity
    * @param entityId The entity identifier
    */
  def initialize(entityId: String): Unit

  /**
    * Free controller resources
    */
  def dispose(): Unit

  /**
    * Attach a view module. Replay data will be sent to attached views at the end of every replay cycle
    * @param replayView The View to be attached
    */
  def attachView(replayView: ReplayView)
}
object ReplayController {

  /**
    * @param consolidatedState The data source used to fetch historical info of the selected entity
    * @return A ReplayController instance
    */
  def apply(consolidatedState: ReadOnlyEntityRepository): ReplayController = new BaseReplayController(consolidatedState)

  private class BaseReplayController(consolidatedState: ReadOnlyEntityRepository) extends ReplayController {

    private[this] val _stalker = Stalker(consolidatedState)
    private[this] var _replayViewList: Seq[ReplayView] = Seq empty
    private[this] var _timer: Option[java.util.Timer] = None

    consolidatedState attachNewDataListener(era => _stalker informAboutTrueEra era)

    override def initialize(entityId: String): Unit = {
      if (_timer isDefined) throw new IllegalStateException("Replay already running")
      _stalker stalk entityId
      _timer = Some(new java.util.Timer())
      val task = new java.util.TimerTask {
        def run(): Unit = {
          _replayViewList foreach (y => y render((_stalker currentEra, _stalker report)))
        }
      }
      (_timer get) scheduleAtFixedRate(task, 0, 250)
    }

    override def dispose(): Unit = if (_timer isDefined) (_timer get) cancel()

    override def attachView(replayView: ReplayView): Unit = _replayViewList = _replayViewList :+ replayView
  }
}
