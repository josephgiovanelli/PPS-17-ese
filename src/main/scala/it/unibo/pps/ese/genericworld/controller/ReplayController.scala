package it.unibo.pps.ese.genericworld.controller

import it.unibo.pps.ese.dataminer.ReadOnlyEntityRepository
import it.unibo.pps.ese.entitywatcher.Stalker
import it.unibo.pps.ese.view.statistics.ReplayView

trait ReplayController {
  def initialize(entityId: String): Unit
  def dispose(): Unit
  def attachView(replayView: ReplayView)
}
object ReplayController {
  def apply(consolidatedState: ReadOnlyEntityRepository): ReplayController = new BaseReplayController(consolidatedState)

  private class BaseReplayController(consolidatedState: ReadOnlyEntityRepository) extends ReplayController {

    private[this] val _stalker = new Stalker(consolidatedState)
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
