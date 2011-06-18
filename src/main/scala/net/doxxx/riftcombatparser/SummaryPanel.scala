package net.doxxx.riftcombatparser

import swing.event.Event

trait SummaryPanel {
  def title: String
  final def update(summary: Map[String, Summary]) {
    val oldActor = selectedActor
    updateImpl(summary)
    if (oldActor.isDefined) selectActor(oldActor.get)
  }
  protected def updateImpl(summary: Map[String, Summary])

  def selectedActor: Option[String]
  def selectActor(actor: String)
}

case class SelectedActorChanged(actor: String) extends Event
