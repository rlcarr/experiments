package org.carreiro.experiments

trait SomeTrait {
  def code(): Int
  def greeting(): String
}

trait SomeOtherTrait extends SomeTrait {
  override def code(): Int = 33
  override def greeting(): String = "Dear Prudence"
}