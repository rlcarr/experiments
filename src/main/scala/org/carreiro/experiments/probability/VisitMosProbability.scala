package org.carreiro.experiments.probability

import java.security.SecureRandom

import scala.collection.mutable

object VisitMosProbability {
  private val random = new SecureRandom()

  /**
    * Parameters for a trial.
    *
    * @param kids           number of kids in the trial
    * @param visitsInPeriod how often each kid visits in a period
    * @param periodLength   number of possible visits in a period
    */
  final case class TrialParams(kids: Int,
                               visitsInPeriod: Int,
                               periodLength: Int)

  /**
    * Creates the set of visits for one kid. This is a set
    * of the visit slots the child visited.
    *
    * @param params the trial params
    * @return the set of visits for one kid
    */
  def createVisits(params: TrialParams): Set[Int] = {
    val visits = mutable.Set.empty[Int]
    while (visits.size < params.visitsInPeriod) {
      visits += (random.nextInt(params.periodLength) + 1)
    }

    visits.toSet
  }

  /**
    * Creates a visit set for each kid, then computes the visit
    * slots all the kids were present at.
    *
    * @param params the trial params
    * @return the visit slots all the kids were at
    */
  def slotsWhenAllAreThere(params: TrialParams): Set[Int] = {
    val visits = Seq.fill(params.kids)(createVisits(params))
    intersectSets(visits)
  }

  def fractionOfOnesVisitsWhenAllAreThere(params: TrialParams): Double = {
    slotsWhenAllAreThere(params).size.toDouble / params.visitsInPeriod
  }

  /**
    * Repeats a trial, returning a sequence of trial outcomes.
    *
    * @param numTrials number of trials to run
    * @param params    the parameters for each trial
    * @param aTrial    the trial to be run
    * @return a sequence of trial outcomes
    */
  def trials[A](numTrials: Int,
                params: TrialParams
               )(aTrial: TrialParams => A): Seq[A] = {

    Seq.fill(numTrials)(aTrial(params))
  }

  def numSlotsAllAreThereByFormula(params: TrialParams): Double = {
    params.periodLength *
      math.pow(params.visitsInPeriod.toDouble / params.periodLength.toDouble, params.kids)
  }

  def numSlotsAllAreThereByExperiment(numTrials: Int, params: TrialParams): Double = {
    val theTrials = trials(numTrials, params)(slotsWhenAllAreThere).map(_.size)
    mean(theTrials)
  }

  def numSlotsAllAreThereGivenOneIsThereByFormula(params: TrialParams): Double = {
    params.visitsInPeriod *
      math.pow(params.visitsInPeriod.toDouble / params.periodLength.toDouble, params.kids - 1)
  }

  def numSlotsAllAreThereGivenOneIsThereByExperiment(numTrials: Int,
                                                     params: TrialParams): Double = {

    val theTrials = trials(numTrials, params)(fractionOfOnesVisitsWhenAllAreThere)
    mean(theTrials) * params.visitsInPeriod
  }

  /**
    * Compute the intersection of all the input sets.
    *
    * @param sets sets to compute the total intersection of
    * @tparam A type of element in the sets
    * @return the intersection of all the sets
    */
  def intersectSets[A](sets: Seq[Set[A]]): Set[A] = {
    sets.reduceLeft((l, r) => l.intersect(r))
  }

  private def mean[A](ts: Iterable[A])(implicit converter: Numeric[A]): Double = {
    converter.toDouble(ts.sum) / ts.size
  }
}
