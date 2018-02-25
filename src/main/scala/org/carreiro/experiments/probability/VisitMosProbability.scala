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
  private final case class TrialParams(kids: Int,
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
    * Creates a visit set for each kid, then computes the number of visit
    * slots all the kids were present at.
    *
    * @param params the trial params
    * @return the number of visit slots all the kids were at
    */
  def timesAllAreThere(params: TrialParams): Int = {
    val visits = Seq.fill(params.kids)(createVisits(params))
    intersectSets(visits).size
  }

  /**
    * Repeats a trial, returning a sequence of trial outcomes.
    *
    * @param numTrials number of trials to run
    * @param params the parameters for each trial
    * @param aTrial the trial to be run
    * @return a sequence of trial outcomes
    */
  def trials(numTrials: Int,
             params: TrialParams
            )(aTrial: TrialParams => Int): Seq[Int] = {

    Seq.fill(numTrials)(aTrial(params))
  }

  def numTimesAllAreThereByFormula(params: TrialParams): Double = {
    params.periodLength *
      math.pow(params.visitsInPeriod.toDouble / params.periodLength.toDouble, params.kids)
  }

  def numTimesAllAreThereByExperiment(numTrials: Int, params: TrialParams): Double = {
    val theTrials = trials(numTrials, params)(timesAllAreThere)
    theTrials.sum.toDouble / theTrials.size.toDouble
  }

  def numTimesAllIsThereGivenOneIsThereByFormula(params: TrialParams): Double = {
    numTimesAllAreThereByFormula(params.copy(kids = params.kids - 1))
  }

  def numTimesAllIsThereGivenOneIsThereByExperiment(numTrials: Int,
                                                    params: TrialParams): Double = {

    val theTrials = trials(numTrials, params)
    theTrials.sum.toDouble / theTrials.size.toDouble
  }

  /**
    * Compute the intersection of all the input sets.
    *
    * @param sets sets to compute the total intersection of
    * @tparam A type of element in the sets
    * @return the intersection of all the sets
    */
  def intersectSets[A](sets: Seq[Set[A]]): Set[A] = {
    sets.reduceLeft((lhs, rhs) => lhs.intersect(rhs))
  }
}
