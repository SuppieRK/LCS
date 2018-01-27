package io.suppie.lcs.rl

import java.util.Random
import java.{lang => jl, util => ju}

import io.suppie.lcs.rl.environment.pomdp.PomdpAgent
import io.suppie.lcs.rl.environment.{Action, Consequence, DeterministicAction, PomdpEnvironmentExecutor}

import scala.collection.JavaConverters._

object ScalaPomdpTest extends App {
  val agent = PomdpAgent(
    TestExecutor(),
    episodesAmount = 1000,
    episodeLength = 100
  )

  agent.getPolicy.asScala.foreach { entry =>
    println(s"${entry._1.asScala.toList.head} -> ${entry._2.asInstanceOf[TestAction].newState.asScala.toList.head}")
  }
}

class TestAction(val newState: ju.HashMap[String, jl.Double]) extends DeterministicAction

case class TestExecutor() extends PomdpEnvironmentExecutor {
  lazy val rng: Random = new Random(System.currentTimeMillis())

  lazy val a0: TestAction = new TestAction(makeSingletonMap("test", 0.0))
  lazy val a1: TestAction = new TestAction(makeSingletonMap("test", 1.0))
  lazy val a2: TestAction = new TestAction(makeSingletonMap("test", 2.0))
  lazy val a3: TestAction = new TestAction(makeSingletonMap("test", 3.0))
  lazy val a4: TestAction = new TestAction(makeSingletonMap("test", 4.0))
  lazy val a5: TestAction = new TestAction(makeSingletonMap("test", 5.0))

  lazy val consequenceMap: Map[TestAction, Consequence] = Map(
    a0 -> Consequence(a0, 0.0),
    a1 -> Consequence(a1, 0.0),
    a2 -> Consequence(a2, 0.0),
    a3 -> Consequence(a3, 0.0),
    a4 -> Consequence(a4, 0.0),
    a5 -> Consequence(a5, 100.0)
  )

  lazy val environment = Map(
    a0.newState -> List(a4),
    a1.newState -> List(a3, a5),
    a2.newState -> List(a3),
    a3.newState -> List(a1, a2, a4),
    a4.newState -> List(a0, a3, a5),
    a5.newState -> List(a1, a4, a5)
  )

  val goalStates = Set(a5.newState)

  var currentState: ju.HashMap[String, jl.Double] = a0.newState

  override def doAction(action: Action): Consequence = action match {
    case ta: TestAction =>
      currentState = ta.newState
      consequenceMap(ta)
    case _ => throw new Exception("Unknown action")
  }

  override def getAndSetRandomState: ju.HashMap[String, jl.Double] = {
    val p: List[ju.HashMap[String, jl.Double]] = environment.keys.toList
    currentState = p(rng.nextInt(p.size))
    currentState
  }

  override def getCurrentState: ju.HashMap[String, jl.Double] = currentState

  override def isCurrentStateGoal: jl.Boolean = goalStates.contains(currentState)

  override def getAvailableActions: List[Action] = environment(currentState)

  private def makeSingletonMap(key: String, value: jl.Double): ju.HashMap[String, jl.Double] = {
    val result: ju.HashMap[String, jl.Double] = new ju.HashMap[String, jl.Double]()
    result.put(key, value)
    result
  }
}