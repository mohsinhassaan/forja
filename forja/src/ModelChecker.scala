package forja

import scala.collection.mutable
import scala.concurrent.ExecutionContext

transparent trait ModelChecker:
  type State
  type ErrorState

  extension (state: State) def checkErrorState: Option[ErrorState]

  def initStates(using ExecutionContext): Iterator[State]
  def nextStates(state: State)(using ExecutionContext): Iterator[State]

  def assertCheck(): Unit =
    check() match
      case None                     => // ok
      case Some((errorState, path)) =>
        val builder = StringBuilder()
        path.foreach: state =>
          builder ++= state.toString()
          builder ++= "\n---\n"
        builder ++= errorState.toString()
        throw AssertionError(builder.result())
    end match
  end assertCheck

  def check()(using
      ctx: ExecutionContext = ExecutionContext.global,
  ): Option[(ErrorState, Seq[State])] =
    val stateQueue = mutable.Queue.from(initStates)
    val knownStates = mutable.HashMap.from[State, Option[State]](
      stateQueue.iterator.map(_ -> None),
    )

    var result: Option[(ErrorState, Seq[State])] = None

    while stateQueue.nonEmpty && result.isEmpty
    do
      val state = stateQueue.synchronized(stateQueue.dequeue())
      var hasNextStates = false
      nextStates(state).foreach: nextState =>
        hasNextStates = true
        knownStates.getOrElseUpdate(
          nextState, {
            stateQueue.enqueue(nextState)
            Some(state)
          },
        )

      if !hasNextStates
      then
        state.checkErrorState match
          case None             =>
          case Some(errorState) =>
            val path =
              Iterator
                .iterate(Some(state): Option[State]): stateOpt =>
                  stateOpt
                    .flatMap(knownStates.get)
                    .flatten
                .takeWhile(_.nonEmpty)
                .flatten
                .toSeq
                .reverse
            result = Some((errorState, path))
        end match
      end if
    end while

    result
  end check
end ModelChecker
