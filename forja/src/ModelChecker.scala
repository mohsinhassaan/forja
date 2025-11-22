package forja

import scala.collection.mutable
import scala.concurrent.ExecutionContext

transparent trait ModelChecker:
  type State

  extension (state: State) def isErrorState: Boolean
  end extension

  def initStates(using ExecutionContext): Iterator[State]
  def nextStates(state: State)(using ExecutionContext): Iterator[State]

  def check()(using
      ctx: ExecutionContext = ExecutionContext.global,
  ): Option[Seq[State]] =
    val stateQueue = mutable.Queue.from(initStates)
    val knownStates = mutable.HashMap.from[State, Option[State]](
      stateQueue.iterator.map(_ -> None),
    )

    var result: Option[Seq[State]] = None

    while stateQueue.nonEmpty && result.isEmpty
    do
      val state = stateQueue.synchronized(stateQueue.dequeue)
      nextStates(state).foreach: nextState =>
        knownStates.getOrElseUpdate(
          nextState, {
            stateQueue.enqueue(nextState)
            Some(state)
          },
        )

        if nextState.isErrorState
        then
          result = Some:
            Seq.from:
              Iterator.unfold(Some(nextState): Option[State]): nextStateOpt =>
                nextStateOpt.map: nextState =>
                  (nextState, knownStates(nextState))
        end if
    end while

    result
  end check
end ModelChecker
