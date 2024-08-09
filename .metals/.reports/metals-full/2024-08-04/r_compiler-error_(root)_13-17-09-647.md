jar:file://<HOME>/Library/Caches/Coursier/v1/https/repo1.maven.org/maven2/dev/zio/zio_3/2.0.22/zio_3-2.0.22-sources.jar!/zio/Scope.scala
### java.lang.AssertionError: assertion failed

occurred in the presentation compiler.

presentation compiler configuration:


action parameters:
uri: jar:file://<HOME>/Library/Caches/Coursier/v1/https/repo1.maven.org/maven2/dev/zio/zio_3/2.0.22/zio_3-2.0.22-sources.jar!/zio/Scope.scala
text:
```scala
/*
 * Copyright 2022-2024 John A. De Goes and the ZIO Contributors
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package zio

import zio.stacktracer.TracingImplicits.disableAutoTrace

import scala.collection.immutable.LongMap

/**
 * A `Scope` is the foundation of safe, composable resource management in ZIO. A
 * scope has two fundamental operators, `addFinalizer`, which adds a finalizer
 * to the scope, and `close`, which closes a scope and runs all finalizers that
 * have been added to the scope.
 */
trait Scope extends Serializable { self =>

  /**
   * Adds a finalizer to this scope. The finalizer is guaranteed to be run when
   * the scope is closed.
   */
  def addFinalizerExit(finalizer: Exit[Any, Any] => UIO[Any])(implicit trace: Trace): UIO[Unit]

  /**
   * Forks a new scope that is a child of this scope. Finalizers added to the
   * child scope will be run according to the specified `ExecutionStrategy`. The
   * child scope will automatically be closed when this scope is closed.
   */
  def forkWith(executionStrategy: => ExecutionStrategy)(implicit trace: Trace): UIO[Scope.Closeable]

  /**
   * A simplified version of `addFinalizerWith` when the `finalizer` does not
   * depend on the `Exit` value that the scope is closed with.
   */
  final def addFinalizer(finalizer: => UIO[Any])(implicit trace: Trace): UIO[Unit] =
    addFinalizerExit(_ => finalizer)

  /**
   * The execution strategy finalizers associated with this scope will be run
   * with.
   */
  def executionStrategy: ExecutionStrategy =
    ExecutionStrategy.Sequential

  /**
   * Extends the scope of a `ZIO` workflow that needs a scope into this scope by
   * providing it to the workflow but not closing the scope when the workflow
   * completes execution. This allows extending a scoped value into a larger
   * scope.
   */
  final def extend[R]: Scope.ExtendPartiallyApplied[R] =
    new Scope.ExtendPartiallyApplied[R](self)

  /**
   * Forks a new scope that is a child of this scope. Finalizers added to this
   * scope will be run sequentially in the reverse of the order in which they
   * were added when this scope is closed. The child scope will automatically be
   * closed when this scope is closed.
   */
  final def fork(implicit trace: Trace): UIO[Scope.Closeable] =
    forkWith(executionStrategy)
}

object Scope {

  sealed trait Closeable extends Scope { self =>

    /**
     * Closes a scope with the specified exit value, running all finalizers that
     * have been added to the scope.
     */
    def close(exit: => Exit[Any, Any])(implicit trace: Trace): UIO[Unit]

    /**
     * Uses the scope by providing it to a `ZIO` workflow that needs a scope,
     * guaranteeing that the scope is closed with the result of that workflow as
     * soon as the workflow completes execution, whether by success, failure, or
     * interruption.
     */
    final def use[R]: Scope.UsePartiallyApplied[R] =
      new Scope.UsePartiallyApplied[R](self)
  }

  /**
   * Accesses a scope in the environment and adds a finalizer to it.
   */
  def addFinalizer(finalizer: => UIO[Any])(implicit trace: Trace): ZIO[Scope, Nothing, Unit] =
    ZIO.serviceWithZIO(_.addFinalizer(finalizer))

  /**
   * Accesses a scope in the environment and adds a finalizer to it.
   */
  def addFinalizerExit(finalizer: Exit[Any, Any] => UIO[Any])(implicit
    trace: Trace
  ): ZIO[Scope, Nothing, Unit] =
    ZIO.serviceWithZIO(_.addFinalizerExit(finalizer))

  /**
   * A layer that constructs a scope and closes it when the workflow the layer
   * is provided to completes execution, whether by success, failure, or
   * interruption. This can be used to close a scope when providing a layer to a
   * workflow.
   */
  val default: ZLayer[Any, Nothing, Scope] =
    ZLayer.scopedEnvironment(
      ZIO
        .acquireReleaseExit(Scope.make(Trace.empty))((scope, exit) => scope.close(exit)(Trace.empty))(
          Trace.empty
        )
        .map(ZEnvironment[Scope](_))(Trace.empty)
    )(Trace.empty)

  /**
   * The global scope which is never closed. Finalizers added to this scope will
   * be immediately discarded and closing this scope has no effect.
   */
  val global: Scope.Closeable =
    new Scope.Closeable {
      def addFinalizerExit(finalizer: Exit[Any, Any] => UIO[Any])(implicit trace: Trace): UIO[Unit] =
        ZIO.unit
      def close(exit: => Exit[Any, Any])(implicit trace: Trace): UIO[Unit] =
        ZIO.unit
      def forkWith(executionStrategy: => ExecutionStrategy)(implicit trace: Trace): UIO[Scope.Closeable] =
        makeWith(executionStrategy)
    }

  /**
   * Makes a scope. Finalizers added to this scope will be run sequentially in
   * the reverse of the order in which they were added when this scope is
   * closed.
   */
  def make(implicit trace: Trace): UIO[Scope.Closeable] =
    makeWith(ExecutionStrategy.Sequential)

  /**
   * Makes a scope. Finalizers added to this scope will be run according to the
   * specified `ExecutionStrategy`.
   */
  def makeWith(executionStrategy0: => ExecutionStrategy)(implicit trace: Trace): UIO[Scope.Closeable] =
    ReleaseMap.make.map { releaseMap =>
      new Scope.Closeable { self =>
        def addFinalizerExit(finalizer: Exit[Any, Any] => UIO[Any])(implicit trace: Trace): UIO[Unit] =
          releaseMap.add(finalizer).unit
        def close(exit: => Exit[Any, Any])(implicit trace: Trace): UIO[Unit] =
          ZIO.suspendSucceed(releaseMap.releaseAll(exit, executionStrategy).unit)
        override val executionStrategy: ExecutionStrategy =
          executionStrategy0
        def forkWith(executionStrategy: => ExecutionStrategy)(implicit trace: Trace): UIO[Scope.Closeable] =
          ZIO.uninterruptible {
            for {
              scope     <- Scope.makeWith(executionStrategy)
              finalizer <- releaseMap.add(scope.close(_))
              _         <- scope.addFinalizerExit(finalizer)
            } yield scope
          }
      }
    }

  /**
   * Makes a scope. Finalizers added to this scope will be run in parallel when
   * this scope is closed.
   */
  def parallel(implicit trace: Trace): UIO[Scope.Closeable] =
    makeWith(ExecutionStrategy.Parallel)

  final class ExtendPartiallyApplied[R](private val scope: Scope) extends AnyVal {
    def apply[E, A](zio: => ZIO[Scope with R, E, A])(implicit trace: Trace): ZIO[R, E, A] =
      zio.provideSomeEnvironment[R](_.union[Scope](ZEnvironment(scope)))
  }

  final class UsePartiallyApplied[R](private val scope: Scope.Closeable) extends AnyVal {
    def apply[E, A](zio: => ZIO[Scope with R, E, A])(implicit trace: Trace): ZIO[R, E, A] =
      scope.extend[R](zio).onExit(scope.close(_))
  }

  private type Finalizer = Exit[Any, Any] => UIO[Any]

  private sealed abstract class State
  private final case class Exited(nextKey: Long, exit: Exit[Any, Any], update: Finalizer => Finalizer) extends State
  private final case class Running(nextKey: Long, finalizers: LongMap[Finalizer], update: Finalizer => Finalizer)
      extends State

  /**
   * A `ReleaseMap` represents the finalizers associated with a scope.
   *
   * The design of `ReleaseMap` is inspired by ResourceT, written by Michael
   * Snoyman @snoyberg.
   * (https://github.com/snoyberg/conduit/blob/master/resourcet/Control/Monad/Trans/Resource/Internal.hs)
   */
  private abstract class ReleaseMap extends Serializable {

    /**
     * An opaque identifier for a finalizer stored in the map.
     */
    type Key

    /**
     * Adds a finalizer to the finalizers associated with this scope. If the
     * finalizers associated with this scope have already been run this
     * finalizer will be run immediately.
     *
     * The finalizer returned from this method will remove the original
     * finalizer from the map and run it.
     */
    def add(finalizer: Finalizer)(implicit trace: Trace): UIO[Finalizer]

    /**
     * Adds a finalizer to the finalizers associated with this scope. If the
     * scope is still open, a [[Key]] will be returned. This is an opaque
     * identifier that can be used to activate this finalizer and remove it from
     * the map. from the map. If the scope has been closed, the finalizer will
     * be executed immediately (with the [[Exit]] value with which the scope has
     * ended) and no Key will be returned.
     */
    def addIfOpen(finalizer: Finalizer)(implicit trace: Trace): UIO[Option[Key]]

    /**
     * Retrieves the finalizer associated with this key.
     */
    def get(key: Key)(implicit trace: Trace): UIO[Option[Finalizer]]

    /**
     * Runs the specified finalizer and removes it from the finalizers
     * associated with this scope.
     */
    def release(key: Key, exit: Exit[Any, Any])(implicit trace: Trace): UIO[Any]

    /**
     * Runs the finalizers associated with this scope using the specified
     * execution strategy. After this action finishes, any finalizers added to
     * this scope will be run immediately.
     */
    def releaseAll(exit: Exit[Any, Any], execStrategy: ExecutionStrategy)(implicit trace: Trace): UIO[Any]

    /**
     * Removes the finalizer associated with this key and returns it.
     */
    def remove(key: Key)(implicit trace: Trace): UIO[Option[Finalizer]]

    /**
     * Replaces the finalizer associated with this key and returns it. If the
     * finalizers associated with this scope have already been run this
     * finalizer will be run immediately.
     */
    def replace(key: Key, finalizer: Finalizer)(implicit trace: Trace): UIO[Option[Finalizer]]

    /**
     * Updates the finalizers associated with this scope using the specified
     * function.
     */
    def updateAll(f: Finalizer => Finalizer)(implicit trace: Trace): UIO[Unit]
  }

  private object ReleaseMap {

    /**
     * Creates a new ReleaseMap.
     */
    def make(implicit trace: Trace): UIO[ReleaseMap] =
      ZIO.succeed(unsafe.make()(Unsafe.unsafe))

    private object unsafe {

      /**
       * Creates a new ReleaseMap.
       */
      def make()(implicit Unsafe: Unsafe) = {
        // The sorting order of the LongMap uses bit ordering (000, 001, ... 111 but with 64 bits). This
        // works out to be `0 ... Long.MaxValue, Long.MinValue, ... -1`. The order of the map is mainly
        // important for the finalization, in which we want to walk it in reverse order. So we insert
        // into the map using keys that will build it in reverse. That way, when we do the final iteration,
        // the finalizers are already in correct order.
        val initialKey: Long = -1L

        def next(l: Long) =
          if (l == 0L) throw new RuntimeException("ReleaseMap wrapped around")
          else if (l == Long.MinValue) Long.MaxValue
          else l - 1

        val ref: Ref[State] =
          Ref.unsafe.make(Running(initialKey, LongMap.empty, identity))

        new ReleaseMap {
          type Key = Long

          def add(finalizer: Finalizer)(implicit trace: Trace): UIO[Finalizer] =
            addIfOpen(finalizer).map {
              case Some(key) => release(key, _)
              case None      => _ => ZIO.unit
            }

          def addIfOpen(finalizer: Finalizer)(implicit trace: Trace): UIO[Option[Key]] =
            ref.modify {
              case Exited(nextKey, exit, update) =>
                finalizer(exit).as(None) -> Exited(next(nextKey), exit, update)
              case Running(nextKey, fins, update) =>
                ZIO.succeed(Some(nextKey)) -> Running(next(nextKey), fins.updated(nextKey, finalizer), update)
            }.flatten

          def get(key: Key)(implicit trace: Trace): UIO[Option[Finalizer]] =
            ref.get.map {
              case Exited(_, _, _)     => None
              case Running(_, fins, _) => fins get key
            }

          def release(key: Key, exit: Exit[Any, Any])(implicit trace: Trace): UIO[Any] =
            ref.modify {
              case s @ Exited(_, _, _) => (ZIO.unit, s)
              case s @ Running(_, fins, update) =>
                (
                  fins.get(key).fold(ZIO.unit: UIO[Any])(fin => update(fin)(exit)),
                  s.copy(finalizers = fins - key)
                )
            }.flatten

          def releaseAll(exit: Exit[Any, Any], execStrategy: ExecutionStrategy)(implicit trace: Trace): UIO[Any] =
            ref.modify {
              case s @ Exited(_, _, _) => (ZIO.unit, s)
              case Running(nextKey, fins, update) =>
                execStrategy match {
                  case ExecutionStrategy.Sequential =>
                    (
                      ZIO
                        .foreach(fins: Iterable[(Long, Finalizer)]) { case (_, fin) =>
                          update(fin).apply(exit).exit
                        }
                        .flatMap(results => ZIO.done(Exit.collectAll(results) getOrElse Exit.unit)),
                      Exited(nextKey, exit, update)
                    )

                  case ExecutionStrategy.Parallel =>
                    (
                      ZIO
                        .foreachPar(fins: Iterable[(Long, Finalizer)]) { case (_, finalizer) =>
                          update(finalizer)(exit).exit
                        }
                        .flatMap(results => ZIO.done(Exit.collectAllPar(results) getOrElse Exit.unit)),
                      Exited(nextKey, exit, update)
                    )

                  case ExecutionStrategy.ParallelN(n) =>
                    (
                      ZIO
                        .foreachPar(fins: Iterable[(Long, Finalizer)]) { case (_, finalizer) =>
                          update(finalizer)(exit).exit
                        }
                        .flatMap(results => ZIO.done(Exit.collectAllPar(results) getOrElse Exit.unit))
                        .withParallelism(n),
                      Exited(nextKey, exit, update)
                    )

                }
            }.flatten

          def remove(key: Key)(implicit trace: Trace): UIO[Option[Finalizer]] =
            ref.modify {
              case Exited(nk, exit, update)  => (None, Exited(nk, exit, update))
              case Running(nk, fins, update) => (fins get key, Running(nk, fins - key, update))
            }

          def replace(key: Key, finalizer: Finalizer)(implicit trace: Trace): UIO[Option[Finalizer]] =
            ref.modify {
              case Exited(nk, exit, update) => (finalizer(exit).as(None), Exited(nk, exit, update))
              case Running(nk, fins, update) =>
                (ZIO.succeed(fins get key), Running(nk, fins.updated(key, finalizer), update))
            }.flatten

          def updateAll(f: Finalizer => Finalizer)(implicit trace: Trace): UIO[Unit] =
            ref.update {
              case Exited(key, exit, update)  => Exited(key, exit, update.andThen(f))
              case Running(key, exit, update) => Running(key, exit, update.andThen(f))
            }
        }
      }
    }
  }
}

```



#### Error stacktrace:

```
scala.runtime.Scala3RunTime$.assertFailed(Scala3RunTime.scala:11)
	dotty.tools.dotc.core.TypeOps$.dominators$1(TypeOps.scala:248)
	dotty.tools.dotc.core.TypeOps$.approximateOr$1(TypeOps.scala:382)
	dotty.tools.dotc.core.TypeOps$.orDominator(TypeOps.scala:395)
	dotty.tools.dotc.core.Types$OrType.join(Types.scala:3554)
	dotty.tools.dotc.core.Types$OrType.widenUnionWithoutNull(Types.scala:3570)
	dotty.tools.dotc.core.Types$Type.widenUnion(Types.scala:1358)
	dotty.tools.dotc.core.ConstraintHandling.widenOr$1(ConstraintHandling.scala:653)
	dotty.tools.dotc.core.ConstraintHandling.widenInferred(ConstraintHandling.scala:669)
	dotty.tools.dotc.core.ConstraintHandling.widenInferred$(ConstraintHandling.scala:29)
	dotty.tools.dotc.core.TypeComparer.widenInferred(TypeComparer.scala:30)
	dotty.tools.dotc.core.ConstraintHandling.instanceType(ConstraintHandling.scala:708)
	dotty.tools.dotc.core.ConstraintHandling.instanceType$(ConstraintHandling.scala:29)
	dotty.tools.dotc.core.TypeComparer.instanceType(TypeComparer.scala:30)
	dotty.tools.dotc.core.TypeComparer$.instanceType(TypeComparer.scala:3207)
	dotty.tools.dotc.core.Types$TypeVar.typeToInstantiateWith(Types.scala:4916)
	dotty.tools.dotc.core.Types$TypeVar.instantiate(Types.scala:4926)
	dotty.tools.dotc.typer.Inferencing.tryInstantiate$1(Inferencing.scala:790)
	dotty.tools.dotc.typer.Inferencing.doInstantiate$1(Inferencing.scala:793)
	dotty.tools.dotc.typer.Inferencing.interpolateTypeVars(Inferencing.scala:796)
	dotty.tools.dotc.typer.Inferencing.interpolateTypeVars$(Inferencing.scala:611)
	dotty.tools.dotc.typer.Typer.interpolateTypeVars(Typer.scala:120)
	dotty.tools.dotc.typer.Typer.simplify(Typer.scala:3215)
	dotty.tools.dotc.typer.Typer.readaptSimplified$1(Typer.scala:3689)
	dotty.tools.dotc.typer.Typer.addImplicitArgs$1(Typer.scala:3921)
	dotty.tools.dotc.typer.Typer.adaptNoArgsImplicitMethod$1(Typer.scala:3930)
	dotty.tools.dotc.typer.Typer.adaptNoArgs$1(Typer.scala:4130)
	dotty.tools.dotc.typer.Typer.adapt1(Typer.scala:4379)
	dotty.tools.dotc.typer.Typer.adapt(Typer.scala:3677)
	dotty.tools.dotc.typer.Typer.typed(Typer.scala:3274)
	dotty.tools.dotc.typer.Typer.typed(Typer.scala:3278)
	dotty.tools.dotc.typer.Typer.typedExpr(Typer.scala:3389)
	dotty.tools.dotc.typer.Namer.typedAheadExpr$$anonfun$1(Namer.scala:1685)
	dotty.tools.dotc.typer.Namer.typedAhead(Namer.scala:1675)
	dotty.tools.dotc.typer.Namer.typedAheadExpr(Namer.scala:1685)
	dotty.tools.dotc.typer.Namer.valOrDefDefSig(Namer.scala:1747)
	dotty.tools.dotc.typer.Namer.defDefSig(Namer.scala:1828)
	dotty.tools.dotc.typer.Namer$Completer.typeSig(Namer.scala:808)
	dotty.tools.dotc.typer.Namer$Completer.completeInCreationContext(Namer.scala:955)
	dotty.tools.dotc.typer.Namer$Completer.complete(Namer.scala:831)
	dotty.tools.dotc.core.SymDenotations$SymDenotation.completeFrom(SymDenotations.scala:178)
	dotty.tools.dotc.core.Denotations$Denotation.completeInfo$1(Denotations.scala:190)
	dotty.tools.dotc.core.Denotations$Denotation.info(Denotations.scala:192)
	dotty.tools.dotc.core.SymDenotations$SymDenotation.ensureCompleted(SymDenotations.scala:398)
	dotty.tools.dotc.typer.Typer.retrieveSym(Typer.scala:3060)
	dotty.tools.dotc.typer.Typer.typedNamed$1(Typer.scala:3085)
	dotty.tools.dotc.typer.Typer.typedUnadapted(Typer.scala:3196)
	dotty.tools.dotc.typer.Typer.typed(Typer.scala:3274)
	dotty.tools.dotc.typer.Typer.typed(Typer.scala:3278)
	dotty.tools.dotc.typer.Typer.traverse$1(Typer.scala:3300)
	dotty.tools.dotc.typer.Typer.typedStats(Typer.scala:3346)
	dotty.tools.dotc.typer.Typer.typedBlockStats(Typer.scala:1193)
	dotty.tools.dotc.typer.Typer.typedBlock(Typer.scala:1197)
	dotty.tools.dotc.typer.Typer.typedUnnamed$1(Typer.scala:3121)
	dotty.tools.dotc.typer.Typer.typedUnadapted(Typer.scala:3197)
	dotty.tools.dotc.typer.Typer.typed(Typer.scala:3274)
	dotty.tools.dotc.typer.Typer.typed(Typer.scala:3278)
	dotty.tools.dotc.typer.Typer.typedFunctionValue(Typer.scala:1676)
	dotty.tools.dotc.typer.Typer.typedFunction(Typer.scala:1416)
	dotty.tools.dotc.typer.Typer.typedUnnamed$1(Typer.scala:3123)
	dotty.tools.dotc.typer.Typer.typedUnadapted(Typer.scala:3197)
	dotty.tools.dotc.typer.ProtoTypes$FunProto.$anonfun$7(ProtoTypes.scala:509)
	dotty.tools.dotc.typer.ProtoTypes$FunProto.cacheTypedArg(ProtoTypes.scala:432)
	dotty.tools.dotc.typer.ProtoTypes$FunProto.typedArg(ProtoTypes.scala:510)
	dotty.tools.dotc.typer.Applications$ApplyToUntyped.typedArg(Applications.scala:913)
	dotty.tools.dotc.typer.Applications$ApplyToUntyped.typedArg(Applications.scala:913)
	dotty.tools.dotc.typer.Applications$Application.addTyped$1(Applications.scala:605)
	dotty.tools.dotc.typer.Applications$Application.matchArgs(Applications.scala:669)
	dotty.tools.dotc.typer.Applications$Application.init(Applications.scala:491)
	dotty.tools.dotc.typer.Applications$TypedApply.<init>(Applications.scala:795)
	dotty.tools.dotc.typer.Applications$ApplyToUntyped.<init>(Applications.scala:912)
	dotty.tools.dotc.typer.Applications.ApplyTo(Applications.scala:1142)
	dotty.tools.dotc.typer.Applications.ApplyTo$(Applications.scala:351)
	dotty.tools.dotc.typer.Typer.ApplyTo(Typer.scala:120)
	dotty.tools.dotc.typer.Applications.simpleApply$1(Applications.scala:985)
	dotty.tools.dotc.typer.Applications.realApply$1$$anonfun$2(Applications.scala:1068)
	dotty.tools.dotc.typer.Typer.tryEither(Typer.scala:3413)
	dotty.tools.dotc.typer.Applications.realApply$1(Applications.scala:1079)
	dotty.tools.dotc.typer.Applications.typedApply(Applications.scala:1117)
	dotty.tools.dotc.typer.Applications.typedApply$(Applications.scala:351)
	dotty.tools.dotc.typer.Typer.typedApply(Typer.scala:120)
	dotty.tools.dotc.typer.Typer.typedUnnamed$1(Typer.scala:3113)
	dotty.tools.dotc.typer.Typer.typedUnadapted(Typer.scala:3197)
	dotty.tools.dotc.typer.ProtoTypes$FunProto.$anonfun$7(ProtoTypes.scala:509)
	dotty.tools.dotc.typer.ProtoTypes$FunProto.cacheTypedArg(ProtoTypes.scala:432)
	dotty.tools.dotc.typer.ProtoTypes$FunProto.typedArg(ProtoTypes.scala:510)
	dotty.tools.dotc.typer.Applications$ApplyToUntyped.typedArg(Applications.scala:913)
	dotty.tools.dotc.typer.Applications$ApplyToUntyped.typedArg(Applications.scala:913)
	dotty.tools.dotc.typer.Applications$Application.addTyped$1(Applications.scala:605)
	dotty.tools.dotc.typer.Applications$Application.matchArgs(Applications.scala:669)
	dotty.tools.dotc.typer.Applications$Application.init(Applications.scala:491)
	dotty.tools.dotc.typer.Applications$TypedApply.<init>(Applications.scala:795)
	dotty.tools.dotc.typer.Applications$ApplyToUntyped.<init>(Applications.scala:912)
	dotty.tools.dotc.typer.Applications.ApplyTo(Applications.scala:1142)
	dotty.tools.dotc.typer.Applications.ApplyTo$(Applications.scala:351)
	dotty.tools.dotc.typer.Typer.ApplyTo(Typer.scala:120)
	dotty.tools.dotc.typer.Applications.simpleApply$1(Applications.scala:985)
	dotty.tools.dotc.typer.Applications.realApply$1$$anonfun$2(Applications.scala:1068)
	dotty.tools.dotc.typer.Typer.tryEither(Typer.scala:3413)
	dotty.tools.dotc.typer.Applications.realApply$1(Applications.scala:1079)
	dotty.tools.dotc.typer.Applications.typedApply(Applications.scala:1117)
	dotty.tools.dotc.typer.Applications.typedApply$(Applications.scala:351)
	dotty.tools.dotc.typer.Typer.typedApply(Typer.scala:120)
	dotty.tools.dotc.typer.Typer.typedUnnamed$1(Typer.scala:3113)
	dotty.tools.dotc.typer.Typer.typedUnadapted(Typer.scala:3197)
	dotty.tools.dotc.typer.Typer.typed(Typer.scala:3274)
	dotty.tools.dotc.typer.Typer.typed(Typer.scala:3278)
	dotty.tools.dotc.typer.Typer.typedTuple(Typer.scala:3028)
	dotty.tools.dotc.typer.Typer.typedUnnamed$1(Typer.scala:3151)
	dotty.tools.dotc.typer.Typer.typedUnadapted(Typer.scala:3197)
	dotty.tools.dotc.typer.Typer.typed(Typer.scala:3274)
	dotty.tools.dotc.typer.Typer.typed(Typer.scala:3278)
	dotty.tools.dotc.typer.Typer.typedExpr(Typer.scala:3389)
	dotty.tools.dotc.typer.Typer.typedBlock(Typer.scala:1200)
	dotty.tools.dotc.typer.Typer.typedUnnamed$1(Typer.scala:3121)
	dotty.tools.dotc.typer.Typer.typedUnadapted(Typer.scala:3197)
	dotty.tools.dotc.typer.Typer.typed(Typer.scala:3274)
	dotty.tools.dotc.typer.Typer.typed(Typer.scala:3278)
	dotty.tools.dotc.typer.Typer.typedExpr(Typer.scala:3389)
	dotty.tools.dotc.typer.Typer.caseRest$1(Typer.scala:1971)
	dotty.tools.dotc.typer.Typer.typedCase(Typer.scala:1987)
	dotty.tools.dotc.typer.Typer.typedCases$$anonfun$1(Typer.scala:1912)
	dotty.tools.dotc.core.Decorators$.loop$1(Decorators.scala:99)
	dotty.tools.dotc.core.Decorators$.mapconserve(Decorators.scala:115)
	dotty.tools.dotc.typer.Typer.typedCases(Typer.scala:1917)
	dotty.tools.dotc.typer.Typer.$anonfun$37(Typer.scala:1902)
	dotty.tools.dotc.typer.Applications.harmonic(Applications.scala:2387)
	dotty.tools.dotc.typer.Applications.harmonic$(Applications.scala:351)
	dotty.tools.dotc.typer.Typer.harmonic(Typer.scala:120)
	dotty.tools.dotc.typer.Typer.typedMatchFinish(Typer.scala:1902)
	dotty.tools.dotc.typer.Typer.typedMatch(Typer.scala:1831)
	dotty.tools.dotc.typer.Typer.typedUnnamed$1(Typer.scala:3128)
	dotty.tools.dotc.typer.Typer.typedUnadapted(Typer.scala:3197)
	dotty.tools.dotc.typer.Typer.typed(Typer.scala:3274)
	dotty.tools.dotc.typer.Typer.typed(Typer.scala:3278)
	dotty.tools.dotc.typer.Typer.typedExpr(Typer.scala:3389)
	dotty.tools.dotc.typer.Typer.typedBlock(Typer.scala:1200)
	dotty.tools.dotc.typer.Typer.typedUnnamed$1(Typer.scala:3121)
	dotty.tools.dotc.typer.Typer.typedUnadapted(Typer.scala:3197)
	dotty.tools.dotc.typer.Typer.typed(Typer.scala:3274)
	dotty.tools.dotc.typer.Typer.typed(Typer.scala:3278)
	dotty.tools.dotc.typer.Typer.typedExpr(Typer.scala:3389)
	dotty.tools.dotc.typer.Typer.caseRest$1(Typer.scala:1971)
	dotty.tools.dotc.typer.Typer.typedCase(Typer.scala:1987)
	dotty.tools.dotc.typer.Typer.typedCases$$anonfun$1(Typer.scala:1912)
	dotty.tools.dotc.core.Decorators$.loop$1(Decorators.scala:99)
	dotty.tools.dotc.core.Decorators$.mapconserve(Decorators.scala:115)
	dotty.tools.dotc.typer.Typer.typedCases(Typer.scala:1917)
	dotty.tools.dotc.typer.Typer.$anonfun$37(Typer.scala:1902)
	dotty.tools.dotc.typer.Applications.harmonic(Applications.scala:2387)
	dotty.tools.dotc.typer.Applications.harmonic$(Applications.scala:351)
	dotty.tools.dotc.typer.Typer.harmonic(Typer.scala:120)
	dotty.tools.dotc.typer.Typer.typedMatchFinish(Typer.scala:1902)
	dotty.tools.dotc.typer.Typer.typedMatch(Typer.scala:1831)
	dotty.tools.dotc.typer.Typer.typedUnnamed$1(Typer.scala:3128)
	dotty.tools.dotc.typer.Typer.typedUnadapted(Typer.scala:3197)
	dotty.tools.dotc.typer.Typer.typed(Typer.scala:3274)
	dotty.tools.dotc.typer.Typer.typed(Typer.scala:3278)
	dotty.tools.dotc.typer.Typer.typedExpr(Typer.scala:3389)
	dotty.tools.dotc.typer.Namer.typedAheadExpr$$anonfun$1(Namer.scala:1685)
	dotty.tools.dotc.typer.Namer.typedAhead(Namer.scala:1675)
	dotty.tools.dotc.typer.Namer.typedAheadExpr(Namer.scala:1685)
	dotty.tools.dotc.typer.Namer.valOrDefDefSig(Namer.scala:1747)
	dotty.tools.dotc.typer.Namer.defDefSig(Namer.scala:1828)
	dotty.tools.dotc.typer.Namer$Completer.typeSig(Namer.scala:808)
	dotty.tools.dotc.typer.Namer$Completer.completeInCreationContext(Namer.scala:955)
	dotty.tools.dotc.typer.Namer$Completer.complete(Namer.scala:831)
	dotty.tools.dotc.core.SymDenotations$SymDenotation.completeFrom(SymDenotations.scala:178)
	dotty.tools.dotc.core.Denotations$Denotation.completeInfo$1(Denotations.scala:190)
	dotty.tools.dotc.core.Denotations$Denotation.info(Denotations.scala:192)
	dotty.tools.dotc.core.SymDenotations$SymDenotation.ensureCompleted(SymDenotations.scala:398)
	dotty.tools.dotc.typer.Typer.retrieveSym(Typer.scala:3060)
	dotty.tools.dotc.typer.Typer.typedNamed$1(Typer.scala:3085)
	dotty.tools.dotc.typer.Typer.typedUnadapted(Typer.scala:3196)
	dotty.tools.dotc.typer.Typer.typed(Typer.scala:3274)
	dotty.tools.dotc.typer.Typer.typed(Typer.scala:3278)
	dotty.tools.dotc.typer.Typer.traverse$1(Typer.scala:3300)
	dotty.tools.dotc.typer.Typer.typedStats(Typer.scala:3346)
	dotty.tools.dotc.typer.Typer.typedBlockStats(Typer.scala:1193)
	dotty.tools.dotc.typer.Typer.typedBlock(Typer.scala:1197)
	dotty.tools.dotc.typer.Typer.typedUnnamed$1(Typer.scala:3121)
	dotty.tools.dotc.typer.Typer.typedUnadapted(Typer.scala:3197)
	dotty.tools.dotc.typer.Typer.typed(Typer.scala:3274)
	dotty.tools.dotc.typer.Typer.typed(Typer.scala:3278)
	dotty.tools.dotc.typer.Typer.typedFunctionValue(Typer.scala:1676)
	dotty.tools.dotc.typer.Typer.typedFunction(Typer.scala:1416)
	dotty.tools.dotc.typer.Typer.typedUnnamed$1(Typer.scala:3123)
	dotty.tools.dotc.typer.Typer.typedUnadapted(Typer.scala:3197)
	dotty.tools.dotc.typer.Typer.typed(Typer.scala:3274)
	dotty.tools.dotc.typer.Typer.typed(Typer.scala:3278)
	dotty.tools.dotc.typer.Typer.typedMatch(Typer.scala:1785)
	dotty.tools.dotc.typer.Typer.typedUnnamed$1(Typer.scala:3128)
	dotty.tools.dotc.typer.Typer.typedUnadapted(Typer.scala:3197)
	dotty.tools.dotc.typer.ProtoTypes$FunProto.$anonfun$7(ProtoTypes.scala:509)
	dotty.tools.dotc.typer.ProtoTypes$FunProto.cacheTypedArg(ProtoTypes.scala:432)
	dotty.tools.dotc.typer.ProtoTypes$FunProto.typedArg(ProtoTypes.scala:510)
	dotty.tools.dotc.typer.Applications$ApplyToUntyped.typedArg(Applications.scala:913)
	dotty.tools.dotc.typer.Applications$ApplyToUntyped.typedArg(Applications.scala:913)
	dotty.tools.dotc.typer.Applications$Application.addTyped$1(Applications.scala:605)
	dotty.tools.dotc.typer.Applications$Application.matchArgs(Applications.scala:669)
	dotty.tools.dotc.typer.Applications$Application.init(Applications.scala:491)
	dotty.tools.dotc.typer.Applications$TypedApply.<init>(Applications.scala:795)
	dotty.tools.dotc.typer.Applications$ApplyToUntyped.<init>(Applications.scala:912)
	dotty.tools.dotc.typer.Applications.ApplyTo(Applications.scala:1142)
	dotty.tools.dotc.typer.Applications.ApplyTo$(Applications.scala:351)
	dotty.tools.dotc.typer.Typer.ApplyTo(Typer.scala:120)
	dotty.tools.dotc.typer.Applications.simpleApply$1(Applications.scala:985)
	dotty.tools.dotc.typer.Applications.realApply$1$$anonfun$2(Applications.scala:1068)
	dotty.tools.dotc.typer.Typer.tryEither(Typer.scala:3413)
	dotty.tools.dotc.typer.Applications.realApply$1(Applications.scala:1079)
	dotty.tools.dotc.typer.Applications.typedApply(Applications.scala:1117)
	dotty.tools.dotc.typer.Applications.typedApply$(Applications.scala:351)
	dotty.tools.dotc.typer.Typer.typedApply(Typer.scala:120)
	dotty.tools.dotc.typer.Typer.typedUnnamed$1(Typer.scala:3113)
	dotty.tools.dotc.typer.Typer.typedUnadapted(Typer.scala:3197)
	dotty.tools.dotc.typer.Typer.typed(Typer.scala:3274)
	dotty.tools.dotc.typer.Typer.typed(Typer.scala:3278)
	dotty.tools.dotc.typer.Typer.typedExpr(Typer.scala:3389)
	dotty.tools.dotc.typer.Typer.typeSelectOnTerm$1(Typer.scala:763)
	dotty.tools.dotc.typer.Typer.typedSelect(Typer.scala:801)
	dotty.tools.dotc.typer.Typer.typedNamed$1(Typer.scala:3088)
	dotty.tools.dotc.typer.Typer.typedUnadapted(Typer.scala:3196)
	dotty.tools.dotc.typer.Typer.typed(Typer.scala:3274)
	dotty.tools.dotc.typer.Typer.typed(Typer.scala:3278)
	dotty.tools.dotc.typer.Typer.typedExpr(Typer.scala:3389)
	dotty.tools.dotc.typer.Typer.$anonfun$62(Typer.scala:2603)
	dotty.tools.dotc.inlines.PrepareInlineable$.dropInlineIfError(PrepareInlineable.scala:256)
	dotty.tools.dotc.typer.Typer.typedDefDef(Typer.scala:2603)
	dotty.tools.dotc.typer.Typer.typedNamed$1(Typer.scala:3095)
	dotty.tools.dotc.typer.Typer.typedUnadapted(Typer.scala:3196)
	dotty.tools.dotc.typer.Typer.typed(Typer.scala:3274)
	dotty.tools.dotc.typer.Typer.typed(Typer.scala:3278)
	dotty.tools.dotc.typer.Typer.traverse$1(Typer.scala:3300)
	dotty.tools.dotc.typer.Typer.typedStats(Typer.scala:3346)
	dotty.tools.dotc.typer.Typer.typedClassDef(Typer.scala:2790)
	dotty.tools.dotc.typer.Typer.typedTypeOrClassDef$1(Typer.scala:3101)
	dotty.tools.dotc.typer.Typer.typedNamed$1(Typer.scala:3105)
	dotty.tools.dotc.typer.Typer.typedUnadapted(Typer.scala:3196)
	dotty.tools.dotc.typer.Typer.typed(Typer.scala:3274)
	dotty.tools.dotc.typer.Typer.typed(Typer.scala:3278)
	dotty.tools.dotc.typer.Typer.traverse$1(Typer.scala:3300)
	dotty.tools.dotc.typer.Typer.typedStats(Typer.scala:3346)
	dotty.tools.dotc.typer.Typer.typedBlockStats(Typer.scala:1193)
	dotty.tools.dotc.typer.Typer.typedBlock(Typer.scala:1197)
	dotty.tools.dotc.typer.Typer.typedUnnamed$1(Typer.scala:3121)
	dotty.tools.dotc.typer.Typer.typedUnadapted(Typer.scala:3197)
	dotty.tools.dotc.typer.Typer.typed(Typer.scala:3274)
	dotty.tools.dotc.typer.Typer.typed(Typer.scala:3278)
	dotty.tools.dotc.typer.Typer.typedNew(Typer.scala:921)
	dotty.tools.dotc.typer.Typer.typedUnnamed$1(Typer.scala:3117)
	dotty.tools.dotc.typer.Typer.typedUnadapted(Typer.scala:3197)
	dotty.tools.dotc.typer.Typer.typed(Typer.scala:3274)
	dotty.tools.dotc.typer.Typer.typed(Typer.scala:3278)
	dotty.tools.dotc.typer.Typer.typedExpr(Typer.scala:3389)
	dotty.tools.dotc.typer.Typer.typedBlock(Typer.scala:1200)
	dotty.tools.dotc.typer.Typer.typedUnnamed$1(Typer.scala:3121)
	dotty.tools.dotc.typer.Typer.typedUnadapted(Typer.scala:3197)
	dotty.tools.dotc.typer.Typer.typed(Typer.scala:3274)
	dotty.tools.dotc.typer.Typer.typed(Typer.scala:3278)
	dotty.tools.dotc.typer.Typer.typedExpr(Typer.scala:3389)
	dotty.tools.dotc.typer.Namer.typedAheadExpr$$anonfun$1(Namer.scala:1685)
	dotty.tools.dotc.typer.Namer.typedAhead(Namer.scala:1675)
	dotty.tools.dotc.typer.Namer.typedAheadExpr(Namer.scala:1685)
	dotty.tools.dotc.typer.Namer.typedAheadRhs$1$$anonfun$1(Namer.scala:1941)
	dotty.tools.dotc.inlines.PrepareInlineable$.dropInlineIfError(PrepareInlineable.scala:256)
	dotty.tools.dotc.typer.Namer.typedAheadRhs$1(Namer.scala:1941)
	dotty.tools.dotc.typer.Namer.rhsType$1(Namer.scala:1949)
	dotty.tools.dotc.typer.Namer.cookedRhsType$1(Namer.scala:1967)
	dotty.tools.dotc.typer.Namer.lhsType$1(Namer.scala:1968)
	dotty.tools.dotc.typer.Namer.inferredResultType(Namer.scala:1979)
	dotty.tools.dotc.typer.Namer.inferredType$1(Namer.scala:1723)
	dotty.tools.dotc.typer.Namer.valOrDefDefSig(Namer.scala:1729)
	dotty.tools.dotc.typer.Namer.defDefSig(Namer.scala:1828)
	dotty.tools.dotc.typer.Namer$Completer.typeSig(Namer.scala:808)
	dotty.tools.dotc.typer.Namer$Completer.completeInCreationContext(Namer.scala:955)
	dotty.tools.dotc.typer.Namer$Completer.complete(Namer.scala:831)
	dotty.tools.dotc.core.SymDenotations$SymDenotation.completeFrom(SymDenotations.scala:178)
	dotty.tools.dotc.core.Denotations$Denotation.completeInfo$1(Denotations.scala:190)
	dotty.tools.dotc.core.Denotations$Denotation.info(Denotations.scala:192)
	dotty.tools.dotc.core.Types$TermRef.underlying(Types.scala:2920)
	dotty.tools.dotc.core.Types$Type.widenSingleton(Types.scala:1299)
	dotty.tools.dotc.typer.ProtoTypes$Compatibility.normalizedCompatible(ProtoTypes.scala:64)
	dotty.tools.dotc.typer.ProtoTypes$Compatibility.normalizedCompatible$(ProtoTypes.scala:31)
	dotty.tools.dotc.typer.Typer.normalizedCompatible(Typer.scala:120)
	dotty.tools.dotc.typer.ProtoTypes$SelectionProto.qualifies$1(ProtoTypes.scala:227)
	dotty.tools.dotc.typer.ProtoTypes$SelectionProto.isMatchedBy$$anonfun$1(ProtoTypes.scala:230)
	dotty.tools.dotc.core.Denotations$SingleDenotation.hasAltWith(Denotations.scala:645)
	dotty.tools.dotc.typer.ProtoTypes$SelectionProto.isMatchedBy(ProtoTypes.scala:230)
	dotty.tools.dotc.core.TypeComparer.isMatchedByProto(TypeComparer.scala:2145)
	dotty.tools.dotc.core.TypeComparer.firstTry$1(TypeComparer.scala:345)
	dotty.tools.dotc.core.TypeComparer.recur(TypeComparer.scala:1553)
	dotty.tools.dotc.core.TypeComparer.isSubType(TypeComparer.scala:214)
	dotty.tools.dotc.core.TypeComparer.isSubType(TypeComparer.scala:224)
	dotty.tools.dotc.core.TypeComparer.topLevelSubType(TypeComparer.scala:132)
	dotty.tools.dotc.core.TypeComparer.testSubType(TypeComparer.scala:149)
	dotty.tools.dotc.core.TypeComparer$.testSubType(TypeComparer.scala:3152)
	dotty.tools.dotc.typer.Typer.adaptNoArgsOther$1(Typer.scala:4067)
	dotty.tools.dotc.typer.Typer.adaptNoArgs$1(Typer.scala:4154)
	dotty.tools.dotc.typer.Typer.adapt1(Typer.scala:4379)
	dotty.tools.dotc.typer.Typer.adapt(Typer.scala:3677)
	dotty.tools.dotc.typer.Typer.typed(Typer.scala:3274)
	dotty.tools.dotc.typer.Typer.typed(Typer.scala:3278)
	dotty.tools.dotc.typer.Typer.typedExpr(Typer.scala:3389)
	dotty.tools.dotc.typer.Typer.typeSelectOnTerm$1(Typer.scala:763)
	dotty.tools.dotc.typer.Typer.typedSelect(Typer.scala:801)
	dotty.tools.dotc.typer.Typer.typedNamed$1(Typer.scala:3088)
	dotty.tools.dotc.typer.Typer.typedUnadapted(Typer.scala:3196)
	dotty.tools.dotc.typer.Typer.typed(Typer.scala:3274)
	dotty.tools.dotc.typer.Typer.typed(Typer.scala:3278)
	dotty.tools.dotc.typer.Typer.typedExpr(Typer.scala:3389)
	dotty.tools.dotc.typer.Applications.realApply$1(Applications.scala:957)
	dotty.tools.dotc.typer.Applications.typedApply(Applications.scala:1117)
	dotty.tools.dotc.typer.Applications.typedApply$(Applications.scala:351)
	dotty.tools.dotc.typer.Typer.typedApply(Typer.scala:120)
	dotty.tools.dotc.typer.Typer.typedUnnamed$1(Typer.scala:3113)
	dotty.tools.dotc.typer.Typer.typedUnadapted(Typer.scala:3197)
	dotty.tools.dotc.typer.Typer.typed(Typer.scala:3274)
	dotty.tools.dotc.typer.Typer.typed(Typer.scala:3278)
	dotty.tools.dotc.typer.Typer.typedExpr(Typer.scala:3389)
	dotty.tools.dotc.typer.Applications.realApply$1(Applications.scala:957)
	dotty.tools.dotc.typer.Applications.typedApply(Applications.scala:1117)
	dotty.tools.dotc.typer.Applications.typedApply$(Applications.scala:351)
	dotty.tools.dotc.typer.Typer.typedApply(Typer.scala:120)
	dotty.tools.dotc.typer.Typer.typedUnnamed$1(Typer.scala:3113)
	dotty.tools.dotc.typer.Typer.typedUnadapted(Typer.scala:3197)
	dotty.tools.dotc.typer.Typer.typed(Typer.scala:3274)
	dotty.tools.dotc.typer.Typer.typed(Typer.scala:3278)
	dotty.tools.dotc.typer.Typer.typedExpr(Typer.scala:3389)
	dotty.tools.dotc.typer.Namer.typedAheadExpr$$anonfun$1(Namer.scala:1685)
	dotty.tools.dotc.typer.Namer.typedAhead(Namer.scala:1675)
	dotty.tools.dotc.typer.Namer.typedAheadExpr(Namer.scala:1685)
	dotty.tools.dotc.typer.Namer.valOrDefDefSig(Namer.scala:1747)
	dotty.tools.dotc.typer.Namer.defDefSig(Namer.scala:1828)
	dotty.tools.dotc.typer.Namer$Completer.typeSig(Namer.scala:808)
	dotty.tools.dotc.typer.Namer$Completer.completeInCreationContext(Namer.scala:955)
	dotty.tools.dotc.typer.Namer$Completer.complete(Namer.scala:831)
	dotty.tools.dotc.core.SymDenotations$SymDenotation.completeFrom(SymDenotations.scala:178)
	dotty.tools.dotc.core.Denotations$Denotation.completeInfo$1(Denotations.scala:190)
	dotty.tools.dotc.core.Denotations$Denotation.info(Denotations.scala:192)
	dotty.tools.dotc.core.SymDenotations$SymDenotation.ensureCompleted(SymDenotations.scala:398)
	dotty.tools.dotc.typer.Typer.retrieveSym(Typer.scala:3060)
	dotty.tools.dotc.typer.Typer.typedNamed$1(Typer.scala:3085)
	dotty.tools.dotc.typer.Typer.typedUnadapted(Typer.scala:3196)
	dotty.tools.dotc.typer.Typer.typed(Typer.scala:3274)
	dotty.tools.dotc.typer.Typer.typed(Typer.scala:3278)
	dotty.tools.dotc.typer.Typer.traverse$1(Typer.scala:3300)
	dotty.tools.dotc.typer.Typer.typedStats(Typer.scala:3346)
	dotty.tools.dotc.typer.Typer.typedBlockStats(Typer.scala:1193)
	dotty.tools.dotc.typer.Typer.typedBlock(Typer.scala:1197)
	dotty.tools.dotc.typer.Typer.typedUnnamed$1(Typer.scala:3121)
	dotty.tools.dotc.typer.Typer.typedUnadapted(Typer.scala:3197)
	dotty.tools.dotc.typer.Typer.typed(Typer.scala:3274)
	dotty.tools.dotc.typer.Typer.typed(Typer.scala:3278)
	dotty.tools.dotc.typer.Typer.typedFunctionValue(Typer.scala:1676)
	dotty.tools.dotc.typer.Typer.makeContextualFunction(Typer.scala:3260)
	dotty.tools.dotc.typer.Typer.typedUnadapted(Typer.scala:3194)
	dotty.tools.dotc.typer.ProtoTypes$FunProto.$anonfun$7(ProtoTypes.scala:509)
	dotty.tools.dotc.typer.ProtoTypes$FunProto.cacheTypedArg(ProtoTypes.scala:432)
	dotty.tools.dotc.typer.ProtoTypes$FunProto.typedArg(ProtoTypes.scala:510)
	dotty.tools.dotc.typer.Applications$ApplyToUntyped.typedArg(Applications.scala:913)
	dotty.tools.dotc.typer.Applications$ApplyToUntyped.typedArg(Applications.scala:913)
	dotty.tools.dotc.typer.Applications$Application.addTyped$1(Applications.scala:605)
	dotty.tools.dotc.typer.Applications$Application.matchArgs(Applications.scala:669)
	dotty.tools.dotc.typer.Applications$Application.init(Applications.scala:491)
	dotty.tools.dotc.typer.Applications$TypedApply.<init>(Applications.scala:795)
	dotty.tools.dotc.typer.Applications$ApplyToUntyped.<init>(Applications.scala:912)
	dotty.tools.dotc.typer.Applications.ApplyTo(Applications.scala:1142)
	dotty.tools.dotc.typer.Applications.ApplyTo$(Applications.scala:351)
	dotty.tools.dotc.typer.Typer.ApplyTo(Typer.scala:120)
	dotty.tools.dotc.typer.Applications.simpleApply$1(Applications.scala:985)
	dotty.tools.dotc.typer.Applications.realApply$1$$anonfun$2(Applications.scala:1068)
	dotty.tools.dotc.typer.Typer.tryEither(Typer.scala:3413)
	dotty.tools.dotc.typer.Applications.realApply$1(Applications.scala:1079)
	dotty.tools.dotc.typer.Applications.typedApply(Applications.scala:1117)
	dotty.tools.dotc.typer.Applications.typedApply$(Applications.scala:351)
	dotty.tools.dotc.typer.Typer.typedApply(Typer.scala:120)
	dotty.tools.dotc.typer.Typer.typedUnnamed$1(Typer.scala:3113)
	dotty.tools.dotc.typer.Typer.typedUnadapted(Typer.scala:3197)
	dotty.tools.dotc.typer.Typer.typed(Typer.scala:3274)
	dotty.tools.dotc.typer.Typer.typed(Typer.scala:3278)
	dotty.tools.dotc.typer.Typer.typedExpr(Typer.scala:3389)
	dotty.tools.dotc.typer.Typer.$anonfun$62(Typer.scala:2603)
	dotty.tools.dotc.inlines.PrepareInlineable$.dropInlineIfError(PrepareInlineable.scala:256)
	dotty.tools.dotc.typer.Typer.typedDefDef(Typer.scala:2603)
	dotty.tools.dotc.typer.Typer.typedNamed$1(Typer.scala:3095)
	dotty.tools.dotc.typer.Typer.typedUnadapted(Typer.scala:3196)
	dotty.tools.dotc.typer.Typer.typed(Typer.scala:3274)
	dotty.tools.dotc.typer.Typer.typed(Typer.scala:3278)
	dotty.tools.dotc.typer.Typer.traverse$1(Typer.scala:3300)
	dotty.tools.dotc.typer.Typer.typedStats(Typer.scala:3346)
	dotty.tools.dotc.typer.Typer.typedClassDef(Typer.scala:2790)
	dotty.tools.dotc.typer.Typer.typedTypeOrClassDef$1(Typer.scala:3101)
	dotty.tools.dotc.typer.Typer.typedNamed$1(Typer.scala:3105)
	dotty.tools.dotc.typer.Typer.typedUnadapted(Typer.scala:3196)
	dotty.tools.dotc.typer.Typer.typed(Typer.scala:3274)
	dotty.tools.dotc.typer.Typer.typed(Typer.scala:3278)
	dotty.tools.dotc.typer.Typer.traverse$1(Typer.scala:3300)
	dotty.tools.dotc.typer.Typer.typedStats(Typer.scala:3346)
	dotty.tools.dotc.typer.Typer.typedClassDef(Typer.scala:2790)
	dotty.tools.dotc.typer.Typer.typedTypeOrClassDef$1(Typer.scala:3101)
	dotty.tools.dotc.typer.Typer.typedNamed$1(Typer.scala:3105)
	dotty.tools.dotc.typer.Typer.typedUnadapted(Typer.scala:3196)
	dotty.tools.dotc.typer.Typer.typed(Typer.scala:3274)
	dotty.tools.dotc.typer.Typer.typed(Typer.scala:3278)
	dotty.tools.dotc.typer.Typer.traverse$1(Typer.scala:3300)
	dotty.tools.dotc.typer.Typer.typedStats(Typer.scala:3346)
	dotty.tools.dotc.typer.Typer.typedPackageDef(Typer.scala:2923)
	dotty.tools.dotc.typer.Typer.typedUnnamed$1(Typer.scala:3147)
	dotty.tools.dotc.typer.Typer.typedUnadapted(Typer.scala:3197)
	dotty.tools.dotc.typer.Typer.typed(Typer.scala:3274)
	dotty.tools.dotc.typer.Typer.typed(Typer.scala:3278)
	dotty.tools.dotc.typer.Typer.typedExpr(Typer.scala:3389)
	dotty.tools.dotc.typer.TyperPhase.typeCheck$$anonfun$1(TyperPhase.scala:47)
	scala.runtime.function.JProcedure1.apply(JProcedure1.java:15)
	scala.runtime.function.JProcedure1.apply(JProcedure1.java:10)
	dotty.tools.dotc.core.Phases$Phase.monitor(Phases.scala:477)
	dotty.tools.dotc.typer.TyperPhase.typeCheck(TyperPhase.scala:53)
	dotty.tools.dotc.typer.TyperPhase.$anonfun$4(TyperPhase.scala:99)
	scala.collection.Iterator$$anon$6.hasNext(Iterator.scala:479)
	scala.collection.Iterator$$anon$9.hasNext(Iterator.scala:583)
	scala.collection.immutable.List.prependedAll(List.scala:152)
	scala.collection.immutable.List$.from(List.scala:684)
	scala.collection.immutable.List$.from(List.scala:681)
	scala.collection.IterableOps$WithFilter.map(Iterable.scala:898)
	dotty.tools.dotc.typer.TyperPhase.runOn(TyperPhase.scala:100)
	dotty.tools.dotc.Run.runPhases$1$$anonfun$1(Run.scala:315)
	scala.runtime.function.JProcedure1.apply(JProcedure1.java:15)
	scala.runtime.function.JProcedure1.apply(JProcedure1.java:10)
	scala.collection.ArrayOps$.foreach$extension(ArrayOps.scala:1323)
	dotty.tools.dotc.Run.runPhases$1(Run.scala:337)
	dotty.tools.dotc.Run.compileUnits$$anonfun$1(Run.scala:350)
	dotty.tools.dotc.Run.compileUnits$$anonfun$adapted$1(Run.scala:360)
	dotty.tools.dotc.util.Stats$.maybeMonitored(Stats.scala:69)
	dotty.tools.dotc.Run.compileUnits(Run.scala:360)
	dotty.tools.dotc.Run.compileSources(Run.scala:261)
	dotty.tools.dotc.interactive.InteractiveDriver.run(InteractiveDriver.scala:161)
	dotty.tools.pc.MetalsDriver.run(MetalsDriver.scala:47)
	dotty.tools.pc.SemanticdbTextDocumentProvider.textDocument(SemanticdbTextDocumentProvider.scala:32)
	dotty.tools.pc.ScalaPresentationCompiler.semanticdbTextDocument$$anonfun$1(ScalaPresentationCompiler.scala:198)
```
#### Short summary: 

java.lang.AssertionError: assertion failed