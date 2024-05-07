package org.scalajs.linker.backend.wasmemitter

import org.scalajs.ir.Names._
import org.scalajs.ir.Types._

object SpecialNames {
  /* Our back-end-specific box classes for the generic representation of
   * `char` and `long`. These classes are not part of the classpath. They are
   * generated automatically by `LibraryPatches`.
   */
  val CharBoxClass = BoxedCharacterClass.withSuffix("Box")
  val LongBoxClass = BoxedLongClass.withSuffix("Box")

  val CharBoxCtor = MethodName.constructor(List(CharRef))
  val LongBoxCtor = MethodName.constructor(List(LongRef))

  // The constructor of java.lang.Class
  val ClassCtor = MethodName.constructor(List(ClassRef(ObjectClass)))

  // js.JavaScriptException, for WrapAsThrowable and UnwrapFromThrowable
  val JSExceptionClass = ClassName("scala.scalajs.js.JavaScriptException")
  val JSExceptionCtor = MethodName.constructor(List(ClassRef(ObjectClass)))
  val JSExceptionField = FieldName(JSExceptionClass, SimpleFieldName("exception"))

  val hashCodeMethodName = MethodName("hashCode", Nil, IntRef)
}
