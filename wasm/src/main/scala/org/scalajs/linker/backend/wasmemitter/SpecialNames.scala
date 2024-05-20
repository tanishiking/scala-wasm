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

  /** A unique simple method name to map all method *signatures* into `MethodName`s. */
  val normalizedSimpleMethodName = SimpleMethodName("m")

  // Memory Instructions
  val WasmMemoryAllocatorClass = ClassName("scala.scalajs.wasm.MemoryAllocator")
  val WasmMemorySegmentClass = ClassName("scala.scalajs.wasm.MemorySegment")
  val WasmMemoryAllocatorCtor = MethodName.constructor(Nil)
  val WasmMemorySegmentCtor = MethodName.constructor(List(IntRef, IntRef))

  val WasmMemorySegmentStartAddressField =
    FieldName(WasmMemorySegmentClass, SimpleFieldName("start"))
  val WasmMemorySegmentSizeField = FieldName(WasmMemorySegmentClass, SimpleFieldName("size"))
  val allocateMethodName =
    MethodName("allocate", List(IntRef), ClassRef(SpecialNames.WasmMemorySegmentClass))
  val freeMethodName = MethodName("free", Nil, VoidRef)

  val loadByteMethodName = MethodName("_loadByte", List(IntRef), IntRef)
  val loadIntMethodName = MethodName("_loadInt", List(IntRef), IntRef)
  val loadMethodNames = Set(loadByteMethodName, loadIntMethodName)

  val storeByteMethodName = MethodName("_storeByte", List(IntRef, ByteRef), VoidRef)
  val storeIntMethodName = MethodName("_storeInt", List(IntRef, IntRef), VoidRef)
  val storeMethodNames = Set(storeIntMethodName, storeByteMethodName)

  // WASI functions
  val WASI = ClassName("scala.scalajs.wasm.wasi$")
  val wasiFdWrite = MethodName("fdWrite", List(IntRef, IntRef, IntRef, IntRef), IntRef)
}
