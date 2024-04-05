package wasm.ir2wasm

object EmbeddedConstants {
  /* Values returned by the `jsValueType` helper.
   *
   * 0: false
   * 1: true
   * 2: string
   * 3: number
   * 4: undefined
   * 5: everything else
   *
   * This encoding has the following properties:
   *
   * - false and true also return their value as the appropriate i32.
   * - the types implementing `Comparable` are consecutive from 0 to 3.
   */

  final val JSValueTypeFalse = 0
  final val JSValueTypeTrue = 1
  final val JSValueTypeString = 2
  final val JSValueTypeNumber = 3
  final val JSValueTypeUndefined = 4
  final val JSValueTypeOther = 5
}
