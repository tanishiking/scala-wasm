package wasm.wasm4s

import scala.collection.mutable

import org.scalajs.ir.{Names => IRNames}
import org.scalajs.ir.{Types => IRTypes}

import Names.WasmLocalName

class WasmFunctionContext private (private val _receiver: Option[WasmLocal]) {
  private var cnt = 0
  private var labelIdx = 0

  val locals = new WasmSymbolTable[WasmLocalName, WasmLocal]()
  def receiver = _receiver.getOrElse(throw new Error("Can access to the receiver in this context."))

  private val registeredLabels =
    mutable.AnyRefMap.empty[IRNames.LabelName, (WasmImmediate.LabelIdx, IRTypes.Type)]

  def genLabel(): WasmImmediate.LabelIdx = {
    val label = WasmImmediate.LabelIdx(labelIdx)
    labelIdx += 1
    label
  }

  def registerLabel(irLabelName: IRNames.LabelName, expectedType: IRTypes.Type): WasmImmediate.LabelIdx = {
    val label = genLabel()
    registeredLabels(irLabelName) = (label, expectedType)
    label
  }

  def getLabelFor(irLabelName: IRNames.LabelName): (WasmImmediate.LabelIdx, IRTypes.Type) = {
    registeredLabels.getOrElse(irLabelName, {
      throw new IllegalArgumentException(s"Unknown label ${irLabelName.nameString}")
    })
  }

  def genSyntheticLocalName(): WasmLocalName = {
    val name = WasmLocalName.synthetic(cnt)
    cnt += 1
    name
  }
}

object WasmFunctionContext {
  def apply(): WasmFunctionContext = new WasmFunctionContext(None)
  def apply(receiver: WasmLocal): WasmFunctionContext = new WasmFunctionContext(Some(receiver))
  def apply(receiver: Option[WasmLocal]): WasmFunctionContext = new WasmFunctionContext(receiver)
}
