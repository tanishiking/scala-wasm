package wasm.utils

import org.scalajs.ir.Printers.IRTreePrinter

import org.scalajs.linker.standard.LinkedClass

object LinkedClassPrinters {
  def showLinkedClass(clazz: LinkedClass): String = {
    val writer = new java.io.StringWriter()
    val printer = new LinkedClassPrinter(writer)
    printer.print(clazz)
    printer.println()
    writer.toString()
  }

  private class LinkedClassPrinter(_out: java.io.Writer) extends IRTreePrinter(_out) {
    def print(clazz: LinkedClass): Unit = {
      print("linked ")
      print(clazz.kind.toString())
      print(" ")
      print(clazz.className)
      clazz.superClass.foreach { cls =>
        print(" extends ")
        print(cls)
        clazz.jsSuperClass.foreach { tree =>
          print(" (via ")
          print(tree)
          print(")")
        }
      }
      if (clazz.interfaces.nonEmpty) {
        print(" implements ")
        var rest = clazz.interfaces
        while (rest.nonEmpty) {
          print(rest.head)
          rest = rest.tail
          if (rest.nonEmpty)
            print(", ")
        }
      }
      clazz.jsNativeLoadSpec.foreach { spec =>
        print(" loadfrom ")
        print(spec)
      }
      print(" ")
      printColumn(
        clazz.fields
          ::: clazz.methods
          ::: clazz.jsConstructorDef.toList
          ::: clazz.exportedMembers
          ::: clazz.jsNativeMembers,
        "{",
        "",
        "}"
      )
    }
  }
}
