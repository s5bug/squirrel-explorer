package tf.bug.fe

import narr.*
import org.scalablytyped.runtime.StringDictionary
import scala.annotation.switch
import scala.scalajs.js
import scala.scalajs.js.annotation.JSImport
import tf.bug.cnut.SqInstructionType
import tf.bug.worker.DragonboxApi
import typings.lezerLr.mod as lezerLr
import typings.codemirrorLanguage.mod as codemirrorLanguage
import typings.codemirrorLint.mod as codemirrorLint
import typings.codemirrorState.mod.Extension

object LezerCnutLanguage {
  @js.native
  @JSImport("@/cnut_lezer.grammar", "parser")
  object parser extends lezerLr.LRParser

  val configuredParser: lezerLr.LRParser =
    parser.configure(lezerLr.ParserConfig())

  val language: codemirrorLanguage.LRLanguage = {
    val data = typings.codemirrorLanguage.anon.LanguageData(configuredParser)
    data.setName("cnut")
    codemirrorLanguage.LRLanguage.define(data)
  }

  val cnut: codemirrorLanguage.LanguageSupport =
    new codemirrorLanguage.LanguageSupport(language, js.Array(
      language.data.of(StringDictionary()),
    ))

  def localAtSpotForOp(locals: NArray[LezerLocalVarInfo], spot: Int, index: Int)(using render: LezerRender): Option[LezerLocalVarInfo] = {
    locals.find { local =>
      local.pos.flatMap(_.value).exists { pos =>
        (pos == spot) && local.startOp.flatMap(_.value).exists { startOp =>
          local.endOp.flatMap(_.value).exists { endOp =>
            (startOp - 1) <= index && index <= endOp
          }
        }
      }
    }
  }

  def makeInfoDiagnostic(from: Double, to: Double, text: String): codemirrorLint.Diagnostic = {
    val base = codemirrorLint.Diagnostic(
      from = from,
      to = to,
      severity = codemirrorLint.Severity.hint,
      message = text,
    )

    base.setRenderMessage(_ => {
      val c = org.scalajs.dom.document.createElement("code")
      c.innerText = text
      c
    })

    base.setActionsVarargs(codemirrorLint.Action((_, _, _) => {
      org.scalajs.dom.window.navigator.clipboard.writeText(text)
    }, "Copy"))
  }

  def diagnosticForLocal(
    locals: NArray[LezerLocalVarInfo],
    opIdx: Int,
    localIdx: Int,
    node: LezerInt,
    diagnostics: js.Array[codemirrorLint.Diagnostic]
  )(using render: LezerRender): Unit = {
    val toStackSpot = localAtSpotForOp(locals, localIdx, opIdx)
    val name = toStackSpot.flatMap(_.name).flatMap(_.text)
    name.foreach(n => diagnostics.push(makeInfoDiagnostic(node.from, node.to, n.substring(1, n.length - 1))))
  }

  def diagnosticForLiteral(
    literals: NArray[LezerObject],
    literalIdx: Int,
    node: LezerInt,
    diagnostics: js.Array[codemirrorLint.Diagnostic]
  )(using render: LezerRender): Unit = {
    val literal = literals(literalIdx)
    literal.text.foreach { literalValue =>
      diagnostics.push(makeInfoDiagnostic(node.from, node.to, literalValue))
    }
  }

  def diagnosticForFunction(
    functions: NArray[LezerFunctionProto],
    functionIdx: Int,
    node: LezerInt,
    diagnostics: js.Array[codemirrorLint.Diagnostic]
  )(using render: LezerRender): Unit = {
    val function = functions(functionIdx)
    (function.name: Option[LezerObject]).foreach { nameValue =>
      nameValue.text.foreach { n =>
        diagnostics.push(makeInfoDiagnostic(node.from, node.to, n.substring(1, n.length - 1)))
      }
    }
  }

  def diagnosticForInstruction(
    functionNode: LezerFunctionProto,
    locals: NArray[LezerLocalVarInfo],
    literals: NArray[LezerObject],
    functions: NArray[LezerFunctionProto],
    myIndex: Int,
    instruction: LezerInstruction,
    diagnostics: js.Array[codemirrorLint.Diagnostic],
    floatDiagnosticBuilder: FloatDiagnosticBuilder,
  )(using render: LezerRender): Unit = for {
    it <- instruction.instructionType.flatMap(_.value)
    a0Node <- instruction.a0
    a0 <- a0Node.value
    a1Node <- instruction.a1
    a1 <- a1Node.value
    a2Node <- instruction.a2
    a2 <- a2Node.value
    a3Node <- instruction.a3
    a3 <- a3Node.value
  } do {
    it match {
      case SqInstructionType.Load =>
        diagnosticForLocal(locals, myIndex, a0, a0Node, diagnostics)
        diagnosticForLiteral(literals, a1, a1Node, diagnostics)
      case SqInstructionType.LoadInt =>
        diagnosticForLocal(locals, myIndex, a0, a0Node, diagnostics)
      case SqInstructionType.LoadFloat =>
        diagnosticForLocal(locals, myIndex, a0, a0Node, diagnostics)

        val floatBits = a1
        floatDiagnosticBuilder.addFloatDiagnostic(a1Node.from, a1Node.to, floatBits)
      case SqInstructionType.DLoad =>
        diagnosticForLocal(locals, myIndex, a0, a0Node, diagnostics)
        diagnosticForLiteral(literals, a1, a1Node, diagnostics)
        diagnosticForLocal(locals, myIndex, a2, a2Node, diagnostics)
        diagnosticForLiteral(literals, a3, a3Node, diagnostics)
      case SqInstructionType.PrepCallK =>
        diagnosticForLiteral(literals, a1, a1Node, diagnostics)
      case SqInstructionType.GetK =>
        diagnosticForLocal(locals, myIndex, a0, a0Node, diagnostics)
        diagnosticForLiteral(literals, a1, a1Node, diagnostics)
      case SqInstructionType.Arith =>
        diagnosticForLocal(locals, myIndex, a0, a0Node, diagnostics)
        diagnosticForLocal(locals, myIndex, a1, a1Node, diagnostics)
        diagnosticForLocal(locals, myIndex, a2, a2Node, diagnostics)

        val ch = a3.toChar
        diagnostics.push(makeInfoDiagnostic(a3Node.from, a3Node.to, ch.toString))
      case SqInstructionType.Bitw =>
        diagnosticForLocal(locals, myIndex, a0, a0Node, diagnostics)
        diagnosticForLocal(locals, myIndex, a1, a1Node, diagnostics)
        diagnosticForLocal(locals, myIndex, a2, a2Node, diagnostics)

        val op = (a3: @switch) match {
          case 0 => "&"
          case 2 => "|"
          case 3 => "^"
          case 4 => "<<"
          case 5 => ">>"
          case 6 => ">>>"
          case 7 => null
        }
        if op != null then diagnostics.push(makeInfoDiagnostic(a3Node.from, a3Node.to, op))
      case SqInstructionType.CompArith =>
        diagnosticForLocal(locals, myIndex, a1, a1Node, diagnostics)

        val ch = a3.toChar
        diagnostics.push(makeInfoDiagnostic(a3Node.from, a3Node.to, ch.toString))
      case SqInstructionType.CompArithL =>
        diagnosticForLocal(locals, myIndex, a0, a0Node, diagnostics)
        diagnosticForLocal(locals, myIndex, a1, a1Node, diagnostics)
        diagnosticForLocal(locals, myIndex, a2, a2Node, diagnostics)

        val ch = a3.toChar
        diagnostics.push(makeInfoDiagnostic(a3Node.from, a3Node.to, ch.toString))
      case SqInstructionType.Cmp =>
        diagnosticForLocal(locals, myIndex, a0, a0Node, diagnostics)
        diagnosticForLocal(locals, myIndex, a1, a1Node, diagnostics)
        diagnosticForLocal(locals, myIndex, a2, a2Node, diagnostics)

        val op = (a3: @switch) match {
          case 0 => ">"
          case 2 => ">="
          case 3 => "<"
          case 4 => "<="
          case _ => null
        }
        if op != null then diagnostics.push(makeInfoDiagnostic(a3Node.from, a3Node.to, op))
      case SqInstructionType.Closure =>
        diagnosticForLocal(locals, myIndex, a0, a0Node, diagnostics)
        diagnosticForFunction(functions, a1, a1Node, diagnostics)
      case _ => ()
    }
  }

  // maybe we opaque this to make it able to be IO
  def cnutLinter(dbox: DragonboxApi): Extension = codemirrorLint.linter(view => {
    given nodeText: LezerRender = LezerRender.of(safeOpt(_).map { node =>
      view.state.doc.sliceString(node.from, node.to)
    })

    codemirrorLanguage.forceParsing(view, view.state.doc.length)

    val diagnostics: js.Array[codemirrorLint.Diagnostic] = js.Array()
    val floatDiagnosticBuilder: FloatDiagnosticBuilder = FloatDiagnosticBuilder.of(dbox)
    codemirrorLanguage.syntaxTree(view.state).cursor().iterate(node => {
      if node.name == "FunctionProto" then {
        val lezerFunction = LezerFunctionProto.of(node.node)

        for {
          instructions <- lezerFunction.instructions
          localVarInfos <- lezerFunction.localVarInfos
          literals <- lezerFunction.literals
          functions <- lezerFunction.functions
        } do {
          var i = 0
          while i < instructions.length do {
            val instruction = instructions(i)

            diagnosticForInstruction(lezerFunction, localVarInfos, literals, functions, i, instruction, diagnostics, floatDiagnosticBuilder)

            i += 1
          }
        }
      }
    })
    floatDiagnosticBuilder.buildAndClean(diagnostics)
    diagnostics
  })
}
