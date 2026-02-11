package tf.bug.fe

import narr.*
import org.scalablytyped.runtime.StringDictionary
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
      local.pos.map(_.value).exists { pos =>
        (pos == spot) && local.startOp.map(_.value).exists { startOp =>
          local.endOp.map(_.value).exists { endOp =>
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
      severity = codemirrorLint.Severity.info,
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

  def diagnosticForInstruction(
    functionNode: LezerFunctionProto,
    locals: NArray[LezerLocalVarInfo],
    literals: NArray[LezerObject],
    myIndex: Int,
    instruction: LezerInstruction,
    diagnostics: js.Array[codemirrorLint.Diagnostic],
    floatDiagnosticBuilder: FloatDiagnosticBuilder,
  )(using render: LezerRender): Unit = for {
    it <- instruction.instructionType.map(_.value)
    a0Node <- instruction.a0
    a0 = a0Node.value
    a1Node <- instruction.a1
    a1 = a1Node.value
  } do {
    it match {
      case SqInstructionType.Load =>
        val toStackSpot = localAtSpotForOp(locals, a0, myIndex)
        val name = toStackSpot.flatMap(_.name).map(_.text)

        val literal = literals(a1)
        val literalValue = literal.text

        name.foreach(n => diagnostics.push(makeInfoDiagnostic(a0Node.from, a0Node.to, n.substring(1, n.length - 1))))
        diagnostics.push(makeInfoDiagnostic(a1Node.from, a1Node.to, literalValue))
      case SqInstructionType.LoadInt =>
        val toStackSpot = localAtSpotForOp(locals, a0, myIndex)
        val name = toStackSpot.flatMap(_.name).map(_.text)

        name.foreach(n => diagnostics.push(makeInfoDiagnostic(a0Node.from, a0Node.to, n.substring(1, n.length - 1))))
      case SqInstructionType.LoadFloat =>
        val toStackSpot = localAtSpotForOp(locals, a0, myIndex)
        val name = toStackSpot.flatMap(_.name).map(_.text)

        val floatBits = a1

        name.foreach(n => diagnostics.push(makeInfoDiagnostic(a0Node.from, a0Node.to, n.substring(1, n.length - 1))))
        floatDiagnosticBuilder.addFloatDiagnostic(a1Node.from, a1Node.to, floatBits)
      case _ => ()
    }
  }

  // maybe we opaque this to make it able to be IO
  def cnutLinter(dbox: DragonboxApi): Extension = codemirrorLint.linter(view => {
    given nodeText: LezerRender = LezerRender.of(node => {
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
        } do {
          var i = 0
          while i < instructions.length do {
            val instruction = instructions(i)

            diagnosticForInstruction(lezerFunction, localVarInfos, literals, i, instruction, diagnostics, floatDiagnosticBuilder)

            i += 1
          }
        }
      }
    })
    floatDiagnosticBuilder.buildAndClean(diagnostics)
    diagnostics
  })
}
