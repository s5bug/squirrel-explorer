package tf.bug.fe

import cats.effect.IO
import cats.effect.std.Dispatcher
import cats.syntax.all.*
import fs2.{Chunk, Collector, Stream}
import narr.*
import org.scalablytyped.runtime.StringDictionary
import scala.annotation.switch
import scala.scalajs.js
import scala.scalajs.js.annotation.JSImport
import tf.bug.NArrayFactory
import tf.bug.cnut.SqInstructionType
import tf.bug.worker.DragonboxApi
import typings.lezerCommon.mod as lezerCommon
import typings.lezerLr.mod as lezerLr
import typings.codemirrorLanguage.mod as codemirrorLanguage
import typings.codemirrorLint.{mod, mod as codemirrorLint}
import typings.codemirrorState.mod.Extension
import typings.lezerCommon.mod.SyntaxNode

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

  def localAtSpotForOp(locals: NArray[LezerLocalVarInfo], spot: Int, index: Int)(using render: LezerRender): Stream[IO, LezerLocalVarInfo] = {
    Stream.range(0, locals.length).map(locals(_)).evalFilter(local => {
      for {
        pos <- local.pos.value
        startOp <- local.startOp.value
        endOp <- local.endOp.value
      } yield pos == spot && (startOp - 1) <= index && index <= endOp
    }).take(1)
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
    node: LezerInt
  )(using render: LezerRender): Stream[IO, codemirrorLint.Diagnostic] = {
    val toStackSpot = localAtSpotForOp(locals, localIdx, opIdx)
    toStackSpot
      .evalMap(_.name.text)
      .map(n => makeInfoDiagnostic(node.from, node.to, n.substring(1, n.length - 1)))
  }

  def diagnosticForLiteral(
    literals: NArray[LezerObject],
    literalIdx: Int,
    node: LezerInt
  )(using render: LezerRender): Stream[IO, codemirrorLint.Diagnostic] = {
    val literal = literals(literalIdx)
    Stream.eval(literal.text).map { literalValue =>
      makeInfoDiagnostic(node.from, node.to, literalValue)
    }
  }

  def diagnosticForFunction(
    functions: NArray[LezerFunctionProto],
    functionIdx: Int,
    node: LezerInt
  )(using render: LezerRender): Stream[IO, codemirrorLint.Diagnostic] = {
    val function = functions(functionIdx)
    Stream.eval((function.name: LezerObject).text).map { n =>
      makeInfoDiagnostic(node.from, node.to, n.substring(1, n.length - 1))
    }
  }

  def diagnosticForInstruction(
    functionNode: LezerFunctionProto,
    locals: NArray[LezerLocalVarInfo],
    literals: NArray[LezerObject],
    functions: NArray[LezerFunctionProto],
    myIndex: Int,
    instruction: LezerInstruction,
    floatDiagnosticBuilder: FloatDiagnosticBuilder,
  )(using render: LezerRender): Stream[IO, codemirrorLint.Diagnostic] = Stream.force(for {
    _ <- IO.cede
    it <- instruction.instructionType.value
    a0Node = instruction.a0
    a0 <- a0Node.value
    a1Node = instruction.a1
    a1 <- a1Node.value
    a2Node = instruction.a2
    a2 <- a2Node.value
    a3Node = instruction.a3
    a3 <- a3Node.value
  } yield it match {
    case SqInstructionType.Load =>
      diagnosticForLocal(locals, myIndex, a0, a0Node) ++
        diagnosticForLiteral(literals, a1, a1Node)
    case SqInstructionType.LoadInt =>
      diagnosticForLocal(locals, myIndex, a0, a0Node)
    case SqInstructionType.LoadFloat =>
      val l = diagnosticForLocal(locals, myIndex, a0, a0Node)

      val floatBits = a1
      l ++ Stream.exec(floatDiagnosticBuilder.addFloatDiagnostic(a1Node.from, a1Node.to, floatBits))
    case SqInstructionType.DLoad =>
      diagnosticForLocal(locals, myIndex, a0, a0Node) ++
        diagnosticForLiteral(literals, a1, a1Node) ++
        diagnosticForLocal(locals, myIndex, a2, a2Node) ++
        diagnosticForLiteral(literals, a3, a3Node)
    case SqInstructionType.PrepCallK =>
      diagnosticForLiteral(literals, a1, a1Node)
    case SqInstructionType.GetK =>
      diagnosticForLocal(locals, myIndex, a0, a0Node) ++
        diagnosticForLiteral(literals, a1, a1Node)
    case SqInstructionType.Arith =>
      val l = diagnosticForLocal(locals, myIndex, a0, a0Node) ++
        diagnosticForLocal(locals, myIndex, a1, a1Node) ++
        diagnosticForLocal(locals, myIndex, a2, a2Node)

      val ch = a3.toChar
      l ++ Stream.emit(makeInfoDiagnostic(a3Node.from, a3Node.to, ch.toString))
    case SqInstructionType.Bitw =>
      val l = diagnosticForLocal(locals, myIndex, a0, a0Node) ++
        diagnosticForLocal(locals, myIndex, a1, a1Node) ++
        diagnosticForLocal(locals, myIndex, a2, a2Node)

      val op = (a3: @switch) match {
        case 0 => "&"
        case 2 => "|"
        case 3 => "^"
        case 4 => "<<"
        case 5 => ">>"
        case 6 => ">>>"
        case 7 => null
      }
      l ++ Stream.emit(op).filter(_ != null).map(makeInfoDiagnostic(a3Node.from, a3Node.to, _))
    case SqInstructionType.CompArith =>
      val l = diagnosticForLocal(locals, myIndex, a1, a1Node)

      val ch = a3.toChar
      l ++ Stream.emit(makeInfoDiagnostic(a3Node.from, a3Node.to, ch.toString))
    case SqInstructionType.CompArithL =>
      val l = diagnosticForLocal(locals, myIndex, a0, a0Node) ++
        diagnosticForLocal(locals, myIndex, a1, a1Node) ++
        diagnosticForLocal(locals, myIndex, a2, a2Node)

      val ch = a3.toChar
      l ++ Stream.emit(makeInfoDiagnostic(a3Node.from, a3Node.to, ch.toString))
    case SqInstructionType.Cmp =>
      val l = diagnosticForLocal(locals, myIndex, a0, a0Node) ++
        diagnosticForLocal(locals, myIndex, a1, a1Node) ++
        diagnosticForLocal(locals, myIndex, a2, a2Node)

      val op = (a3: @switch) match {
        case 0 => ">"
        case 2 => ">="
        case 3 => "<"
        case 4 => "<="
        case _ => null
      }
      l ++ Stream.emit(op).filter(_ != null).map(makeInfoDiagnostic(a3Node.from, a3Node.to, _))
    case SqInstructionType.Closure =>
      diagnosticForLocal(locals, myIndex, a0, a0Node) ++
        diagnosticForFunction(functions, a1, a1Node)
    case _ => Stream.empty
  })


  // emits a unit every time the cursor changes
  def iterateCursor(cursor: lezerCommon.TreeCursor): Stream[IO, Unit] = {
    def go(depth: Int): Stream[IO, Unit] = {
      val here = Stream.emit(())
      val down = IO(cursor.firstChild()).ifM(IO(go(depth + 1)), IO.pure(Stream.empty))
      val across =
        if depth == 0 then IO.pure(Stream.empty)
        else IO(cursor.nextSibling()).ifM(IO(go(depth)), IO(cursor.parent()).as(Stream.empty))
      here ++ Stream.force(down) ++ Stream.force(across)
    }
    go(0)
  }

  def cnutLinter(dispatch: Dispatcher[IO]): Extension = Codemirror.linter(dispatch) { view =>
    given render: LezerRender = new LezerRender {
      override def ensure(to: Int): IO[Unit] =
        Stream.repeatEval(IO(codemirrorLanguage.forceParsing(view, to, 8)).ifM(IO.unit.as(true), IO.cede.as(false))).takeWhile(success => !success).compile.drain
      override def progressFull: IO[Unit] =
        IO(codemirrorLanguage.forceParsing(view, view.state.doc.length, 8)).ifM(IO.unit, IO.cede)
      override def apply(sn: SyntaxNode): IO[String] =
        IO(view.state.doc.sliceString(sn.from, sn.to))
    }

    val result = Stream.exec(render.ensure(view.state.doc.length.toInt)) ++ Stream.resource(FloatDiagnosticBuilder.of()).flatMap { floatDiagnosticBuilder =>
      val cursor = codemirrorLanguage.syntaxTree(view.state).cursor()
      val diagnostics: Stream[IO, codemirrorLint.Diagnostic] = iterateCursor(cursor).evalMap { case () =>
        IO(cursor.name == "FunctionProto").ifM(IO {
          val lezerFunction = LezerFunctionProto.of(cursor.node)
          val instructions = lezerFunction.instructions
          val localVarInfos = lezerFunction.localVarInfos
          val literals = lezerFunction.literals
          val functions = lezerFunction.functions

          Stream.range(0, instructions.length).flatMap { i =>
            val instruction = instructions(i)

            diagnosticForInstruction(lezerFunction, localVarInfos, literals, functions, i, instruction, floatDiagnosticBuilder)
          }
        }, IO(Stream.empty))
      }.flatten
      val floatDiagnostics = floatDiagnosticBuilder.build
      diagnostics ++ floatDiagnostics
    }
    result.compile.to(DiagnosticArrayCollector)
  }
}

object DiagnosticArrayCollector extends Collector[codemirrorLint.Diagnostic] {
  final type Out = js.Array[codemirrorLint.Diagnostic]

  override def newBuilder: Collector.Builder[mod.Diagnostic, js.Array[mod.Diagnostic]] = {
    val array: js.Array[mod.Diagnostic] = js.Array()
    new Collector.Builder[mod.Diagnostic, js.Array[mod.Diagnostic]] {
      override def +=(c: Chunk[mod.Diagnostic]): Unit = c.foreach(array.push(_))
      override def result: js.Array[mod.Diagnostic] = array
    }
  }
}
