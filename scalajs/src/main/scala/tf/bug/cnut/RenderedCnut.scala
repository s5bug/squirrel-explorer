package tf.bug.cnut

import com.github.plokhotnyuk.jsoniter_scala.macros.*
import com.github.plokhotnyuk.jsoniter_scala.core.*
import narr.NArray
import scala.reflect.ClassTag
import scala.scalajs.js
import tf.bug.NArrayFactory
import tf.bug.worker.DragonboxApi

final case class RenderedCnut(
  string: String,
  renderedClosure: RenderedCnut.Closure,
  diagnostics: IArray[Diagnostic],
)

object RenderedCnut {

  given renderedCnutCodec: JsonValueCodec[RenderedCnut] = JsonCodecMaker.make(CodecMakerConfig.withAllowRecursiveTypes(true))
  
  private final val spaces256 = " " * 256
  // FIXME make this accept the builder and be tailrec
  inline def spaces(n: Int): String = spaces256.substring(0, n)

  def render(closure: SqClosure, diagnosticBuilder: Diagnostic.Builder): RenderedCnut = {
    val sb: StringBuilder = new StringBuilder()
    diagnosticBuilder.reset()

    val clos = renderClosure(0, sb, diagnosticBuilder, closure)

    RenderedCnut(sb.toString(), clos, diagnosticBuilder.build)
  }

  sealed abstract class Node {
    val from: Int
    val to: Int
  }

  final case class RenderedInt(value: Int, from: Int, to: Int) extends Node

  private def renderInt(indent: Int, builder: StringBuilder, value: Int): RenderedInt = {
    val from = builder.length
    builder ++= value.toString
    val to = builder.length

    RenderedInt(value, from, to)
  }

  final case class RenderedLong(value: Long, from: Int, to: Int) extends Node

  private def renderLong(indent: Int, builder: StringBuilder, value: Long): RenderedLong = {
    val from = builder.length
    builder ++= value.toString
    val to = builder.length

    RenderedLong(value, from, to)
  }

  final case class Arr[T](value: IArray[T], from: Int, to: Int) extends Node

  private def renderArray[T <: AnyRef, U <: Node](indent: Int, builder: StringBuilder, recurse: (Int, StringBuilder, T) => U, value: NArray[T])(using ct: ClassTag[U]): Arr[U] = {
    val from = builder.length
    builder ++= "[\n"
    val result = value.view.map { case v =>
      builder ++= spaces(2 + indent)
      val x = recurse(2 + indent, builder, v)
      builder ++= ",\n"
      x
    }.toArray
    builder ++= spaces(indent)
    builder ++= "]"
    val to = builder.length

    Arr(IArray.unsafeFromArray(result), from, to)
  }

  private def renderArrayIdx[T <: AnyRef, U <: Node](indent: Int, builder: StringBuilder, recurse: (Int, StringBuilder, T, Int) => U, value: NArray[T])(using ct: ClassTag[U]): Arr[U] = {
    val from = builder.length
    builder ++= "[\n"
    val result = value.view.zipWithIndex.map { case (v, idx) =>
      builder ++= spaces(2 + indent)
      val x = recurse(2 + indent, builder, v, idx)
      builder ++= ",\n"
      x
    }.toArray
    builder ++= spaces(indent)
    builder ++= "]"
    val to = builder.length

    Arr(IArray.unsafeFromArray(result), from, to)
  }
  
  private def renderIntArray[U <: Node](indent: Int, builder: StringBuilder, recurse: (Int, StringBuilder, Int) => U, value: NArray[Int])(using ct: ClassTag[U]): Arr[U] = {
    val from = builder.length
    builder ++= "[\n"
    val result = value.view.zipWithIndex.map { case (v, idx) =>
      builder ++= spaces(2 + indent)
      val x = recurse(2 + indent, builder, v)
      builder ++= ",\n"
      x
    }.toArray
    builder ++= spaces(indent)
    builder ++= "]"
    val to = builder.length

    Arr(IArray.unsafeFromArray(result), from, to)
  }

  final case class Obj(underlying: SqObject, from: Int, to: Int) extends Node

  private final def renderObj(indent: Int, builder: StringBuilder, value: SqObject): Obj = {
    val from = builder.length
    builder ++= value.show
    val to = builder.length

    Obj(value, from, to)
  }

  final case class Closure(
    sizeOfSqChar: RenderedLong,
    funcProto: FunctionProto,
    from: Int,
    to: Int
  ) extends Node

  private def renderClosure(indent: Int, builder: StringBuilder, diagnostics: Diagnostic.Builder, value: SqClosure): Closure = {
    val from = builder.length

    builder ++= "Closure(\n"

    builder ++= spaces(2 + indent)
    builder ++= "sizeOfSqChar = "
    val sizeOfSqChar = renderLong(2 + indent, builder, value.sizeOfSqChar)
    builder ++= ",\n"

    builder ++= spaces(2 + indent)
    builder ++= "funcProto = "
    val funcProto = renderFunctionProto(2 + indent, builder, diagnostics, value.funcProto)
    builder ++= "\n"

    builder ++= spaces(indent)
    builder ++= ")"

    val to = builder.length

    Closure(sizeOfSqChar, funcProto, from, to)
  }

  final case class FunctionProto(
    sourceName: Obj,
    name: Obj,
    literals: Arr[Obj],
    parameters: Arr[Obj],
    outerValues: Arr[OuterValue],
    localVarInfos: Arr[LocalVarInfo],
    // line infos explicitly excluded: maybe possible to exclude them using some sort of hkt thingy
    defaultParams: Arr[RenderedInt],
    instructions: Arr[Instruction],
    functions: Arr[FunctionProto],
    stackSize: RenderedInt,
    bgenerator: RenderedInt,
    varparams: RenderedInt,
    from: Int,
    to: Int
  ) extends Node

  private def renderFunctionProto(indent: Int, builder: StringBuilder, diagnostics: Diagnostic.Builder, value: SqFunctionProto): FunctionProto = {
    val from = builder.length
    builder ++= "FunctionProto(\n"

    builder ++= spaces(2 + indent)
    builder ++= "sourceName = "
    val sourceName = renderObj(2 + indent, builder, value.sourceName)
    builder ++= ",\n"

    builder ++= spaces(2 + indent)
    builder ++= "name = "
    val name = renderObj(2 + indent, builder, value.name)
    builder ++= ",\n"

    builder ++= spaces(2 + indent)
    builder ++= "literals = "
    val literals = renderArray(2 + indent, builder, renderObj, value.literals)
    builder ++= ",\n"

    builder ++= spaces(2 + indent)
    builder ++= "parameters = "
    val parameters = renderArray(2 + indent, builder, renderObj, value.parameters)
    builder ++= ",\n"

    builder ++= spaces(2 + indent)
    builder ++= "outerValues = "
    val outerValues = renderArray(2 + indent, builder, renderOuterValue, value.outerValues)
    builder ++= ",\n"

    builder ++= spaces(2 + indent)
    builder ++= "localVarInfos = "
    val localVarInfos = renderArray(2 + indent, builder, renderLocalVarInfo, value.localVarInfos)
    builder ++= ",\n"

    builder ++= spaces(2 + indent)
    builder ++= "defaultParams = "
    val defaultParams = renderIntArray(2 + indent, builder, renderInt, value.defaultParams)
    builder ++= ",\n"

    builder ++= spaces(2 + indent)
    builder ++= "instructions = "
    val instructions = renderArrayIdx(2 + indent, builder, renderInstruction(_, _, diagnostics, value, _, _), value.instructions)
    builder ++= ",\n"

    builder ++= spaces(2 + indent)
    builder ++= "functions = "
    val functions = renderArray(2 + indent, builder, renderFunctionProto(_, _, diagnostics, _), value.functions)
    builder ++= ",\n"

    builder ++= spaces(2 + indent)
    builder ++= "stackSize = "
    val stackSize = renderInt(2 + indent, builder, value.stackSize)
    builder ++= ",\n"

    builder ++= spaces(2 + indent)
    builder ++= "bgenerator = "
    val bgenerator = renderInt(2 + indent, builder, value.bgenerator)
    builder ++= ",\n"

    builder ++= spaces(2 + indent)
    builder ++= "varparams = "
    val varparams = renderInt(2 + indent, builder, value.varparams)
    builder ++= "\n"

    builder ++= spaces(indent)
    builder ++= ")"
    val to = builder.length

    FunctionProto(sourceName, name, literals, parameters, outerValues, localVarInfos, defaultParams, instructions, functions, stackSize, bgenerator, varparams, from, to)
  }

  final case class OuterValue(
    outerType: OuterType,
    src: Obj,
    name: Obj,
    from: Int,
    to: Int
  ) extends Node

  private def renderOuterValue(indent: Int, builder: StringBuilder, value: SqOuterValue): OuterValue = {
    val from = builder.length

    builder ++= "OuterValue(\n"

    builder ++= spaces(2 + indent)
    builder ++= "sqOuterType = "
    val outerType = renderOuterType(2 + indent, builder, value.sqOuterType)
    builder ++= ",\n"

    builder ++= spaces(2 + indent)
    builder ++= "src = "
    val src = renderObj(2 + indent, builder, value.src)
    builder ++= ",\n"

    builder ++= spaces(2 + indent)
    builder ++= "name = "
    val name = renderObj(2 + indent, builder, value.name)
    builder ++= "\n"

    builder ++= spaces(indent)
    builder ++= ")"

    val to = builder.length

    OuterValue(outerType, src, name, from, to)
  }

  final case class OuterType(
    underlying: SqOuterType,
    from: Int,
    to: Int
  ) extends Node

  private def renderOuterType(indent: Int, builder: StringBuilder, value: SqOuterType): OuterType = {
    val from = builder.length
    builder ++= value.toString
    val to = builder.length

    OuterType(value, from, to)
  }

  final case class LocalVarInfo(
    name: Obj,
    pos: RenderedLong,
    startOp: RenderedLong,
    endOp: RenderedLong,
    from: Int,
    to: Int
  ) extends Node

  private def renderLocalVarInfo(indent: Int, builder: StringBuilder, value: SqLocalVarInfo): LocalVarInfo = {
    val from = builder.length

    builder ++= "LocalVarInfo(name = "
    val name = renderObj(2 + indent, builder, value.name)
    builder ++= ", pos = "
    val pos = renderLong(2 + indent, builder, value.pos)
    builder ++= ", startOp = "
    val startOp = renderLong(2 + indent, builder, value.startOp)
    builder ++= ", endOp = "
    val endOp = renderLong(2 + indent, builder, value.endOp)
    builder ++= ")"

    val to = builder.length

    LocalVarInfo(name, pos, startOp, endOp, from, to)
  }

  final case class Instruction(
    instructionType: InstructionType,
    arg0: RenderedInt,
    arg1: RenderedInt,
    arg2: RenderedInt,
    arg3: RenderedInt,
    from: Int,
    to: Int
  ) extends Node

  private def renderInstruction(indent: Int, builder: StringBuilder, diagnosticBuilder: Diagnostic.Builder, parentFunction: SqFunctionProto, value: SqInstruction, idx: Int): Instruction = {
    val from = builder.length

    val it = renderInstructionType(indent, builder, value.sqInstructionType)
    builder ++= "("
    val arg0 = renderInt(2 + indent, builder, value.arg0)
    builder ++= ", "
    val arg1 = renderInt(2 + indent, builder, value.arg1)
    builder ++= ", "
    val arg2 = renderInt(2 + indent, builder, value.arg2)
    builder ++= ", "
    val arg3 = renderInt(2 + indent, builder, value.arg3)
    builder ++= ")"

    val to = builder.length

    val me = Instruction(it, arg0, arg1, arg2, arg3, from, to)
    
    diagnosticBuilder.visit(value, me, parentFunction, idx)
    
    me
  }

  final case class InstructionType(
    underlying: SqInstructionType,
    from: Int,
    to: Int
  ) extends Node

  private def renderInstructionType(indent: Int, builder: StringBuilder, value: SqInstructionType): InstructionType = {
    val from = builder.length
    builder ++= value.toString
    val to = builder.length

    InstructionType(value, from, to)
  }
  
  def diff(previous: RenderedCnut, next: RenderedCnut): js.Array[typings.codemirrorState.anon.From] = {
    val builder: js.Array[typings.codemirrorState.anon.From] = js.Array()
    
    diffClosure(builder, previous, previous.renderedClosure, next, next.renderedClosure)
    
    builder
  }
  
  private def replace(pc: RenderedCnut, pn: Node, nc: RenderedCnut, nn: Node): typings.codemirrorState.anon.From = {
    val c = typings.codemirrorState.anon.From(pn.from)
    c.to = pn.to
    c.insert = nc.string.substring(nn.from, nn.to)
    c
  }
  
  private def diffClosure(acc: js.Array[typings.codemirrorState.anon.From], pc: RenderedCnut, pv: Closure, nc: RenderedCnut, nv: Closure): Unit = {
    diffLong(acc, pc, pv.sizeOfSqChar, nc, nv.sizeOfSqChar)
    diffFunctionProto(acc, pc, pv.funcProto, nc, nv.funcProto)
  }
  
  private def diffInt(acc: js.Array[typings.codemirrorState.anon.From], pc: RenderedCnut, pv: RenderedInt, nc: RenderedCnut, nv: RenderedInt): Unit = {
    if pv.value != nv.value then acc.push(replace(pc, pv, nc, nv))
  }
  
  private def diffFunctionProto(acc: js.Array[typings.codemirrorState.anon.From], pc: RenderedCnut, pv: FunctionProto, nc: RenderedCnut, nv: FunctionProto): Unit = {
    diffObj(acc, pc, pv.sourceName, nc, nv.sourceName)
    diffObj(acc, pc, pv.name, nc, nv.name)
    diffArr(acc, diffObj, pc, pv.literals, nc, nv.literals)
    diffArr(acc, diffObj, pc, pv.parameters, nc, nv.parameters)
    diffArr(acc, diffOuterValue, pc, pv.outerValues, nc, nv.outerValues)
    diffArr(acc, diffLocalVarInfo, pc, pv.localVarInfos, nc, nv.localVarInfos)
    diffArr(acc, diffInt, pc, pv.defaultParams, nc, nv.defaultParams)
    diffArr(acc, diffInstruction, pc, pv.instructions, nc, nv.instructions)
    diffArr(acc, diffFunctionProto, pc, pv.functions, nc, nv.functions)
    diffInt(acc, pc, pv.stackSize, nc, nv.stackSize)
    diffInt(acc, pc, pv.bgenerator, nc, nv.bgenerator)
    diffInt(acc, pc, pv.varparams, nc, nv.varparams)
  }
  
  private def diffObj(acc: js.Array[typings.codemirrorState.anon.From], pc: RenderedCnut, pv: Obj, nc: RenderedCnut, nv: Obj): Unit = {
    if pv.underlying != nv.underlying then acc.push(replace(pc, pv, nc, nv))
  }
  
  private def diffArr[T <: Node](
    acc: js.Array[typings.codemirrorState.anon.From],
    recurse: (js.Array[typings.codemirrorState.anon.From], RenderedCnut, T, RenderedCnut, T) => Unit,
    pc: RenderedCnut, pv: Arr[T], nc: RenderedCnut, nv: Arr[T]
  ): Unit = {
    val matching = Math.min(pv.value.length, nv.value.length)
    
    if matching != 0 then {
      // change the items that match in length
      var i = 0
      while i < matching do {
        recurse(acc, pc, pv.value(i), nc, nv.value(i))
        i += 1
      }

      if pv.value.length != nv.value.length then {
        // the discrepancy is made up by "end of last item in array" to "end of array"
        // replace the old with the new
        val c = typings.codemirrorState.anon.From(pv.value(matching - 1).to)
        c.to = pv.to
        c.insert = nc.string.substring(nv.value(matching - 1).to, nv.to)
        acc.push(c)
      }
    } else {
      // replace the whole array
      acc.push(replace(pc, pv, nc, nv))
    }
  }
  
  private def diffOuterValue(acc: js.Array[typings.codemirrorState.anon.From], pc: RenderedCnut, pv: OuterValue, nc: RenderedCnut, nv: OuterValue): Unit = {
    diffOuterType(acc, pc, pv.outerType, nc, nv.outerType)
    diffObj(acc, pc, pv.src, nc, nv.src)
    diffObj(acc, pc, pv.name, nc, nv.name)
  }
  
  private def diffOuterType(acc: js.Array[typings.codemirrorState.anon.From], pc: RenderedCnut, pv: OuterType, nc: RenderedCnut, nv: OuterType): Unit = {
    if pv.underlying != nv.underlying then acc.push(replace(pc, pv, nc, nv))
  }
  
  private def diffLocalVarInfo(acc: js.Array[typings.codemirrorState.anon.From], pc: RenderedCnut, pv: LocalVarInfo, nc: RenderedCnut, nv: LocalVarInfo): Unit = {
    diffObj(acc, pc, pv.name, nc, nv.name)
    diffLong(acc, pc, pv.pos, nc, nv.pos)
    diffLong(acc, pc, pv.startOp, nc, nv.startOp)
    diffLong(acc, pc, pv.endOp, nc, nv.endOp)
  }
  
  private def diffLong(acc: js.Array[typings.codemirrorState.anon.From], pc: RenderedCnut, pv: RenderedLong, nc: RenderedCnut, nv: RenderedLong): Unit = {
    if pv.value != nv.value then acc.push(replace(pc, pv, nc, nv))
  }
  
  private def diffInstruction(acc: js.Array[typings.codemirrorState.anon.From], pc: RenderedCnut, pv: Instruction, nc: RenderedCnut, nv: Instruction): Unit = {
    diffInstructionType(acc, pc, pv.instructionType, nc, nv.instructionType)
    diffInt(acc, pc, pv.arg0, nc, nv.arg0)
    diffInt(acc, pc, pv.arg1, nc, nv.arg1)
    diffInt(acc, pc, pv.arg2, nc, nv.arg2)
    diffInt(acc, pc, pv.arg3, nc, nv.arg3)
  }
  
  private def diffInstructionType(acc: js.Array[typings.codemirrorState.anon.From], pc: RenderedCnut, pv: InstructionType, nc: RenderedCnut, nv: InstructionType): Unit = {
    if pv.underlying != nv.underlying then acc.push(replace(pc, pv, nc, nv))
  }

}
