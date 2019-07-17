// #Sireum
/*
 Copyright (c) 2019, Robby, Kansas State University
 All rights reserved.

 Redistribution and use in source and binary forms, with or without
 modification, are permitted provided that the following conditions are met:

 1. Redistributions of source code must retain the above copyright notice, this
    list of conditions and the following disclaimer.
 2. Redistributions in binary form must reproduce the above copyright notice,
    this list of conditions and the following disclaimer in the documentation
    and/or other materials provided with the distribution.

 THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND
 ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
 WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
 DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT OWNER OR CONTRIBUTORS BE LIABLE FOR
 ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES
 (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES;
 LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND
 ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
 (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
 SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
 */

package org.sireum.tools

import org.sireum._
import org.sireum.bitcodec._
import org.sireum.bitcodec.Spec._
import org.sireum.lang.{ast => AST}

object BitCodecGraphGen {

  type GenResult = HashSMap[ISZ[String], Graph[BcNode, BcEdge]]

  @datatype trait BcNode {
    def path: ISZ[String]
  }

  object BcNode {

    @datatype class Container(val path: ISZ[String], elements: ISZ[Element]) extends BcNode

    @datatype class Sub(name: String, val path: ISZ[String]) extends BcNode

    @datatype class Branch(val path: ISZ[String]) extends BcNode

    @datatype class Element(name: String, size: String)

  }

  @datatype class BcEdge(labelOpt: Option[String])

  @pure def renderPreds(preds: ISZ[Pred]): String = {
    return st"${(for (pred <- preds) yield render(pred), ".")}".render
  }

  @pure def render(o: Pred): ST = {
    o match {
      case o: Pred.Boolean => return if (o.value) st"T" else st"F"
      case o: Pred.Bits => return st"""u${o.size}\"${o.value}\""""
      case o: Pred.Bytes => return st"u8[${(for (value <- o.value) yield conversions.Z.toU8(value),".")}]"
      case o: Pred.Shorts => return st"u16[${(for (value <- o.value) yield conversions.Z.toU16(value),".")}]"
      case o: Pred.Ints => return st"u32[${(for (value <- o.value) yield conversions.Z.toU32(value),".")}]"
      case o: Pred.Longs => return st"u64[${(for (value <- o.value) yield conversions.Z.toU64(value),".")}]"
      case o: Pred.Skip => return st"_${o.size}"
      case o: Pred.Between => return st"u${o.size}(${o.lo}..${o.hi})"
      case o: Pred.Not => return st"!(${render(o.pred)})"
      case o: Pred.Or => return st"(${(for (pred <- o.preds) yield render(pred), " | ")})"
    }
  }

  @pure def compress(g: Graph[BcNode, BcEdge]): Graph[BcNode, BcEdge] = {
    @pure def isEmptyContainer(node: BcNode): B = {
      node match {
        case node: BcNode.Container if node.elements.isEmpty => return T
        case _ => return F
      }
    }
    var r = Graph.empty[BcNode, BcEdge]
    for (node <- g.nodesInverse) {
      if (isEmptyContainer(node)) {
        for (out <- g.outgoing(node)) {
          val Graph.Edge.Data(_, dest, outData) = out
          for (in <- g.incoming(node)) {
            val Graph.Edge.Data(src, _, inData) = in
            val labelOpt: Option[String] = (inData.labelOpt, outData.labelOpt) match {
              case (Some(lIn), Some(lOut)) => Some(s"$lIn:$lOut")
              case (Some(lIn), _) => Some(lIn)
              case (_, Some(lOut)) => Some(lOut)
              case _ => None()
            }
            r = r.addDataEdge(BcEdge(labelOpt), src, dest)
          }
        }
      } else {
        r = r * node
        for (out <- g.outgoing(node) if !isEmptyContainer(out.dest)) {
          r = r * out.dest
          r = r.addEdge(out)
        }
        for (in <- g.incoming(node) if !isEmptyContainer(in.source)) {
          r = r * in.source
          r = r.addEdge(in)
        }
      }
    }
    return r
  }

  @pure def inlineGraph(g: Graph[BcNode, BcEdge], genResult: GenResult): Graph[BcNode, BcEdge] = {
    def isSubEdge(e: Graph.Edge[BcNode, BcEdge]): B = {
      e.source match {
        case _: BcNode.Sub => return T
        case _ =>
      }
      e.dest match {
        case _: BcNode.Sub => return T
        case _ =>
      }
      return F
    }
    var r = Graph.empty[BcNode, BcEdge]
    var subMap = HashMap.empty[ISZ[String], Graph[BcNode, BcEdge]]
    def getSources(node: BcNode): ISZ[BcNode] = {
      node match {
        case node: BcNode.Sub =>
          val subG = subMap.get(node.path).get
          var nodes = ISZ[BcNode]()
          for (subNode <- subG.nodesInverse if subG.outgoing(subNode).isEmpty) {
            nodes = nodes :+ subNode
          }
          return nodes
        case _ => return ISZ(node)
      }
    }
    def getDests(node: BcNode): ISZ[BcNode] = {
      node match {
        case node: BcNode.Sub =>
          val subG = subMap.get(node.path).get
          var nodes = ISZ[BcNode]()
          for (subNode <- subG.nodesInverse if subG.incoming(subNode).isEmpty) {
            nodes = nodes :+ subNode
          }
          return nodes
        case _ => return ISZ(node)
      }
    }

    for (node <- g.nodesInverse) {
      node match {
        case node: BcNode.Sub =>
          val subG = inlineGraph(genResult.get(node.path).get, genResult)
          subMap = subMap + node.path ~> subG
          for (subNode <- subG.nodesInverse) {
            r = r * subNode
          }
          for (subEdge <- subG.allEdges) {
            r = r.addEdge(subEdge)
          }
        case _ => r = r * node
      }
    }
    for (e <- g.allEdges) {
      val Graph.Edge.Data(src, dest, data) = e
      for (from <- getSources(src); to <- getDests(dest)) {
        r = r.addDataEdge(data, from, to)
      }
    }
    return r
  }

  @pure def graphDot(g: Graph[BcNode, BcEdge]): ST = {
    @pure def elementST(e: BcNode.Element): ST = {
      return st"<TD>${e.name}:${e.size}</TD>"
    }
    @pure def elementsST(elements: ISZ[BcNode.Element]): ST = {
      val r =
        st"""<TABLE BORDER="0" CELLBORDER="1" CELLSPACING="0">
            |  <TR>${for (e <- elements) yield elementST(e)}</TR>
            |</TABLE>"""
      return r
    }
    @pure def nodePath(n: BcNode): ST = {
      val path: ISZ[String] = if (n.path(0) == "") ops.ISZOps(n.path).drop(1) else n.path
      return st"./${(path, ".")}.svg"
    }

    var nodes = ISZ[ST]()
    var edges = ISZ[ST]()

    for (i <- 0 until g.nodesInverse.size) {
      val node = g.nodesInverse(i)
      val nodeName = s"n$i"
      node match {
        case node: BcNode.Container =>
          nodes = nodes :+
            st"""$nodeName [label=<
                |  ${elementsST(node.elements)}
                |>];"""
        case node: BcNode.Branch =>
          nodes = nodes :+ st"$nodeName [shape=point];"
        case node: BcNode.Sub =>
          nodes = nodes :+ st"""$nodeName [style=dotted, label="${node.name}", href="${nodePath(node)}"];"""
      }
    }

    for (es <- g.incomingEdges.values; edge <- es.elements) {
      val Graph.Internal.Edge.Data(src, dest, data) = edge
      data.labelOpt match {
        case Some(l) =>
          g.nodesInverse(dest) match {
            case _: BcNode.Branch => edges = edges :+ st"""n$src -> n$dest [arrowhead=none, label="$l"];"""
            case _ => edges = edges :+ st"""n$src -> n$dest [label="$l"];"""
          }
        case _ =>
          g.nodesInverse(dest) match {
            case _: BcNode.Branch => edges = edges :+ st"""n$src -> n$dest [arrowhead=none];"""
            case _ => edges = edges :+ st"""n$src -> n$dest;"""
          }
      }
    }

    val r =
      st"""digraph G {
          |  rankdir = LR;
          |  node [shape=plaintext];
          |  edge [arrowhead=vee];
          |
          |  ${(nodes, "\n")}
          |
          |  ${(edges, "\n")}
          |}"""
    return r
  }

  @pure def specDot(o: Spec, enums: HashSMap[String, AST.Stmt.Enum]): ST = {
    val gen = BitCodecGraphGen(enums)
    val g = gen.genSpecInlined(o)
    return graphDot(g)
  }
}

import BitCodecGraphGen._

@datatype class BitCodecGraphGen(enums: HashSMap[String, AST.Stmt.Enum]) {

  @pure def genSpecInlined(o: Spec): Graph[BcNode, BcEdge] = {
    val r = genSpec(o)
    return inlineGraph(r.values(r.size - 1), r)
  }

  @pure def genSpec(o: Spec): GenResult = {
    o match {
      case o: Concat =>
        return genSpecConcat(ISZ(o.name), o, HashSMap.empty)
      case _ =>
        val oNorm = Concat("", ISZ(o))
        val r = genSpecConcat(ISZ(), oNorm, HashSMap.empty)
        return r -- ISZ(ISZ(""))
    }
  }

  @pure def genSpecConcat(path: ISZ[String], o: Concat, acc: GenResult): GenResult = {
    var accs = acc
    var r = Graph.empty[BcNode, BcEdge]
    var current = BcNode.Container(path :+ s"${r.nodes.size}", ISZ())
    var currentIndex: Z = 0
    r = r * current

    def updateCurrent(node: BcNode.Container): Unit = {
      r = r(nodes = (r.nodes - current ~> currentIndex) + node ~> currentIndex, nodesInverse = r.nodesInverse(currentIndex ~> node))
      current = node
    }

    def setCurrent(node: BcNode.Container): Unit = {
      currentIndex = r.nodes.get(node).get
      current = node
    }

    def sub(from: BcNode, element: Concat, to: BcNode, inLabelOpt: Option[String], outLabelOpt: Option[String]): BcNode = {
      val elementNode = BcNode.Sub(element.name, path :+ element.name)
      r = r * elementNode
      r = r.addDataEdge(BcEdge(inLabelOpt), from, elementNode)
      r = r.addDataEdge(BcEdge(outLabelOpt), elementNode, to)
      accs = genSpecConcat(elementNode.path, element, accs)
      return elementNode
    }

    def predRepeat(isWhile: B, preds: ISZ[Pred], e: Spec): Unit = {
      val next = BcNode.Container(path:+ s"${r.nodes.size}", ISZ())
      r = r * next
      val predsLabel = renderPreds(preds)
      val eNorm: Concat = e match {
        case e: Concat => e
        case _ => Concat(ops.StringOps(e.name).firstToUpper, ISZ(e))
      }
      val (loopLabel, outLabel): (String, String) = if (isWhile) {
        (predsLabel, s"!($predsLabel)")
      } else {
        (s"!($predsLabel)", predsLabel)
      }
      val eNode = sub(current, eNorm, next, None(), Some(outLabel))
      r = r.addDataEdge(BcEdge(Some(loopLabel)), eNode, eNode)
    }

    for (element <- o.elements) {
      element match {
        case element: Concat =>
          val next = BcNode.Container(path:+ s"${r.nodes.size}", ISZ())
          sub(current, element, next, None(), None())
          setCurrent(next)
        case element: Poly =>
          val desc = element.polyDesc
          val dependsOn = st"${(desc.dependsOn, ", ")}"
          desc.compName match {
            case string"Union" =>
              val next = BcNode.Container(path :+ s"${r.nodes.size}", ISZ())
              r = r * next
              val branch = BcNode.Branch(path ++ ISZ(desc.name, s"${r.nodes.size}"))
              r = r * branch
              r = r.addDataEdge(BcEdge(Some(dependsOn.render)), current, branch)
              for (e <- desc.elementsOpt.get) {
                val eNorm: Concat = e match {
                  case e: Concat => e
                  case _ => Concat(ops.StringOps(e.name).firstToUpper, ISZ(e))
                }
                sub(branch, eNorm, next, None(), None())
              }
              setCurrent(next)
            case string"Repeat" =>
              val next = BcNode.Container(path :+ s"${r.nodes.size}", ISZ())
              r = r * next
              val labelOpt: Option[String] = Some(dependsOn.render)
              val e = desc.elementsOpt.get(0)
              val eNorm: Concat = e match {
                case e: Concat => e
                case _ => Concat(ops.StringOps(e.name).firstToUpper, ISZ(e))
              }
              val eNode = sub(current, eNorm, next, None(), labelOpt)
              r = r.addDataEdge(BcEdge(labelOpt), eNode, eNode)
            case string"Raw" =>
              updateCurrent(current(elements = current.elements :+
                BcNode.Element(desc.name, st"&lt;...($dependsOn)&gt;".render)))
          }
        case element: PredUnion =>
          val next = BcNode.Container(path :+ s"${r.nodes.size}", ISZ())
          r = r * next
          val branch = BcNode.Branch(path ++ ISZ(element.name, s"${r.nodes.size}"))
          r = r * branch
          r = r.addDataEdge(BcEdge(None()), current, branch)
          for (e <- element.subs) {
            val eNorm: Concat = e match {
              case PredSpec(_, spec: Concat) => spec
              case _ => Concat(ops.StringOps(e.spec.name).firstToUpper, ISZ(e.spec))
            }
            sub(branch, eNorm, next, Some(renderPreds(e.preds)), None())
          }
          setCurrent(next)
        case element: PredRepeatWhile =>
          predRepeat(T, element.preds, element.element)
        case element: PredRepeatUntil =>
          predRepeat(F, element.preds, element.element)
        case element: GenUnion =>
          val next = BcNode.Container(path :+ s"${r.nodes.size}", ISZ())
          r = r * next
          val branch = BcNode.Branch(path ++ ISZ(element.name, s"${r.nodes.size}"))
          r = r * branch
          r = r.addDataEdge(BcEdge(None()), current, branch)
          for (e <- element.subs) {
            val eNorm: Concat = e match {
              case e: Concat => e
              case _ => Concat(ops.StringOps(e.name).firstToUpper, ISZ(e))
            }
            sub(branch, eNorm, next, None(), None())
          }
          setCurrent(next)
        case element: GenRepeat =>
          val next = BcNode.Container(path :+ s"${r.nodes.size}", ISZ())
          r = r * next
          val labelOpt: Option[String] = None()
          val e = element.element
          val eNorm: Concat = e match {
            case e: Concat => e
            case _ => Concat(ops.StringOps(e.name).firstToUpper, ISZ(e))
          }
          val eNode = sub(current, eNorm, next, None(), labelOpt)
          r = r.addDataEdge(BcEdge(labelOpt), eNode, eNode)
        case element: GenRaw =>
          updateCurrent(current(elements = current.elements :+
            BcNode.Element(element.name, st"&lt;...&gt;".render)))
        case element: Boolean =>
          updateCurrent(current(elements = current.elements :+ BcNode.Element(element.name, "1")))
        case element: Enum =>
          enums.get(element.objectName) match {
            case Some(enum) =>
              updateCurrent(current(elements = current.elements :+ BcNode.Element(element.name,
                s"${BitCodecGen.bitWidth(enum.elements.size)}")))
            case _ =>
              updateCurrent(current(elements = current.elements :+ BcNode.Element(element.name, s"?")))
          }
        case element: Bits =>
          updateCurrent(current(elements = current.elements :+ BcNode.Element(element.name, s"${element.size}")))
        case element: Bytes =>
          updateCurrent(current(elements = current.elements :+ BcNode.Element(element.name, s"${element.size * 8}")))
        case element: Shorts =>
          updateCurrent(current(elements = current.elements :+ BcNode.Element(element.name, s"${element.size * 16}")))
        case element: Ints =>
          updateCurrent(current(elements = current.elements :+ BcNode.Element(element.name, s"${element.size * 32}")))
        case element: Longs =>
          updateCurrent(current(elements = current.elements :+ BcNode.Element(element.name, s"${element.size * 64}")))
        case element: Pads =>
          updateCurrent(current(elements = current.elements :+ BcNode.Element("(pads)", s"${element.size}")))
      }
    }
    return accs + path ~> compress(r)
  }
}
