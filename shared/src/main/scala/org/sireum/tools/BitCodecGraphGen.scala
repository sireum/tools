// #Sireum
/*
 Copyright (c) 2017-2022, Robby, Kansas State University
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

    @datatype class Container(val path: ISZ[String], val elements: ISZ[Element]) extends BcNode

    @datatype class Sub(val name: String, val path: ISZ[String]) extends BcNode

    @datatype class Branch(val path: ISZ[String]) extends BcNode

    @datatype class Element(val name: String, val size: String)

  }

  @datatype class BcEdge(val labelOpt: Option[String], val tooltipOpt: Option[String])

  @pure def renderPreds(preds: ISZ[Pred]): String = {
    return st"${(for (pred <- preds) yield render(pred), ".")}".render
  }

  @pure def render(o: Pred): ST = {
    o match {
      case o: Pred.Boolean => return if (o.value) st"T" else st"F"
      case o: Pred.Bits => return st"""u${o.size}\"${o.value}\""""
      case o: Pred.Bytes => return st"u8[${(for (value <- o.value) yield conversions.Z.toU8(value), ".")}]"
      case o: Pred.Shorts => return st"u16[${(for (value <- o.value) yield conversions.Z.toU16(value), ".")}]"
      case o: Pred.Ints => return st"u32[${(for (value <- o.value) yield conversions.Z.toU32(value), ".")}]"
      case o: Pred.Longs => return st"u64[${(for (value <- o.value) yield conversions.Z.toU64(value), ".")}]"
      case o: Pred.Floats => return st"f32[${(o.value, ",")}]"
      case o: Pred.Doubles => return st"f64[${(o.value, ",")}]"
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

    @pure def mergeTextOpts(tOpt1: Option[String], tOpt2: Option[String]): Option[String] = {
      (tOpt1, tOpt2) match {
        case (Some(t1), Some(t2)) => return Some(s"$t1:$t2")
        case (Some(t1), _) => return Some(t1)
        case (_, Some(t2)) => return Some(t2)
        case _ => return None()
      }
    }

    if (!ops.ISZOps(g.nodesInverse).exists((n: BcNode) => isEmptyContainer(n))) {
      return g
    }

    var r = Graph.empty[BcNode, BcEdge]
    var added = HashSet.empty[BcNode]
    var eliminated = HashSet.empty[BcNode]
    for (node <- g.nodesInverse if isEmptyContainer(node) && !added.contains(node)) {
      eliminated = eliminated + node
      for (out <- g.outgoing(node); in <- g.incoming(node)) {
        val Graph.Edge.Data(_, dest, outData) = out
        val Graph.Edge.Data(src, _, inData) = in
        val labelOpt = mergeTextOpts(inData.labelOpt, outData.labelOpt)
        val tooltipOpt = mergeTextOpts(inData.tooltipOpt, outData.tooltipOpt)
        r = r * src
        r = r * dest
        added = added + src
        added = added + dest
        r = r.addDataEdge(BcEdge(labelOpt, tooltipOpt), src, dest)
      }
    }
    for (node <- g.nodesInverse if !eliminated.contains(node)) {
      r = r * node
      for (out <- g.outgoing(node) if !eliminated.contains(out.dest)) {
        r = r * out.dest
        r = r.addEdge(out)
      }
      for (in <- g.incoming(node) if !eliminated.contains(in.source)) {
        r = r * in.source
        r = r.addEdge(in)
      }
    }

    return compress(r)
  }

  @pure def inlineGraph(g: Graph[BcNode, BcEdge], genResult: GenResult): Graph[BcNode, BcEdge] = {
    if (!ops.ISZOps(g.nodesInverse).exists((n: BcNode) => n.isInstanceOf[BcNode.Sub])) {
      return g
    }

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

    @pure def elementsST(path: ISZ[String], elements: ISZ[BcNode.Element]): ST = {
      val p = ops.ISZOps(path).dropRight(1)
      val r =
        st"""<TABLE BORDER="0" CELLBORDER="1" CELLSPACING="0" TOOLTIP="${(p, ".")}" HREF="#">
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
                |  ${elementsST(node.path, node.elements)}
                |>];"""
        case _: BcNode.Branch =>
          nodes = nodes :+ st"$nodeName [shape=point];"
        case node: BcNode.Sub =>
          nodes = nodes :+ st"""$nodeName [style=dotted, label="${node.name}", href="${nodePath(node)}"];"""
      }
    }

    for (es <- g.incomingEdges.values; edge <- es.elements) {
      val Graph.Internal.Edge.Data(src, dest, data) = edge
      data.labelOpt match {
        case Some(l) =>
          val tooltip: ST = data.tooltipOpt match {
            case Some(t) =>
              val tt = ops.StringOps(ops.StringOps(t).
                replaceAllLiterally("\n", "&#013;")).
                replaceAllLiterally("\"", "\\\"")
              st""", tooltip="$tt", href="#""""
            case _ => st""
          }
          g.nodesInverse(dest) match {
            case _: BcNode.Branch if !g.nodesInverse(src).isInstanceOf[BcNode.Branch] =>
              edges = edges :+ st"""n$src -> n$dest [arrowhead=none, xlabel="$l"$tooltip];"""
            case _ =>
              edges = edges :+ st"""n$src -> n$dest [xlabel="$l"$tooltip];"""
          }
        case _ =>
          g.nodesInverse(dest) match {
            case _: BcNode.Branch if !g.nodesInverse(src).isInstanceOf[BcNode.Branch] =>
              edges = edges :+ st"""n$src -> n$dest [arrowhead=none];"""
            case _ =>
              edges = edges :+ st"""n$src -> n$dest;"""
          }
      }
    }

    val r =
      st"""digraph G {
          |  rankdir = LR;
          |  graph [splines=ortho];
          |  node [shape=plaintext];
          |  edge [arrowhead=vee];
          |
          |  ${(nodes, "\n")}
          |
          |  ${(edges, "\n")}
          |}"""
    return r
  }

  @pure def specDot(o: Spec,
                    text: String,
                    enums: HashSMap[String, AST.Stmt.Enum],
                    funs: HashSMap[String, (AST.Exp.Fun, AST.Type)]): ST = {
    val gen = BitCodecGraphGen(text, enums, funs)
    val g = gen.genSpecInlined(o)
    return graphDot(compress(g))
  }
}

import BitCodecGraphGen._

@datatype class BitCodecGraphGen(val text: String,
                                 val enums: HashSMap[String, AST.Stmt.Enum],
                                 val funs: HashSMap[String, (AST.Exp.Fun, AST.Type)]) {

  @pure def genSpecInlined(o: Spec): Graph[BcNode, BcEdge] = {
    val r = genSpec(o)
    return inlineGraph(r.values(r.size - 1), r)
  }

  @pure def genSpec(o: Spec): GenResult = {
    o match {
      case o: ConcatImpl =>
        return genSpecConcat(ISZ(o.name), o, HashSMap.empty)
      case _ =>
        val oNorm = Concat("", ISZ(o))
        val r = genSpecConcat(ISZ(), oNorm, HashSMap.empty)
        return r -- ISZ(ISZ(""))
    }
  }

  @pure def genSpecConcat(path: ISZ[String], o: ConcatImpl, acc: GenResult): GenResult = {
    var accs = acc
    var r = Graph.empty[BcNode, BcEdge]
    var current = BcNode.Container(path :+ s"${r.nodes.size}", ISZ())
    var currentIndex: Z = 0
    var subNumMap = HashMap.empty[String, Z]
    r = r * current

    def normalize(s: Spec): ConcatImpl = {
      s match {
        case s: ConcatImpl => return s
        case s: Composite => return ConcatImpl(ops.StringOps(s.name).firstToUpper, ISZ(s), s.asOpt)
        case _ => return Concat(ops.StringOps(s.name).firstToUpper, ISZ(s))
      }
    }

    def updateCurrent(node: BcNode.Container): Unit = {
      r = r(nodes = (r.nodes - current ~> currentIndex) + node ~> currentIndex, nodesInverse = r.nodesInverse(currentIndex ~> node))
      current = node
    }

    def setCurrent(node: BcNode.Container): Unit = {
      currentIndex = r.nodes.get(node).get
      current = node
    }

    def sub(from: BcNode, element: ConcatImpl, to: BcNode, inLabelOpt: Option[String], outLabelOpt: Option[String]): Unit = {
      var name: String = element.name
      subNumMap.get(name) match {
        case Some(n) =>
          subNumMap = subNumMap + name ~> (n + 1)
          name = s"$name$n"
        case _ => subNumMap = subNumMap + name ~> 2
      }
      val elementNode = BcNode.Sub(element.name, path :+ ops.StringOps(element.asOpt.getOrElse(name)).firstToLower)
      r = r * elementNode
      r = r.addDataEdge(BcEdge(inLabelOpt, None()), from, elementNode)
      r = r.addDataEdge(BcEdge(outLabelOpt, None()), elementNode, to)
      accs = genSpecConcat(elementNode.path, element, accs)
    }

    def predRepeat(isWhile: B, preds: ISZ[Pred], e: Spec, maxElements: Z): Unit = {
      val bPre = BcNode.Branch(path :+ s"${r.nodes.size}")
      r = r * bPre
      val bPost = BcNode.Branch(path :+ s"${r.nodes.size}")
      r = r * bPost
      val next = BcNode.Container(path :+ s"${r.nodes.size}", ISZ())
      r = r * next
      val predsLabel = renderPreds(preds)
      val eNorm: ConcatImpl = normalize(e)
      val loopLabel: String = if (isWhile) predsLabel else s"!($predsLabel)"
      sub(bPre, eNorm, bPost, None(), if (maxElements > 0) Some(s"max: $maxElements") else None())
      r = r.addDataEdge(BcEdge(None(), None()), current, bPre)
      r = r.addDataEdge(BcEdge(Some(loopLabel), None()), bPost, bPre)
      r = r.addDataEdge(BcEdge(None(), None()), bPost, next)
      setCurrent(next)
    }

    for (element <- o.elements) {
      element match {
        case element: ConcatImpl =>
          val next = BcNode.Container(path :+ s"${r.nodes.size}", ISZ())
          sub(current, element, next, None(), None())
          setCurrent(next)
        case element: Poly =>
          val desc = element.polyDesc
          val dependsOn = st"${(desc.dependsOn, ", ")}"
          def tooltipOpt: Option[String] = {
            funs.get(desc.name) match {
              case Some((fun, _)) =>
                val fpos = fun.posOpt.get
                return Some(BitCodecGen.reorientLines(ops.StringOps(text).
                  substring(fpos.offset, fpos.offset + fpos.length), fpos.beginColumn - 1))
              case _ => return None()
            }
          }
          desc.compName match {
            case string"Union" =>
              val bPre = BcNode.Branch(path :+ s"${r.nodes.size}")
              r = r * bPre
              val bPost = BcNode.Branch(path :+ s"${r.nodes.size}")
              r = r * bPost
              val next = BcNode.Container(path :+ s"${r.nodes.size}", ISZ())
              r = r * next
              val elements = desc.elementsOpt.get
              for (i <- 0 until elements.size) {
                val e = elements(i)
                val eNorm: ConcatImpl = normalize(e)
                sub(bPre, eNorm, bPost, Some(s"$i"), None())
              }
              val label = dependsOn.render
              r = r.addDataEdge(BcEdge(Some(label), tooltipOpt), current, bPre)
              r = r.addDataEdge(BcEdge(None(), None()), bPost, next)
              setCurrent(next)
            case string"Repeat" =>
              val bPre = BcNode.Branch(path :+ s"${r.nodes.size}")
              r = r * bPre
              val bPost = BcNode.Branch(path :+ s"${r.nodes.size}")
              r = r * bPost
              val next = BcNode.Container(path :+ s"${r.nodes.size}", ISZ())
              r = r * next
              val loopLabel = dependsOn.render
              val e = desc.elementsOpt.get(0)
              val eNorm: ConcatImpl = normalize(e)
              sub(bPre, eNorm, bPost, None(), if (desc.max > 0) Some(s"max: ${desc.max}") else None())
              r = r.addDataEdge(BcEdge(None(), None()), current, bPre)
              r = r.addDataEdge(BcEdge(Some(loopLabel), tooltipOpt), bPost, bPre)
              r = r.addDataEdge(BcEdge(None(), None()), bPost, next)
              setCurrent(next)
            case string"Raw" =>
              val max: String = if (desc.max > 0) s"max: ${desc.max}, " else ""
              updateCurrent(current(elements = current.elements :+
                BcNode.Element(desc.name, st"&lt;$max...($dependsOn)&gt;".render)))
          }
        case element: PredUnionImpl =>
          val bPre = BcNode.Branch(path :+ s"${r.nodes.size}")
          r = r * bPre
          val bPost = BcNode.Branch(path :+ s"${r.nodes.size}")
          r = r * bPost
          val next = BcNode.Container(path :+ s"${r.nodes.size}", ISZ())
          r = r * next
          for (e <- element.subs) {
            val eNorm: ConcatImpl = normalize(e.spec)
            sub(bPre, eNorm, bPost, Some(renderPreds(e.preds)), None())
          }
          r = r.addDataEdge(BcEdge(None(), None()), current, bPre)
          r = r.addDataEdge(BcEdge(None(), None()), bPost, next)
          setCurrent(next)
        case element: PredRepeatWhileImpl =>
          predRepeat(T, element.preds, element.element, element.maxElements)
        case element: PredRepeatUntilImpl =>
          predRepeat(F, element.preds, element.element, element.maxElements)
        case element: GenUnionImpl =>
          val bPre = BcNode.Branch(path :+ s"${r.nodes.size}")
          r = r * bPre
          val bPost = BcNode.Branch(path :+ s"${r.nodes.size}")
          r = r * bPost
          val next = BcNode.Container(path :+ s"${r.nodes.size}", ISZ())
          r = r * next
          for (e <- element.subs) {
            val eNorm: ConcatImpl = normalize(e)
            sub(bPre, eNorm, bPost, None(), None())
          }
          r = r.addDataEdge(BcEdge(None(), None()), current, bPre)
          r = r.addDataEdge(BcEdge(None(), None()), bPost, next)
          setCurrent(next)
        case element: GenRepeatImpl =>
          val bPre = BcNode.Branch(path :+ s"${r.nodes.size}")
          r = r * bPre
          val bPost = BcNode.Branch(path :+ s"${r.nodes.size}")
          r = r * bPost
          val next = BcNode.Container(path :+ s"${r.nodes.size}", ISZ())
          r = r * next
          val e = element.element
          val eNorm: ConcatImpl = normalize(e)
          sub(bPre, eNorm, bPost, None(), if (element.maxElements > 0) Some(s"max: ${element.maxElements}") else None())
          r = r.addDataEdge(BcEdge(None(), None()), current, bPre)
          r = r.addDataEdge(BcEdge(None(), None()), bPost, bPre)
          r = r.addDataEdge(BcEdge(None(), None()), bPost, next)
          setCurrent(next)
        case element: GenRawImpl =>
          val max: String = if (element.maxSize > 0) s"max: ${element.maxSize}" else ""
          updateCurrent(current(elements = current.elements :+
            BcNode.Element(element.name, st"&lt;$max...&gt;".render)))
        case element: Boolean =>
          updateCurrent(current(elements = current.elements :+ BcNode.Element(element.name, "B")))
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
        case element: BytesImpl =>
          val sizeOrVal: String =
            (element.minOpt, element.maxOpt) match {
              case (Some(min), Some(max)) =>
                if (min == max) s"= $min"
                else if (element.size == 1) s"$min..$max"
                else s"${element.size}*8; $min..$max"
              case _ => s"${element.size}*8"
            }
          updateCurrent(current(elements = current.elements :+ BcNode.Element(element.name, s"${element.size * 8} ($sizeOrVal)")))
        case element: ShortsImpl =>
          val sizeOrVal: String =
            (element.minOpt, element.maxOpt) match {
              case (Some(min), Some(max)) =>
                if (min == max) s"= $min"
                else if (element.size == 1) s"$min..$max"
                else s"${element.size}*16; $min..$max"
              case _ => s"${element.size}*16"
            }
          updateCurrent(current(elements = current.elements :+ BcNode.Element(element.name, s"${element.size * 16} ($sizeOrVal)")))
        case element: IntsImpl =>
          val sizeOrVal: String =
            (element.minOpt, element.maxOpt) match {
              case (Some(min), Some(max)) =>
                if (min == max) s"= $min"
                else if (element.size == 1) s"$min..$max"
                else s"${element.size}*32; $min..$max"
              case _ => s"${element.size}*32"
            }
          updateCurrent(current(elements = current.elements :+ BcNode.Element(element.name, s"${element.size * 32} ($sizeOrVal)")))
        case element: LongsImpl =>
          val sizeOrVal: String =
            (element.minOpt, element.maxOpt) match {
              case (Some(min), Some(max)) =>
                if (min == max) s"= $min"
                else if (element.size == 1) s"$min..$max"
                else s"${element.size}*64; $min..$max"
              case _ => s"${element.size}*64"
            }
          updateCurrent(current(elements = current.elements :+ BcNode.Element(element.name, s"${element.size * 64} ($sizeOrVal)")))
        case element: FloatsImpl =>
          val sizeOrVal: String =
            (element.minOpt, element.maxOpt) match {
              case (Some(min), Some(max)) =>
                if (element.size == 1) s"$min..$max"
                else s"${element.size}*32; $min..$max"
              case _ => s"${element.size}*32"
            }
          updateCurrent(current(elements = current.elements :+ BcNode.Element(element.name, s"${element.size * 32} ($sizeOrVal)")))
        case element: DoublesImpl =>
          val sizeOrVal: String =
            (element.minOpt, element.maxOpt) match {
              case (Some(min), Some(max)) =>
                if (element.size == 1) s"$min..$max"
                else s"${element.size}*64; $min..$max"
              case _ => s"${element.size}*64"
            }
          updateCurrent(current(elements = current.elements :+ BcNode.Element(element.name, s"${element.size * 64} ($sizeOrVal)")))
        case element: Pads =>
          updateCurrent(current(elements = current.elements :+ BcNode.Element("(pads)", s"${element.size}")))
      }
    }
    return accs + path ~> r
  }
}
