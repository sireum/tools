// #Sireum
/*
 Copyright (c) 2017-2026,Robby, Kansas State University
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
import org.sireum.message._
import org.sireum.ops._
import org.sireum.lang.{ast => AST}
import org.sireum.lang.symbol._
import org.sireum.lang.symbol.Resolver._
import TransformerGen._
import org.sireum.lang.symbol.GlobalDeclarationResolver

object PrePostTransformerGen {

  def gen(
    isImmutable: B,
    isReversed: B,
    licenseOpt: Option[String],
    nameOpt: Option[String],
    fileUris: ISZ[String],
    programs: ISZ[AST.TopUnit.Program],
    exclude: ISZ[String],
    reporter: Reporter
  ): ST = {
    return genWithOpaqueTypes(isImmutable, isReversed, licenseOpt, nameOpt, fileUris, programs, exclude, reporter, ISZ[String]())
  }

  def genWithOpaqueTypes(
    isImmutable: B,
    isReversed: B,
    licenseOpt: Option[String],
    nameOpt: Option[String],
    fileUris: ISZ[String],
    programs: ISZ[AST.TopUnit.Program],
    exclude: ISZ[String],
    reporter: Reporter,
    opaqueTypes: ISZ[String]
  ): ST = {
    val gdr = GlobalDeclarationResolver(HashSMap.empty, HashSMap.empty, reporter)
    for (p <- programs) {
      gdr.resolveProgram(p)
    }
    val name = nameOpt.getOrElse(if (isImmutable) "Transformer" else "MTransformer")
    val t = PrePostTransformerGen(
      gdr.globalNameMap,
      gdr.globalTypeMap,
      AST.Util.ids2strings(programs(0).packageName.ids),
      isImmutable,
      isReversed,
      HashSet ++ exclude,
      reporter
    )
    t.opaqueTypes = HashSet ++ opaqueTypes
    val r = t.gen(licenseOpt, fileUris, name)
    reporter.reports(t.reporter.messages)
    return r
  }
}

@record class PrePostTransformerGen(
  val globalNameMap: NameMap,
  val globalTypeMap: TypeMap,
  val packageName: QName,
  val isImmutable: B,
  val isReversed: B,
  val exclude: HashSet[String],
  val reporter: Reporter
) {

  var opaqueTypes: HashSet[String] = HashSet.empty[String]

  val globalTypes: ISZ[TypeInfo] = sortedGlobalTypes(globalTypeMap)
  val poset: Poset[QName] = typePoset(globalTypeMap, globalTypes, reporter)
  val template: Template = if (isImmutable) Template.Transformer() else Template.MTransformer()
  var optionAdded: B = F
  var moptionAdded: B = F
  var collAdded: HashSet[String] = HashSet.empty[String]
  var specificAdded: HashSet[String] = HashSet.empty[String]
  var preMethods: ISZ[ST] = ISZ()
  var postMethods: ISZ[ST] = ISZ()
  var transformHelpers: ISZ[ST] = ISZ()
  var transformMethods: ISZ[ST] = ISZ()
  var transformSpecificMethods: ISZ[ST] = ISZ()

  def gen(licenseOpt: Option[String], fileUris: ISZ[String], name: String): ST = {
    for (ti <- globalTypes) {
      ti match {
        case ti: TypeInfo.Adt => genAdt(ti)
        case ti: TypeInfo.Sig => genRoot(ti.name, T)
        case _ =>
      }
    }
    return template.main(
      licenseOpt,
      fileUris,
      packageName,
      name,
      preMethods,
      postMethods,
      transformHelpers,
      transformMethods ++ transformSpecificMethods
    )
  }

  def genRoot(name: QName, isSig: B): Unit = {
    val rootTypeString = typeNameString(packageName, name)
    val rootTypeName = typeName(packageName, name)
    var preMethodCases = ISZ[ST]()
    var postMethodCases = ISZ[ST]()
    var methodCases = ISZ[ST]()
    val descendants = poset.descendantsOf(name).elements
    val sortedDescendants: ISZ[TypeInfo] = {
      var r = ISZ[TypeInfo]()
      for (d <- descendants) {
        globalTypeMap.get(d) match {
          case Some(info) => r = r :+ info
          case _ =>
        }
      }
      ISZOps(r).sortWith(ltTypeInfo(uriLt _))
    }
    for (child <- sortedDescendants) {
      child match {
        case childTI: TypeInfo.Adt if !childTI.ast.isRoot =>
          val p: (Option[ST], Option[ST]) =
            if (isSig || !poset.isChildOf(name, childTI.name))
              (Some(template.preAdapt(rootTypeString)), Some(template.postAdapt(rootTypeString)))
            else (None[ST](), None[ST]())
          val childIds = childTI.name
          val childTypeString = typeNameString(packageName, childIds)
          val childTypeName = typeName(packageName, childIds)
          preMethodCases = preMethodCases :+ template
            .preMethodRootCase(childTypeName, childTypeString, rootTypeString, p._1)
          postMethodCases = postMethodCases :+ template
            .postMethodRootCase(childTypeName, childTypeString, rootTypeString, p._2)
          val ac = genAdtChild(childTI)
          methodCases = methodCases :+ template.transformMethodCase(childTypeString, ac)
        case _ =>
      }
    }
    val transformMethodMatchST = template.transformMethodMatch(rootTypeString, methodCases)
    preMethods = preMethods :+ template.preMethodRoot(rootTypeName, rootTypeString, preMethodCases)
    postMethods = postMethods :+ template.postMethodRoot(rootTypeName, rootTypeString, postMethodCases)
    transformMethods = transformMethods :+
      template.transformMethod(rootTypeName, rootTypeString, transformMethodMatchST, None(), None())
  }

  def genAdt(ti: TypeInfo.Adt): Unit = {
    if (exclude.contains(ti.ast.id.value)) {
      return
    }
    if (!ti.ast.isDatatype && isImmutable) {
      reporter.error(
        ti.ast.id.attr.posOpt,
        transformerGenKind,
        s"Cannot generate immutable transformer for @record ${ti.ast.id.value}."
      )
      return
    }
    if (ti.ast.isRoot) {
      genRoot(ti.name, F)
    } else {
      val adTypeString = typeNameString(packageName, ti.name)
      val adTypeName = typeName(packageName, ti.name)
      val superTypeString: ST = adtParent(ti.name) match {
        case Some(name) => typeNameString(packageName, name)
        case _ => adTypeString
      }
      if (!isImmutable) {
        transformHelpers = transformHelpers :+ template.preMethodResult(adTypeName, superTypeString)
        transformHelpers = transformHelpers :+ template.postMethodResult(adTypeName, superTypeString)
      }
      preMethods = preMethods :+ template.preMethod(adTypeName, adTypeString, superTypeString)
      postMethods = postMethods :+ template.postMethod(adTypeName, adTypeString, superTypeString)
      if (hasNoAdtParent(ti.name)) {
        val ac = genAdtChild(ti)
        transformMethods = transformMethods :+
          template.transformMethod(adTypeName, adTypeString, template.transformMethodMatchSimple(ac), None(), None())
      }
    }
  }

  @pure def hasNoAdtParent(name: QName): B = {
    val parents = poset.parentsOf(name)
    for (parent <- parents.elements) {
      globalTypeMap.get(parent) match {
        case Some(_: TypeInfo.Adt) => return F
        case _ =>
      }
    }
    return T
  }

  def genAdtChild(ti: TypeInfo.Adt): AdtChild = {
    val methodCaseMembers = Buffer.create[ST]()
    val methodCaseChanges = Buffer.create[ST]()
    val methodCaseUpdates = Buffer.create[ST]()

    def renderType(tipe: AST.Type): ST = {
      tipe match {
        case t: AST.Type.Named =>
          val ids = AST.Util.ids2strings(t.name.ids)
          val base: ST = ti.scope.resolveType(globalTypeMap, ids) match {
            case Some(info: TypeInfo.Adt) => typeNameString(packageName, info.name)
            case Some(info: TypeInfo.Sig) => typeNameString(packageName, info.name)
            case _ => typeNameString(packageName, ids)
          }
          if (t.typeArgs.isEmpty) {
            return base
          }
          val args: ISZ[ST] = for (arg <- t.typeArgs) yield renderType(arg)
          return st"$base[${(args, ", ")}]"
        case _ =>
          reporter.error(tipe.posOpt, resolverKind, s"Unsupported type $tipe")
          return st""
      }
    }

    def addCollectionHelper(isImmutableCollection: B, indexType: ST): Unit = {
      val coll = st"${if (isImmutableCollection) "IS" else "MS"}$indexType".render
      if (!collAdded.contains(coll)) {
        collAdded = collAdded + coll
        transformHelpers = transformHelpers :+
          (if (isImmutableCollection) template.transformIS(indexType, isReversed)
           else template.transformMS(indexType, isReversed))
      }
    }

    def transformType(tipe: AST.Type, p: AST.AdtParam, ctx: ST, value: ST,
                      level: Z): Option[(ST, ST, Option[QName])] = {
      tipe match {
        case t: AST.Type.Named =>
          val ids = AST.Util.ids2strings(t.name.ids)
          val kind: String =
            if (ids.size == 1 || (ids.size == 3 && ids(0) == "org" && ids(1) == "sireum")) ids(ids.size - 1)
            else ""
          kind.native match {
            case _ if kind == "IS" || kind == "MS" || kind == "ISZ" || kind == "MSZ" ||
              kind == "Option" || kind == "MOption" =>
              val indexed = kind == "IS" || kind == "MS"
              val sequence = indexed || kind == "ISZ" || kind == "MSZ"
              val mutable = kind == "MS" || kind == "MSZ" || kind == "MOption"
              val arity: Z = if (indexed) 2 else 1
              if (t.typeArgs.size != arity) {
                reporter.error(t.attr.posOpt, transformerGenKind, s"Expecting $arity type arguments for $kind")
                return None()
              }
              if (isImmutable && mutable) {
                reporter.error(p.id.attr.posOpt, transformerGenKind,
                  s"$kind unsupported in immutable transformer for parameter ${p.id.value}")
                return None()
              }
              val childCtx = st"ctx$level"
              val childValue = st"e$level"
              transformType(t.typeArgs(arity - 1), p, childCtx, childValue, level + 1) match {
                case Some((childType, childCall, directNameOpt)) =>
                  val indexType: ST = if (indexed) renderType(t.typeArgs(0)) else st"Z"
                  val resultType: ST =
                    if (sequence) st"${if (mutable) "MS" else "IS"}[$indexType, $childType]"
                    else st"$kind[$childType]"
                  val helper: ST =
                    if (sequence) st"transform${if (mutable) "MS" else "IS"}$indexType"
                    else st"transform$kind"
                  if (sequence) {
                    addCollectionHelper(!mutable, indexType)
                  } else if (mutable && !moptionAdded) {
                    moptionAdded = T
                    transformHelpers = transformHelpers :+ template.transformMOption
                  } else if (!mutable && !optionAdded) {
                    optionAdded = T
                    transformHelpers = transformHelpers :+ template.transformOption
                  }
                  val f: ST = directNameOpt match {
                    case Some(name) => st"transform${typeName(packageName, name)} _"
                    case _ =>
                      if (isImmutable) st"($childCtx: Context, $childValue: $childType) => $childCall"
                      else st"($childValue: $childType) => $childCall"
                  }
                  val call: ST =
                    if (isImmutable) st"$helper($ctx, $value, $f)"
                    else st"$helper($value, $f)"
                  return Some((resultType, call, None[QName]()))
                case _ => return None()
              }
            case _ =>
              adtNameOpt(ti, ids, t.attr.posOpt) match {
                case Some(name) =>
                  transformSpecific(name)
                  val call: ST =
                    if (isImmutable) st"transform${typeName(packageName, name)}($ctx, $value)"
                    else st"transform${typeName(packageName, name)}($value)"
                  return Some((renderType(t), call, Some(name)))
                case _ => return None()
              }
          }
        case t: AST.Type.Tuple =>
          var hasTransform = F
          for (arg <- t.args) {
            if (transformType(arg, p, ctx, value, level).nonEmpty) {
              hasTransform = T
            }
          }
          if (hasTransform) {
            reporter.error(p.id.attr.posOpt, transformerGenKind,
              s"Tuple fields containing traversable types are unsupported for parameter ${p.id.value}")
          }
          return None()
        case _ =>
          reporter.error(p.id.attr.posOpt, transformerGenKind, s"Unsupported type for parameter ${p.id.value}")
          return None()
      }
    }

    var i = 0
    val params: ISZ[AST.AdtParam] =
      if (isReversed) for (j <- ti.ast.params.size - 1 to 0 by -1) yield ti.ast.params(j)
      else ti.ast.params
    for (p <- params) {
      val ctx: ST = if (i == 0) st"preR.ctx" else st"r${i - 1}.ctx"
      transformType(p.tipe, p, ctx, st"o2.${p.id.value}", 0) match {
        case Some((tipe, exp, _)) =>
          methodCaseMembers.append(template.transformMethodCaseMemberExpr(i, tipe, exp))
          methodCaseChanges.append(template.transformMethodCaseChanged(i))
          methodCaseUpdates.append(template.transformMethodCaseUpdate(i, p.id.value))
          i = i + 1
        case _ =>
      }
    }
    return AdtChild(i - 1, methodCaseMembers.toIS, methodCaseChanges.toIS, methodCaseUpdates.toIS)
  }


  def transformSpecific(name: QName): Unit = {
    globalTypeMap.get(name) match {
      case Some(ti: TypeInfo.Adt) if !ti.ast.isRoot && adtParent(ti.name).nonEmpty =>
        val adTypeString = typeNameString(packageName, name)
        val adts = adTypeString.render
        if (specificAdded.contains(adts)) {
          return
        }
        specificAdded = specificAdded + adts
        val adTypeName = typeName(packageName, name)
        val ac = genAdtChild(ti)
        transformSpecificMethods = transformSpecificMethods :+
          template.transformMethod(
            adTypeName,
            adTypeString,
            template.transformMethodMatchSimple(ac),
            Some(template.preAdapt(adTypeString)),
            Some(template.postAdapt(adTypeString))
          )
      case _ =>
    }
  }

  def adtParent(n: QName): Option[QName] = {
    var r: QName = ISZ()
    for (name <- poset.parentsOf(n).elements if r.isEmpty) {
      globalTypeMap.get(name) match {
        case Some(_: TypeInfo.Adt) => r = name
        case _ =>
      }
    }
    return if (r.isEmpty) None() else Some(r)
  }

  def adtTypeNameOpt(ti: TypeInfo.Adt, tipe: AST.Type): Option[QName] = {
    tipe match {
      case tipe: AST.Type.Named => return adtNameOpt(ti, AST.Util.ids2strings(tipe.name.ids), tipe.attr.posOpt)
      case _ => return None()
    }
  }

  def adtNameOpt(ti: TypeInfo.Adt, ids: QName, posOpt: Option[Position]): Option[QName] = {
    if (ids.size == 1 || (ids.size == 3 && ids(0) == "org" && ids(1) == "sireum")) {
      val leaf: String = if (ids.size == 1) ids(0) else ids(2)
      leaf.native match {
        case "B" => return None[QName]()
        case "C" => return None[QName]()
        case "Z" => return None[QName]()
        case "Z8" => return None[QName]()
        case "Z16" => return None[QName]()
        case "Z32" => return None[QName]()
        case "Z64" => return None[QName]()
        case "N" => return None[QName]()
        case "N8" => return None[QName]()
        case "N16" => return None[QName]()
        case "N32" => return None[QName]()
        case "N64" => return None[QName]()
        case "S8" => return None[QName]()
        case "S16" => return None[QName]()
        case "S32" => return None[QName]()
        case "S64" => return None[QName]()
        case "U8" => return None[QName]()
        case "U16" => return None[QName]()
        case "U32" => return None[QName]()
        case "U64" => return None[QName]()
        case "F32" => return None[QName]()
        case "F64" => return None[QName]()
        case "F16" => return None[QName]()
        case "R" => return None[QName]()
        case "String" => return None[QName]()
        case "ST" => return None[QName]()
        case "Position" => return None[QName]()
        case "FlatPos" => return None[QName]()
        case "PosInfo" => return None[QName]()
        case "DocInfo" => return None[QName]()
        case "Message" => return None[QName]()
        case _ =>
      }
    }
    if (ids.size == 4 && ids(0) == "org" && ids(1) == "sireum" && ids(2) == "message") {
      ids(3).native match {
        case "Position" => return None[QName]()
        case "FlatPos" => return None[QName]()
        case "PosInfo" => return None[QName]()
        case "DocInfo" => return None[QName]()
        case "Message" => return None[QName]()
        case _ =>
      }
    }
    ti.scope.resolveType(globalTypeMap, ids) match {
      case Some(info: TypeInfo.Adt) => return Some(info.name)
      case Some(info: TypeInfo.Sig) => return Some(info.name)
      case Some(_) => return None()
      case _ =>
        val sourceName = st"${(ids, ".")}".render
        if (opaqueTypes.contains(sourceName)) {
          return None[QName]()
        }
        reporter.error(posOpt, transformerGenKind, s"Could not find ${typeNameString(packageName, ids).render}.")
        return None()
    }
  }

}
