// #Sireum
/*
 Copyright (c) 2017-2025, Robby, Kansas State University
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
    if (!ti.ast.isDatatype && isImmutable) {
      reporter.error(
        ti.ast.id.attr.posOpt,
        transformerGenKind,
        s"Cannot generate immutable transformer for @record ${ti.ast.id.value}."
      )
      return
    }
    if (exclude.contains(ti.ast.id.value)) {
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
    var methodCaseMembers = ISZ[ST]()
    var methodCaseChanges = ISZ[ST]()
    var methodCaseUpdates = ISZ[ST]()

    def addChangedUpdate(i: Z, fieldName: String): Unit = {
      methodCaseChanges = methodCaseChanges :+ template.transformMethodCaseChanged(i)
      methodCaseUpdates = methodCaseUpdates :+ template.transformMethodCaseUpdate(i, fieldName)
    }

    def transformMethodCaseMemberS(
      isImmutableCollection: B,
      i: Z,
      indexType: ST,
      name: QName,
      fieldName: String
    ): Unit = {
      val adTypeString = typeNameString(packageName, name)
      val adTypeName = typeName(packageName, name)
      val transformMethodCaseMemberSST: ST =
        if (isImmutableCollection)
          template.transformMethodCaseMemberIS(i, i - 1, indexType, adTypeName, adTypeString, fieldName)
        else
          template.transformMethodCaseMemberMS(i, i - 1, indexType, adTypeName, adTypeString, fieldName)
      methodCaseMembers = methodCaseMembers :+ transformMethodCaseMemberSST
      addChangedUpdate(i, fieldName)

      val coll = s"${if (isImmutableCollection) "IS" else "MS"}$indexType"
      if (!collAdded.contains(coll)) {
        collAdded = collAdded + coll
        transformHelpers = transformHelpers :+
          (if (isImmutableCollection) template.transformIS(indexType, isReversed)
           else template.transformMS(indexType, isReversed))
      }
      transformSpecific(name)
    }

    var i = 0

    def genS(isImmutableCollection: B, indexType: ST, elementType: AST.Type, p: AST.AdtParam): Unit = {
      if (isImmutable && !isImmutableCollection) {
        reporter.error(
          p.id.attr.posOpt,
          transformerGenKind,
          s"MS unsupported in immutable transformer for parameter ${p.id.value}"
        )
        return
      }
      adtTypeNameOpt(ti, elementType) match {
        case Some(name) =>
          transformMethodCaseMemberS(isImmutableCollection, i, indexType, name, p.id.value)
          i = i + 1
        case _ =>
      }
    }

    def genOpt(isImmutableOpt: B, t: AST.Type, p: AST.AdtParam): Unit = {
      if (isImmutable && !isImmutableOpt) {
        reporter.error(
          p.id.attr.posOpt,
          transformerGenKind,
          s"MOption unsupported in immutable transformer for parameter ${p.id.value}"
        )
        return
      }
      adtTypeNameOpt(ti, t) match {
        case Some(name) =>
          val adTypeString = typeNameString(packageName, name)
          val adTypeName = typeName(packageName, name)
          val transformMethodCaseMemberOptionST: ST =
            if (isImmutableOpt)
              template.transformMethodCaseMemberOption(i, i - 1, adTypeName, adTypeString, p.id.value)
            else
              template.transformMethodCaseMemberMOption(i, adTypeName, adTypeString, p.id.value)
          methodCaseMembers = methodCaseMembers :+ transformMethodCaseMemberOptionST
          addChangedUpdate(i, p.id.value)
          if (isImmutableOpt && !optionAdded) {
            optionAdded = T
            transformHelpers = transformHelpers :+ template.transformOption
          } else if (!isImmutableOpt && !moptionAdded) {
            moptionAdded = T
            transformHelpers = transformHelpers :+ template.transformMOption
          }
          transformSpecific(name)
          i = i + 1
        case _ =>
      }
    }

    val params: ISZ[AST.AdtParam] =
      if (isReversed) for (i <- ti.ast.params.size - 1 to 0 by -1) yield ti.ast.params(i)
      else ti.ast.params
    for (p <- params) {
      val fieldName = p.id.value
      p.tipe match {
        case t: AST.Type.Named =>
          val tids = t.name.ids
          tids(tids.size - 1).value.native match {
            case "IS" =>
              val ts = typeString(packageName, t.typeArgs(0), reporter)
              genS(T, ts, t.typeArgs(1), p)
            case "MS" =>
              val ts = typeString(packageName, t.typeArgs(0), reporter)
              genS(F, ts, t.typeArgs(1), p)
            case "ISZ" => genS(T, st"Z", t.typeArgs(0), p)
            case "MSZ" => genS(F, st"Z", t.typeArgs(0), p)
            case "Option" => genOpt(T, t.typeArgs(0), p)
            case "MOption" => genOpt(F, t.typeArgs(0), p)
            case _ =>
              adtNameOpt(ti, AST.Util.ids2strings(t.name.ids), t.attr.posOpt) match {
                case Some(name) =>
                  val adTypeString = typeNameString(packageName, name)
                  val adTypeName = typeName(packageName, name)
                  methodCaseMembers = methodCaseMembers :+
                    template.transformMethodCaseMember(i, i - 1, adTypeName, adTypeString, p.id.value)
                  addChangedUpdate(i, fieldName)
                  transformSpecific(name)
                  i = i + 1
                case _ =>
              }
          }
        case _ =>
          reporter.error(p.id.attr.posOpt, resolverKind, s"Unsupported type for parameter ${p.id.value}")
      }
    }
    return AdtChild(i - 1, methodCaseMembers, methodCaseChanges, methodCaseUpdates)
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
    if (ids.size == 1) {
      ids(0).native match {
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
        case "R" => return None[QName]()
        case "String" => return None[QName]()
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
        reporter.error(posOpt, transformerGenKind, s"Could not find ${typeNameString(packageName, ids).render}.")
        return None()
    }
  }
}
