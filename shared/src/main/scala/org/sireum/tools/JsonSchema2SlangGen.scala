// #Sireum
package org.sireum.tools

import org.sireum._
import org.sireum.lang.symbol.Resolver.QName
import org.sireum.message.Reporter
import org.sireum.parser.json.AST

object JsonSchema2SlangGen {
  @datatype trait Type {
    def descOpt: Option[String]
  }

  object Type {
    @datatype class Bool(val descOpt: Option[String]) extends Type
    @datatype class Enum(val elements: ISZ[String], val elementDescs: ISZ[String], val descOpt: Option[String], val pos: message.Position) extends Type
    @datatype class Integer(val descOpt: Option[String]) extends Type
    @datatype class Null(val descOpt: Option[String]) extends Type
    @datatype class Any(val jsonTypes: ISZ[String], val descOpt: Option[String]) extends Type
    @datatype class Str(val descOpt: Option[String]) extends Type
    @datatype class Object(val fields: ISZ[Field], val descOpt: Option[String], pos: message.Position) extends Type
    @datatype class Ref(val ref: String, val descOpt: Option[String]) extends Type
    @datatype class Array(val elementType: Type, val descOpt: Option[String]) extends Type
    @datatype class Number(val descOpt: Option[String]) extends Type
  }

  @datatype class Field(val isOpt: B, val id: String, val tipe: Type)

  @datatype class Definition(val id: String,
                             val titleOpt: Option[String],
                             val supers: ISZ[String],
                             val descOpt: Option[String],
                             val fields: ISZ[Field])

  val kind: String = "JsonSchema2SlangGen"
  val schemaId: String = "$schema"
  val schemaDraft4: String = "http://json-schema.org/draft-04/schema#"
  val titleId: String = "title"
  val descId: String = "description"
  val definitionsId: String = "definitions"
  val propertiesId: String = "properties"
  val requiredId: String = "required"
  val allOfId: String = "allOf"
  val refId: String = "$ref"
  val typeId: String = "type"
  val objectId: String = "object"
  val arrayId: String = "array"
  val itemsId: String = "items"
  val stringId: String = "string"
  val booleanId: String = "bool"
  val intId: String = "integer"
  val numId: String = "number"
  val enumId: String = "enum"
  val _enumId: String = "_enum"
  val nullId: String = "null"
  val enumDescriptionsId: String = "enumDescriptions"

  def gen(uriOpt: Option[String],
          licenseOpt: Option[String],
          packageName: QName,
          objectName: String,
          tree: AST.Obj,
          reporter: Reporter): Option[ST] = {
    val map = tree.asMap
    assert(map.getStr(schemaId).value == schemaDraft4 && map.getStr(titleId).value == objectId)

    var defnMap = HashSMap.empty[String, Definition]
    var refs = HashSMap.empty[String, HashSSet[message.Position]]

    def isDefnTrait(id: String): B = {
      return refs.contains(id)
    }

    @strictpure def defnId(id: String): String = s"#/definitions/$id"

    def typeOf(m: AST.Map, pos: message.Position): Type = {
      val descOpt = m.getStrValueOpt(descId)
      m.getOpt(typeId) match {
        case Some(v: AST.Str) =>
          val t: Type = v.value match {
            case `booleanId` => Type.Bool(descOpt)
            case `intId` => Type.Integer(descOpt)
            case `numId` => Type.Number(descOpt)
            case `stringId` if m.value.contains(enumId) || m.value.contains(_enumId) =>
              val elementDescriptions: ISZ[String] = m.value.get(enumDescriptionsId) match {
                case Some(arr: AST.Arr) => for (v <- arr.values) yield v.asInstanceOf[AST.Str].value
                case _ => ISZ()
              }
              val elements: ISZ[String] = for (v <- if (m.value.contains(enumId)) m.getArr(enumId).values else
                m.getArr(_enumId).values) yield v.asInstanceOf[AST.Str].value
              Type.Enum(elements, elementDescriptions, descOpt, pos)
            case `stringId` => Type.Str(descOpt)
            case `objectId` => Type.Object(objectField(m), descOpt, pos)
            case `arrayId` =>
              val arr = m.getObj(itemsId)
              Type.Array(typeOf(arr.asMap, arr.pos), descOpt)
            case `nullId` => Type.Null(descOpt)
            case _ => halt(s"TODO: ${v.value}")
          }
          return t
        case Some(v: AST.Arr) =>
          return Type.Any(for (v <- m.getArr(typeId).values) yield v.asInstanceOf[String], descOpt)
        case _ =>
          val ref = m.getStr(refId)
          refs = refs + ref.value ~> (refs.get(ref.value).getOrElse(HashSSet.empty) + ref.pos)
          return Type.Ref(ref.value, descOpt)
      }
    }

    def propertyField(isRequired: B, id: String, o: AST.Obj): Field = {
      return Field(!isRequired, id, typeOf(o.asMap, o.pos))
    }

    def objectField(objectMap: AST.Map): ISZ[Field] = {
      var r = ISZ[Field]()
      objectMap.getOpt(propertiesId) match {
        case Some(obj: AST.Obj) =>
          var required = HashSet.empty[String]
          objectMap.getOpt(requiredId) match {
            case Some(arr: AST.Arr) =>
              for (e <- arr.values) {
                required = required + e.asInstanceOf[AST.Str].value
              }
            case _ =>
          }
          for (kv <- obj.keyValues) {
            r = r :+ propertyField(required.contains(kv.id.value),
              kv.id.value, kv.value.asInstanceOf[AST.Obj])
          }
        case Some(_) => halt(s"Infeasible: ${objectMap.value}")
        case _ =>
      }
      return r
    }

    def processDef(id: String, defn: AST.Map): Unit = {
      defn.getOpt(typeId) match {
        case Some(v: AST.Str) if v.value == objectId =>
          defnMap = defnMap + defnId(id) ~> Definition(
            id = id,
            titleOpt = defn.getStrValueOpt(titleId),
            supers = ISZ(),
            descOpt = defn.getStrValueOpt(descId),
            fields = objectField(defn)
          )
          return
        case _ =>
      }
      defn.getOpt(allOfId) match {
        case Some(arr: AST.Arr) =>
          val vs = arr.values
          var supers = ISZ[String]()
          var i: Z = 0
          var allOfMap = vs(i).asInstanceOf[AST.Obj].asMap
          while (i < vs.size && map.value.contains(refId)) {
            supers = supers :+ allOfMap.getStr(refId).value
            i = i + 1
            allOfMap = vs(i).asInstanceOf[AST.Obj].asMap
          }
          defnMap = defnMap + defnId(id) ~> Definition(
            id = id,
            titleOpt = None(),
            supers = supers,
            descOpt = allOfMap.getStrValueOpt(descId),
            fields = objectField(allOfMap)
          )
        case _ =>
      }
      halt(s"TODO: definition ${defn.value}")
    }

    for (defn <- map.getObj(definitionsId).keyValues) {
      for (kv <- defn.asInstanceOf[AST.Obj].keyValues) {
        processDef(kv.id.value, kv.value.asInstanceOf[AST.Obj].asMap)
      }
    }

    var sts = ISZ[ST]()
    var enumMap = HashSMap.empty[Type.Enum, ST]
    var objectMap = HashSMap.empty[Type.Object, ST]

    @strictpure def mlCommentSTOpt(textOpt: Option[String]): Option[ST] = textOpt match {
      case Some(text) => Some(st" /* $text */")
      case _ => None()
    }


    @strictpure def lCommentSTOpt(textOpt: Option[String]): Option[ST] = textOpt match {
      case Some(text) => Some(st"// $text")
      case _ => None()
    }

    @strictpure def pathIdST(path: ISZ[String]): ST =
      st"${(for (p <- path) yield ops.StringOps(p).firstToUpper, "")}"

    def genTypeH(isOpt: B, t: Type, path: ISZ[String]): (ST, ISZ[String]) = {
      var descs: ISZ[String] = t.descOpt match {
        case Some(desc) => ISZ(desc)
        case _ => ISZ()
      }
      var r: ST = t match {
        case _: Type.Bool => st"B"
        case _: Type.Integer => st"Z"
        case _: Type.Number => st"F64"
        case _: Type.Str => st"String"
        case _: Type.Null => st"None[parser.json.AST]"
        case t: Type.Ref => st"${defnMap.get(t.ref).get.id}"
        case t: Type.Array =>
          val (aST, ds) = genTypeH(F, t.elementType, path :+ "Array")
          descs = descs ++ ds
          aST
        case t: Type.Enum =>
          val id = pathIdST(path)
          if (!enumMap.contains(t)) {
            var elements = ISZ[ST]()
            for (i <- t.elements.indices) {
              var eST = st"${ops.StringOps(t.elements(i)).firstToUpper}"
              if (t.elementDescs.size > i) {
                eST = st"$eST // ${t.elementDescs(i)}"
              }
              elements = elements :+ eST
            }
            sts = sts :+
              st"""@enum object $id {
                  |  ${(elements, "\n")}
                  |}"""
            enumMap = enumMap + t ~> id
          }
          id
        case t: Type.Object =>
          val id = pathIdST(path)
          if (!objectMap.contains(t)) {
            sts = sts :+
              st"""@datatype class $id(${(for (f <- t.fields) yield st"val ${f.id}: ${genType(f.isOpt, f.tipe, ISZ(f.id))}", ",\n")})"""
            objectMap = objectMap + t ~> id
          }
          id
        case t: Type.Any =>
          descs = descs :+ st"Has to be any of { ${(t.jsonTypes, ", ")} }".render
          st"parser.json.AST"
      }
      if (isOpt) {
        r = st"Option[$r]"
      }
      return (r, descs)
    }

    def genType(isOpt: B, t: Type, path: ISZ[String]): ST = {
      val (tST, tDescs) = genTypeH(isOpt, t, path)
      return if (tDescs.isEmpty) tST else if (tDescs.size == 1) st"$tST /* ${tDescs(0)} */" else
        st"""$tST /*
            |     ${(tDescs, "\n")}
            |     */"""
    }

    @strictpure def extendsSTOpt(supers: ISZ[String]): Option[ST] = if (supers.isEmpty) None() else Some(
      st"extends ${(for (s <- supers) yield defnMap.get(s).get.id, ", ")}"
    )

    for (p <- refs.entries if !defnMap.contains(p._1); pos <- p._2.elements) {
      reporter.error(Some(pos), kind, "Could not find $ref: ${p._1}")
    }

    if (reporter.hasError) {
      return None()
    }

    for (p <- defnMap.entries) {
      val (dId, defn) = p
      val st: ST = if (isDefnTrait(dId)) {
        st"""${lCommentSTOpt(defn.titleOpt)}
            |${lCommentSTOpt(defn.descOpt)}
            |@datatype trait ${defn.id}${extendsSTOpt(defn.supers)} {
            |  ${(for (f <- defn.fields) yield st"def ${f.id}: ${genType(f.isOpt, f.tipe, ISZ(f.id))}", "\n")}
            |}"""
      } else {
        var fields = ISZ[Field]()
        def rec(supers: ISZ[String]): Unit = {
          for (s <- supers) {
            val sDefn = defnMap.get(s).get
            rec(sDefn.supers)
            fields = fields ++ sDefn.fields
          }
        }
        rec(defn.supers)
        fields = fields ++ defn.fields
        st"""${lCommentSTOpt(defn.titleOpt)}
            |${lCommentSTOpt(defn.descOpt)}
            |@datatype class ${defn.id}(${(for (f <- fields) yield st"val ${f.id}: ${genType(f.isOpt, f.tipe, ISZ(f.id))}", ",\n")})${extendsSTOpt(defn.supers)}"""
      }
      sts = sts :+ st
    }

    val pOpt: Option[ST] = if (packageName.isEmpty) None() else Some(st"package ${(packageName, ".")}")
    val titleOpt: Option[ST] = map.getStrValueOpt(titleId) match {
      case Some(title) => Some(st"// $title")
      case _ => None()
    }
    val descOpt: Option[ST] = map.getStrValueOpt(descId) match {
      case Some(desc) => Some(st"// $desc")
      case _ => None()
    }

    val sourceOpt: Option[ST] = uriOpt match {
      case Some(uri) =>
        val i = ops.StringOps(uri).lastIndexOf('/')
        Some(st"// Auto-generated from ${if (i >= 0) ops.StringOps(uri).substring(i + 1, uri.size) else uri}")
      case _ => None()
    }

    return Some(
      st"""// #Sireum
          |$licenseOpt
          |$pOpt
          |
          |$sourceOpt
          |
          |$titleOpt
          |$descOpt
          |object $objectName {
          |
          |  ${(sts, "\n\n")}
          |
          |}"""
    )
  }
}