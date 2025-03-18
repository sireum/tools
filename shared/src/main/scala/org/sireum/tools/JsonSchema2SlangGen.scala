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
    @datatype class OneOf(val types: ISZ[Type], val descOpt: Option[String]) extends Type
    @datatype class Raw(val descOpt: Option[String]) extends Type
  }

  @datatype class Field(val defnIdOpt: Option[String], val opt: Field.Opt.Type, val id: String, val tipe: Type)

  object Field {
    @enum object Opt {
      "Required"
      "Optional"
      "Some"
    }
  }

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
  val oneOfId: String = "oneOf"
  val refId: String = "$ref"
  val typeId: String = "type"
  val objectId: String = "object"
  val arrayId: String = "array"
  val itemsId: String = "items"
  val stringId: String = "string"
  val booleanId: String = "boolean"
  val intId: String = "integer"
  val numId: String = "number"
  val enumId: String = "enum"
  val _enumId: String = "_enum"
  val nullId: String = "null"
  val enumDescriptionsId: String = "enumDescriptions"
  val topTypeId: String = "`.Node`"
  val rawTypeId: String = "Raw"
  val nullTypeId: String = "Null"

  def gen(uriOpt: Option[String],
          licenseOpt: Option[String],
          packageName: QName,
          objectName: String,
          tree: AST.Obj,
          reporter: Reporter): Option[ST] = {
    val map = tree.asMap
    assert(map.getStr(schemaId).value == schemaDraft4, s"${map.value}")
    assert(map.getStr(typeId).value == objectId, map.getStr(typeId).value)

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
            case `objectId` =>
              m.getOpt(propertiesId) match {
                case Some(_) => Type.Object(objectField(None(), m), descOpt, pos)
                case _ => Type.Raw(descOpt)
              }
            case `arrayId` =>
              val arr = m.getObj(itemsId)
              Type.Array(typeOf(arr.asMap, arr.pos), descOpt)
            case `nullId` => Type.Null(descOpt)
            case _ => halt(s"TODO: ${v.value}")
          }
          return t
        case Some(v: AST.Arr) =>
          return Type.Any(for (s <- v.values) yield s.asInstanceOf[AST.Str].value, descOpt)
        case _ =>
          m.getOpt(oneOfId) match {
            case Some(v: AST.Arr) =>
              var ts = ISZ[Type]()
              for (e <- v.values) {
                val eObj = e.asInstanceOf[AST.Obj]
                ts = ts :+ typeOf(eObj.asMap, eObj.pos)
              }
              return Type.OneOf(ts, descOpt)
            case _ =>
          }
          assert(m.value.contains(refId), s"${m.value}")
          val ref = m.getStr(refId)
          return Type.Ref(ref.value, descOpt)
      }
    }

    @strictpure def requiredOpt(isRequired: B): Field.Opt.Type =
      if (isRequired) Field.Opt.Required else Field.Opt.Optional

    def propertyField(defnIdOpt: Option[String], isRequired: B, id: String, o: AST.Obj): Field = {
      return Field(defnIdOpt, requiredOpt(isRequired), id, typeOf(o.asMap, o.pos))
    }

    def objectField(defnIdOpt: Option[String], objectMap: AST.Map): ISZ[Field] = {
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
            r = r :+ propertyField(defnIdOpt, required.contains(kv.id.value),
              kv.id.value, kv.value.asInstanceOf[AST.Obj])
          }
        case Some(_) => halt(s"Infeasible: ${objectMap.value}")
        case _ =>
      }
      return r
    }

    def processDef(id: String, defn: AST.Map, pos: message.Position): Unit = {
      defn.getOpt(typeId) match {
        case Some(v: AST.Str) =>
          v.value match {
            case `objectId` =>
              defnMap = defnMap + defnId(id) ~> Definition(
                id = id,
                titleOpt = defn.getStrValueOpt(titleId),
                supers = ISZ(),
                descOpt = defn.getStrValueOpt(descId),
                fields = objectField(Some(id), defn)
              )
              return
            case _ =>
              defnMap = defnMap + defnId(id) ~> Definition(
                id = id,
                titleOpt = defn.getStrValueOpt(titleId),
                supers = ISZ(),
                descOpt = defn.getStrValueOpt(descId),
                fields = ISZ(Field(Some(id), requiredOpt(F), "value", typeOf(defn, pos)))
              )
              return
          }
        case _ =>
      }
      defn.getOpt(allOfId) match {
        case Some(arr: AST.Arr) =>
          val vs = arr.values
          var supers = ISZ[String]()
          var i: Z = 0
          var allOfMap = vs(i).asInstanceOf[AST.Obj].asMap
          while (i < vs.size && allOfMap.value.contains(refId)) {
            val ref = allOfMap.getStr(refId)
            refs = refs + ref.value ~> (refs.get(ref.value).getOrElse(HashSSet.empty) + ref.pos)
            supers = supers :+ ref.value
            i = i + 1
            allOfMap = vs(i).asInstanceOf[AST.Obj].asMap
          }
          defnMap = defnMap + defnId(id) ~> Definition(
            id = id,
            titleOpt = None(),
            supers = supers,
            descOpt = allOfMap.getStrValueOpt(descId),
            fields = objectField(Some(id), allOfMap)
          )
          return
        case _ =>
      }
      halt(s"TODO: definition ${defn.value}")
    }

    for (kv <- map.getObj(definitionsId).keyValues) {
      val v = kv.value.asInstanceOf[AST.Obj]
      processDef(kv.id.value, v.asMap, v.pos)
    }

    var sts = ISZ[ST]()
    var objectMap = HashSMap.empty[Type.Object, ST]

    @pure def mlCommentSTOpt(textOpt: Option[String]): Option[ST] = {
      textOpt match {
        case Some(text) =>
          val ts = ops.StringOps(text).split((c: C) => c == '\n')
          return Some(
            if (ts.size <= 1) st"/* $text */"
            else
              st"""/*
                  |  ${(ts, "\n")}
                  | */"""
          )
        case _ => return None()
      }
    }

    @strictpure def pathIdST(path: ISZ[String]): ST =
      st"${(for (p <- path) yield ops.StringOps(p).firstToUpper, "")}"

    @strictpure def esc(id: String): ST = if (ops.StringOps.scalaKeywords.contains(id)) st"`$id`" else st"$id"

    def genTypeH(opt: Field.Opt.Type, t: Type, path: ISZ[String]): (ST, ISZ[String]) = {
      var descs: ISZ[String] = t.descOpt match {
        case Some(desc) => ISZ(desc)
        case _ => ISZ()
      }
      val r: ST = t match {
        case _: Type.Bool => st"B"
        case _: Type.Integer => st"Z"
        case _: Type.Number => st"F64"
        case _: Type.Str => st"String"
        case _: Type.Null => st"None[$topTypeId]"
        case t: Type.Ref => st"${defnMap.get(t.ref).get.id}"
        case t: Type.Array =>
          val (eST, ds) = genTypeH(requiredOpt(F), t.elementType, path :+ "Array")
          descs = descs ++ ds
          st"ISZ[$eST]"
        case t: Type.Enum =>
          @strictpure def element(i: Z): ST = if (t.elementDescs.isEmpty) st"${t.elements(i)}" else
            st"${t.elements(i)} ${mlCommentSTOpt(Some(t.elementDescs(i)))}"
          descs = descs :+
            st"""Has to be one of {
                |  ${(for (i <- t.elements.indices) yield element(i), ",\n")}
                |}""".render
          st"String"
        case t: Type.Raw =>
          t.descOpt match {
            case Some(desc) => descs = descs :+ desc
            case _ =>
          }
          st"$topTypeId.$rawTypeId"
        case t: Type.Object =>
          val id = pathIdST(path)
          if (!objectMap.contains(t)) {
            sts = sts :+
              st"""@datatype class $id(
                  |  ${(for (f <- t.fields) yield st"val ${esc(f.id)}: ${genType(f.opt, f.tipe, path :+ f.id)}", ",\n")}
                  |) extends $topTypeId"""
            objectMap = objectMap + t ~> id
          }
          id
        case t: Type.OneOf =>
          var (r, descs) = genTypeH(requiredOpt(F), t.types(0), path :+ "OneOf0")
          for (i <- 1 until t.types.size) {
            val (rt, ds) = genTypeH(requiredOpt(F), t.types(i), path :+ "OneOf$i")
            r = st"Either[$r, $rt]"
            descs = descs ++ ds
          }
          r
        case t: Type.Any =>
          descs = descs :+ st"Has to be any of { ${(t.jsonTypes, ", ")} }".render
          st"$topTypeId"
      }
      return (r, descs)
    }

    def genType(opt: Field.Opt.Type, t: Type, path: ISZ[String]): ST = {
      val (tST, tDescs) = genTypeH(opt, t, path)
      var ds = ISZ[String]()
      for (desc <- tDescs; d <- ops.StringOps(desc).split((c: C) => c == '\n')) {
        ds = ds :+ d
      }
      return if (ds.isEmpty) tST
      else if (ds.size == 1)
        st"$tST /* ${ds(0)} */"
      else
        st"""$tST
            |  /*
            |    ${(ds, "\n")}
            |   */"""
    }

    @strictpure def extendsSTOpt(supers: ISZ[String]): Option[ST] = if (supers.isEmpty) None() else Some(
      st" extends ${(for (s <- supers) yield if (defnMap.contains(s)) defnMap.get(s).get.id else s, " with ")}"
    )

    for (p <- refs.entries if !defnMap.contains(p._1); pos <- p._2.elements) {
      reporter.error(Some(pos), kind, s"Could not find $$ref: ${p._1}")
    }

    if (reporter.hasError) {
      return None()
    }

    for (p <- defnMap.entries) {
      val (dId, defn) = p
      @strictpure def fPath(f: Field): ISZ[String] = f.defnIdOpt match {
        case Some(defn) => ISZ(defn, f.id)
        case _ => ISZ(f.id)
      }
      var fields = HashSMap.empty[String, Field]
      def addField(f: Field): Unit = {
        fields = fields + f.id ~> f
      }
      def rec(supers: ISZ[String]): Unit = {
        for (s <- supers) {
          val sDefn = defnMap.get(s).get
          rec(sDefn.supers)
          for (f <- sDefn.fields) {
            addField(f)
          }
        }
      }
      rec(defn.supers)
      for (f <- defn.fields) {
        addField(f)
      }
      val st: ST = if (isDefnTrait(dId)) {
        fields = fields -- (fields.keys -- (for (f <- defn.fields) yield f.id))
        st"""${mlCommentSTOpt(defn.titleOpt)}
            |${mlCommentSTOpt(defn.descOpt)}
            |@datatype trait ${esc(defn.id)}${extendsSTOpt(if (defn.supers.isEmpty) ISZ(topTypeId) else defn.supers)} {
            |
            |  ${(for (f <- fields.values) yield st"def ${esc(f.id)}: ${genType(f.opt, f.tipe, fPath(f))}", "\n\n")}
            |
            |}"""
      } else {
        fields = fields ++ (for (f <- defn.fields) yield (f.id, f))
        st"""${mlCommentSTOpt(defn.titleOpt)}
            |${mlCommentSTOpt(defn.descOpt)}
            |@datatype class ${esc(defn.id)}(
            |  ${(for (f <- fields.values) yield st"val ${esc(f.id)}: ${genType(f.opt, f.tipe, fPath(f))}", ",\n")}
            |)${extendsSTOpt(if (defn.supers.isEmpty) ISZ(topTypeId) else defn.supers)}"""
      }
      sts = sts :+ st
    }

    val pOpt: Option[ST] = if (packageName.isEmpty) None() else Some(st"package ${(packageName, ".")}")

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
          |import org.sireum._
          |
          |$sourceOpt
          |
          |${mlCommentSTOpt(map.getStrValueOpt(titleId))}
          |${mlCommentSTOpt(map.getStrValueOpt(descId))}
          |object $objectName {
          |
          |  @datatype trait $topTypeId
          |  object $topTypeId {
          |    @datatype class $rawTypeId(ast: parser.json.AST) extends $topTypeId
          |    @datatype class $nullTypeId extends $topTypeId
          |  }
          |
          |  ${(sts, "\n\n")}
          |
          |}"""
    )
  }
}