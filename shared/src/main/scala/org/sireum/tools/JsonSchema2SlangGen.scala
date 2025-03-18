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
    @datatype class Enum(val elements: ISZ[String], val elementDescs: ISZ[String], val descOpt: Option[String], val posOpt: Option[message.Position]) extends Type
    @datatype class Integer(val descOpt: Option[String]) extends Type
    @datatype class Null(val descOpt: Option[String]) extends Type
    @datatype class Any(val jsonTypes: ISZ[String], val descOpt: Option[String]) extends Type
    @datatype class Str(val descOpt: Option[String]) extends Type
    @datatype class Object(val fields: ISZ[Field], val descOpt: Option[String], val posOpt: Option[message.Position]) extends Type
    @datatype class Ref(val ref: String, val descOpt: Option[String]) extends Type
    @datatype class Array(val elementType: Type, val descOpt: Option[String]) extends Type
    @datatype class Number(val descOpt: Option[String]) extends Type
    @datatype class OneOf(val types: ISZ[Type], val descOpt: Option[String]) extends Type
    @datatype class Raw(val descOpt: Option[String]) extends Type
  }

  @datatype class Field(val defnIdOpt: Option[String], val opt: Field.Opt.Type, val id: String, val tipe: Type) {
    @strictpure def isRequired: B = opt != Field.Opt.Optional
  }

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
                             val fields: ISZ[Field],
                             val requiredFields: ISZ[String])

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
  val arrayPathId: String = "Array"

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
    var refs = HashSMap.empty[String, HashSSet[Option[message.Position]]]
    var toArrayElementSet = HashSSet.empty[String]
    var fromArrayElementSet = HashSSet.empty[String]

    def isDefnTrait(id: String): B = {
      return refs.contains(id)
    }

    @strictpure def defnId(id: String): String = s"#/definitions/$id"

    def typeOf(m: AST.Map, pos: Option[message.Position]): Type = {
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
                case Some(_) => Type.Object(objectField(None(), m, requiredSet(m)), descOpt, pos)
                case _ => Type.Raw(descOpt)
              }
            case `arrayId` =>
              val arr = m.getObj(itemsId)
              Type.Array(typeOf(arr.asMap, arr.posOpt), descOpt)
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
                ts = ts :+ typeOf(eObj.asMap, eObj.posOpt)
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
      return Field(defnIdOpt, requiredOpt(isRequired), id, typeOf(o.asMap, o.posOpt))
    }

    def objectField(defnIdOpt: Option[String], objectMap: AST.Map, required: HashSSet[String]): ISZ[Field] = {
      var r = ISZ[Field]()
      objectMap.getOpt(propertiesId) match {
        case Some(obj: AST.Obj) =>
          for (kv <- obj.keyValues) {
            r = r :+ propertyField(defnIdOpt, required.contains(kv.id.value),
              kv.id.value, kv.value.asInstanceOf[AST.Obj])
          }
        case Some(_) => halt(s"Infeasible: ${objectMap.value}")
        case _ =>
      }
      return r
    }

    def requiredSet(m: AST.Map): HashSSet[String] = {
      var required = HashSSet.empty[String]
      m.getOpt(requiredId) match {
        case Some(arr: AST.Arr) =>
          for (e <- arr.values) {
            required = required + e.asInstanceOf[AST.Str].value
          }
        case _ =>
      }
      return required
    }

    def processDef(id: String, defn: AST.Map, posOpt: Option[message.Position]): Unit = {
      val required = requiredSet(defn)
      defn.getOpt(typeId) match {
        case Some(v: AST.Str) =>
          v.value match {
            case `objectId` =>
              defnMap = defnMap + defnId(id) ~> Definition(
                id = id,
                titleOpt = defn.getStrValueOpt(titleId),
                supers = ISZ(),
                descOpt = defn.getStrValueOpt(descId),
                fields = objectField(Some(id), defn, required),
                requiredFields = required.elements
              )
              return
            case _ =>
              defnMap = defnMap + defnId(id) ~> Definition(
                id = id,
                titleOpt = defn.getStrValueOpt(titleId),
                supers = ISZ(),
                descOpt = defn.getStrValueOpt(descId),
                fields = ISZ(Field(Some(id), requiredOpt(F), "value", typeOf(defn, posOpt))),
                requiredFields = required.elements
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
            refs = refs + ref.value ~> (refs.get(ref.value).getOrElse(HashSSet.empty) + ref.posOpt)
            supers = supers :+ ref.value
            i = i + 1
            allOfMap = vs(i).asInstanceOf[AST.Obj].asMap
          }
          val allOfRequired = requiredSet(allOfMap)
          val fields = objectField(Some(id), allOfMap, allOfRequired)
          defnMap = defnMap + defnId(id) ~> Definition(
            id = id,
            titleOpt = None(),
            supers = supers,
            descOpt = allOfMap.getStrValueOpt(descId),
            fields = fields,
            requiredFields = allOfRequired.elements)
          return
        case _ =>
      }
      halt(s"TODO: definition ${defn.value}")
    }

    for (kv <- map.getObj(definitionsId).keyValues) {
      val v = kv.value.asInstanceOf[AST.Obj]
      processDef(kv.id.value, v.asMap, v.posOpt)
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

    @strictpure def escField(isVal: B, f: Field): ST = {
      val fId: String = if (isVal && !f.isRequired) s"${f.id}Opt" else f.id
      if (ops.StringOps.scalaKeywords.contains(fId)) st"`$fId`" else st"$fId"
    }
    @strictpure def escId(id: String): ST = if (ops.StringOps.scalaKeywords.contains(id)) st"`$id`" else st"$id"

    @pure def genToArray(t: Type.Array, etST: ST): ST = {
      val r = st"toISZ$etST"
      val etString = etST.render
      if (toArrayElementSet.contains(etString)) {
        return r
      }
      toArrayElementSet = toArrayElementSet + etString

      val vST: ST = t.elementType match {
        case _: Type.Bool => st"v.asInstanceOf[AST.Bool].value"
        case _: Type.Integer => st"v.asInstanceOf[AST.Int].value"
        case _: Type.Number => st"v.asInstanceOf[AST.Dbl].value"
        case _: Type.Str => st"v.asInstanceOf[AST.Str].value"
        case _: Type.Enum => st"v.asInstanceOf[AST.Str].value"
        case _: Type.Raw => st"$topTypeId.$rawTypeId(v)"
        case _: Type.OneOf => st"$topTypeId.$rawTypeId(v)"
        case _: Type.Any => st"$topTypeId.$rawTypeId(v)"
        case _: Type.Null => st"$topTypeId.$nullTypeId()"
        case _: Type.Ref => st"to$etST(v.asInstanceOf[AST.Obj])"
        case et: Type.Array => genToArray(et, st"ISZ$etST")
        case _: Type.Object => st"to$etST(v.asInstanceOf[AST.Obj])"
      }

      sts = sts :+
        st"""@pure def $r(ast: AST.Arr): ISZ[$etST] = {
            |  var r = ISZ[$etST]()
            |  for (v <- ast.values) {
            |    r = r :+ $vST
            |  }
            |  return r
            |}"""

      return r
    }

    @pure def genFromArray(t: Type.Array, etST: ST): ST = {
      val r = st"fromISZ$etST"
      val etString = etST.render
      if (fromArrayElementSet.contains(etString)) {
        return r
      }
      fromArrayElementSet = fromArrayElementSet + etString

      val vST: ST = t.elementType match {
        case _: Type.Bool => st"AST.Bool(v, None())"
        case _: Type.Integer => st"AST.Int(v, None())"
        case _: Type.Number => st"AST.Dbl(v, None())"
        case _: Type.Str => st"AST.Str(v, None())"
        case _: Type.Enum => st"AST.Str(v, None())"
        case _: Type.Raw => st"v.value"
        case _: Type.OneOf => st"v.value"
        case _: Type.Any => st"v.toAST"
        case _: Type.Null => st"AST.Null(None())"
        case _: Type.Ref => st"v.toAST"
        case _: Type.Object => st"v.toAST"
        case et: Type.Array => genFromArray(et, st"ISZ$etST")
      }

      sts = sts :+
        st"""@pure def $r(seq: ISZ[$etST]): AST.Arr = {
            |  var elements = ISZ[AST]()
            |  for (v <- seq) {
            |    elements = elements :+ $vST
            |  }
            |  return AST.Arr(elements, None())
            |}"""

      return r
    }

    @pure def fromBindingST(fields: ISZ[Field], path: ISZ[String]): ST = {
      var fieldStmts = ISZ[ST]()
      for (f <- fields) {
        @strictpure def fieldStmt(value: ST): ST =
          st"kvs = kvs :+ AST.KeyValue(AST.Str(\"${ops.StringOps(escField(F, f).render).escapeST}\", None()), $value)"
        val fId = escField(T, f)
        val fValue: ST = if (f.isRequired) fId else escField(F, f)
        val valueST: ST = f.tipe match {
          case _: Type.Bool => st"AST.Bool($fValue, None())"
          case _: Type.Integer => st"AST.Int($fValue, None())"
          case _: Type.Number => st"AST.Dbl($fValue, None())"
          case _: Type.Str => st"AST.Str($fValue, None())"
          case _: Type.Ref => st"$fValue.toAST"
          case _: Type.Null => st"AST.Null(None())"
          case _: Type.OneOf => st"$fValue.value"
          case _: Type.Any => st"$fValue.toAST"
          case _: Type.Raw => st"$fValue.value"
          case ft: Type.Enum =>
            fieldStmts = fieldStmts :+
              st"assert(${(for (e <- ft.elements) yield st"$fValue == \"${ops.StringOps(e).escapeST}\"", " || ")})"
            st"AST.Str($fValue, None())"
          case _: Type.Object => st"$fValue.toAST"
          case ft: Type.Array =>
            val p: ISZ[String] = f.defnIdOpt match {
              case Some(defn) => ISZ(defn, f.id)
              case _ => path :+ f.id
            }
            val etST = genTypeH(T, T, ft.elementType, p :+ arrayPathId)._1
            val fromArray = genFromArray(ft, etST)
            st"$fromArray($fValue)"
        }
        if (f.isRequired) {
          fieldStmts = fieldStmts :+ fieldStmt(valueST)
        } else {
          fieldStmts = fieldStmts :+
            st"""if ($fId.nonEmpty) {
                |  ${fieldStmt(valueST)}
                |}"""
        }
      }
      val r =
        st"""@pure def toAST: AST.Obj = {
            |  var kvs = ISZ[AST.KeyValue]()
            |  ${(fieldStmts, "\n")}
            |  return AST.Obj(kvs, None())
            |}"""
      return r
    }

    @pure def toBindingST(id: ST, fields: ISZ[Field], path: ISZ[String]): ST = {
      var fieldIds = ISZ[ST]()
      var fieldStmts = ISZ[ST]()
      for (f <- fields) {
        val fId = escField(T, f)
        fieldIds = fieldIds :+ fId
        val fKey = st"${ops.StringOps(f.id).escapeST}"
        f.tipe match {
          case _: Type.Bool =>
            if (f.isRequired) {
              fieldStmts = fieldStmts :+ st"""val $fId = map.getBool("$fKey").value"""
            } else {
              fieldStmts = fieldStmts :+ st"""val $fId = map.getBoolValueOpt("$fKey")"""
            }
          case _: Type.Integer =>
            if (f.isRequired) {
              fieldStmts = fieldStmts :+ st"""val $fId = map.getInt("$fKey").value"""
            } else {
              fieldStmts = fieldStmts :+ st"""val $fId = map.getIntValueOpt("$fKey")"""
            }
          case _: Type.Number =>
            if (f.isRequired) {
              fieldStmts = fieldStmts :+ st"""val $fId = map.getDbl("$fKey").value"""
            } else {
              fieldStmts = fieldStmts :+ st"""val $fId = map.getDblValueOpt("$fKey")"""
            }
          case _: Type.Str =>
            if (f.isRequired) {
              fieldStmts = fieldStmts :+ st"""val $fId = map.getStr("$fKey").value"""
            } else {
              fieldStmts = fieldStmts :+ st"""val $fId = map.getStrValueOpt("$fKey")"""
            }
          case _: Type.Raw =>
            if (f.isRequired) {
              fieldStmts = fieldStmts :+ st"""val $fId = $topTypeId.$rawTypeId(map.get("$fKey"))"""
            } else {
              fieldStmts = fieldStmts :+ st"""val $fId = map.getOpt("$fKey").map((o: AST) => $topTypeId.$rawTypeId(o))"""
            }
          case _: Type.Any =>
            if (f.isRequired) {
              fieldStmts = fieldStmts :+ st"""val $fId = $topTypeId.$rawTypeId(map.get("$fKey"))"""
            } else {
              fieldStmts = fieldStmts :+ st"""val $fId = map.getOpt("$fKey").map((o: AST) => $topTypeId.$rawTypeId(o).asInstanceOf[$topTypeId])"""
            }
          case _: Type.Null =>
            if (f.isRequired) {
              fieldStmts = fieldStmts :+ st"""val $fId = map.getNull("$fKey")"""
            } else {
              fieldStmts = fieldStmts :+ st"""val $fId = map.getNullOpt("$fKey").map((_: AST.Null) => $topTypeId.$nullTypeId())"""
            }
          case _: Type.OneOf =>
            if (f.isRequired) {
              fieldStmts = fieldStmts :+ st"""val $fId = $topTypeId.$rawTypeId(map.get("$fKey"))"""
            } else {
              fieldStmts = fieldStmts :+ st"""val $fId = map.getOpt("$fKey").map((o: AST) => $topTypeId.$rawTypeId(o))"""
            }
          case ft: Type.Enum =>
            @strictpure def assertST(fIdST: ST): ST =
              st"""assert(${(for (e <- ft.elements) yield st"""$fIdST == "${ops.StringOps(e).escapeST}"""", " || ")})"""
            if (f.isRequired) {
              fieldStmts = fieldStmts :+ st"""val $fId = map.getStr("$fKey").value"""
              fieldStmts = fieldStmts :+ assertST(fId)
            } else {
              fieldStmts = fieldStmts :+ st"""val $fId = map.getStrValueOpt("$fKey")"""
              fieldStmts = fieldStmts :+
                st"""$fId match {
                    |  case Some(s) => ${assertST(st"s")}
                    |  case _ =>
                    |}"""
            }
          case ft: Type.Ref =>
            if (f.isRequired) {
              fieldStmts = fieldStmts :+ st"""val $fId = to${defnMap.get(ft.ref).get.id}(map.getObj("$fKey"))"""
            } else {
              fieldStmts = fieldStmts :+ st"""val $fId = map.getObjOpt("$fKey").map((o: AST.Obj) => to${defnMap.get(ft.ref).get.id}(o))"""
            }
          case ft: Type.Array =>
            val p: ISZ[String] = f.defnIdOpt match {
              case Some(defn) => ISZ(defn, f.id)
              case _ => path :+ f.id
            }
            val etST = genTypeH(T, T, ft.elementType, p :+ arrayPathId)._1
            val toArray = genToArray(ft, etST)
            if (f.isRequired) {
              fieldStmts = fieldStmts :+ st"""val $fId = $toArray(map.getArr("$fKey"))"""
            } else {
              fieldStmts = fieldStmts :+ st"""val $fId = map.getArrOpt("$fKey").map((o: AST.Arr) => $toArray(o))"""
            }
          case ft: Type.Object =>
            val p: ISZ[String] = f.defnIdOpt match {
              case Some(defn) => ISZ(defn, f.id)
              case _ => path :+ f.id
            }
            if (f.isRequired) {
              fieldStmts = fieldStmts :+ st"""val $fId = to${genTypeH(T, f.isRequired, ft, p)._1}(map.getObj("$fKey"))"""
            } else {
              fieldStmts = fieldStmts :+ st"""val $fId = map.getObjOpt("$fKey").map((o: AST.Obj) => to${genTypeH(T, T, ft, p)._1}(o))"""
            }
        }
      }
      val r =
        st"""@pure def to$id(ast: AST.Obj): $id = {
            |  val map = ast.asMap
            |  ${(fieldStmts, "\n")}
            |  return $id(${(fieldIds, ", ")})
            |}"""
      return r
    }
    @strictpure def fPath(f: Field): ISZ[String] = f.defnIdOpt match {
      case Some(defn) => ISZ(defn, f.id)
      case _ => ISZ(f.id)
    }
    @pure def mkObjST(id: ST, fields: ISZ[Field]): ST = {
      @pure def isParam(f: Field): B = {
        if (!f.isRequired) {
          return F
        }
        f.tipe match {
          case ft: Type.Enum if ft.elements.size == 1 => return F
          case _ =>
        }
        return T
      }

      @pure def arg(f: Field): ST = {
        f.tipe match {
          case ft: Type.Enum if ft.elements.size == 1 =>
            var r = st"\"${ops.StringOps(ft.elements(0)).escapeST}\""
            if (!f.isRequired) {
              r = st"Some($r)"
            }
            return r
          case _ =>
        }
        if (f.isRequired) {
          return escField(F, f)
        }
        return st"None()"
      }

      val r =
        st"""@pure def mk$id(
            |  ${(for (f <- fields if isParam(f)) yield st"${escField(F, f)}: ${genType(T, f.isRequired, f.tipe, fPath(f))}", ",\n")}
            |): $id = {
            |  return $id(${(for (f <- fields) yield arg(f), ", ")})
            |}"""
      return r
    }
    def genTypeH(isVal: B, isRequired: B, t: Type, path: ISZ[String]): (ST, ISZ[String]) = {
      var descs: ISZ[String] = t.descOpt match {
        case Some(desc) => ISZ(desc)
        case _ => ISZ()
      }
      var r: ST = t match {
        case _: Type.Bool => st"B"
        case _: Type.Integer => st"Z"
        case _: Type.Number => st"F64"
        case _: Type.Str => st"String"
        case _: Type.Null => st"$topTypeId.$nullTypeId"
        case t: Type.Ref => st"${defnMap.get(t.ref).get.id}"
        case _: Type.OneOf => st"$topTypeId.$rawTypeId"
        case t: Type.Array =>
          val (eST, ds) = genTypeH(isVal, T, t.elementType, path :+ arrayPathId)
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
            objectMap = objectMap + t ~> id
            var defs = ISZ[ST]()
            for (f <- t.fields if !f.isRequired) {
              defs = defs :+ st"@strictpure def ${escField(F, f)}: ${genType(F, f.isRequired, f.tipe, path :+ f.id)} = ${escField(T, f)}.get"
            }
            defs = defs :+ fromBindingST(t.fields, path)
            val tST =
              st"""@datatype class $id(
                  |  ${(for (f <- t.fields) yield st"val ${escField(T, f)}: ${genType(T, f.isRequired, f.tipe, path :+ f.id)}", ",\n")}
                  |) extends $topTypeId {
                  |  ${(defs, "\n")}
                  |}"""
            sts = sts :+ tST
            val toST = toBindingST(id, t.fields, path)
            sts = sts :+ toST
            val mkST = mkObjST(id, t.fields)
            sts = sts :+ mkST
          }
          id
        case t: Type.Any =>
          descs = descs :+ st"Has to be any of { ${(t.jsonTypes, ", ")} }".render
          st"$topTypeId"
      }
      if (isVal && !isRequired) {
        r = st"Option[$r]"
      }
      return (r, descs)
    }

    def genType(isVal: B, isRequired: B, t: Type, path: ISZ[String]): ST = {
      val (tST, tDescs) = genTypeH(isVal, isRequired, t, path)
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

    for (p <- refs.entries if !defnMap.contains(p._1); posOpt <- p._2.elements) {
      reporter.error(posOpt, kind, s"Could not find $$ref: ${p._1}")
    }

    if (reporter.hasError) {
      return None()
    }

    var poset = Poset.empty[String]
    for (p <- defnMap.entries) {
      poset = poset.addParents(p._1, p._2.supers)
    }

    for (p <- defnMap.entries) {
      val (dId, defn) = p
      var fields = HashSMap.empty[String, Field]
      def addField(f: Field): Unit = {
        fields.get(f.id) match {
          case Some(pf) =>
            val opt: Field.Opt.Type = (pf.opt, f.opt) match {
              case (Field.Opt.Optional, Field.Opt.Required) => Field.Opt.Some
              case (Field.Opt.Required, _) => Field.Opt.Required
              case (Field.Opt.Some, _) => Field.Opt.Some
              case (_, _) => f.opt
            }
            fields = fields + f.id ~> f(opt = opt)
          case _ => fields = fields + f.id ~> f
        }
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
      if (isDefnTrait(dId)) {
        fields = fields -- (fields.keys -- (for (f <- defn.fields) yield f.id))
        val id = escId(defn.id)
        val defnST =
          st"""${mlCommentSTOpt(defn.titleOpt)}
              |${mlCommentSTOpt(defn.descOpt)}
              |@datatype trait $id${extendsSTOpt(if (defn.supers.isEmpty) ISZ(topTypeId) else defn.supers)} {
              |
              |  ${(for (f <- fields.values) yield st"def ${escField(F, f)}: ${genType(F, f.isRequired, f.tipe, fPath(f))}", "\n\n")}
              |
              |}"""
        sts = sts :+ defnST
        @pure def simpleId(defId: String): String = {
          val i = ops.StringOps(defId).lastIndexOf('/')
          return ops.StringOps(defId).substring(i + 1, defId.size)
        }
        val impls: ISZ[String] = for (descendant <- poset.descendantsOf(dId).elements if poset.childrenOf(descendant).isEmpty) yield simpleId(descendant)
        impls.size match {
          case z"0" =>
          case z"1" =>
            sts = sts :+
              st"""def to$id(ast: AST.Obj): $id = {
                  |  return to${escId(impls(0))}(ast)
                  |}"""
          case _ =>
        }
      } else {
        fields = fields ++ (for (f <- defn.fields) yield (f.id, f))
        val id = escId(defn.id)
        var defs = ISZ[ST]()
        for (f <- fields.values if !f.isRequired) {
          defs = defs :+ st"""@strictpure def ${escField(F, f)}: ${genTypeH(T, T, f.tipe, fPath(f))._1} = ${escField(T, f)}.get"""
        }
        defs = defs :+ fromBindingST(fields.values, ISZ())
        val defnST =
          st"""${mlCommentSTOpt(defn.titleOpt)}
              |${mlCommentSTOpt(defn.descOpt)}
              |@datatype class $id(
              |  ${(for (f <- fields.values) yield st"val ${escField(T, f)}: ${genType(T, f.isRequired, f.tipe, fPath(f))}", ",\n")}
              |)${extendsSTOpt(if (defn.supers.isEmpty) ISZ(topTypeId) else defn.supers)} {
              |  ${(defs, "\n")}
              |}"""
        sts = sts :+ defnST

        val toDefnST = toBindingST(id, fields.values, ISZ())
        sts = sts :+ toDefnST

        val mkDefnST = mkObjST(id, fields.values)
        sts = sts :+ mkDefnST
      }
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
          |import org.sireum.parser.json.AST
          |
          |$sourceOpt
          |
          |${mlCommentSTOpt(map.getStrValueOpt(titleId))}
          |${mlCommentSTOpt(map.getStrValueOpt(descId))}
          |object $objectName {
          |
          |  @datatype trait $topTypeId {
          |    @pure def toAST: AST
          |  }
          |  object $topTypeId {
          |    @datatype class $nullTypeId extends $topTypeId {
          |      @strictpure def toAST: AST = AST.Null(None())
          |    }
          |    @datatype class $rawTypeId(value: parser.json.AST) extends $topTypeId {
          |      @strictpure def toAST: AST = value
          |    }
          |  }
          |
          |  ${(sts, "\n\n")}
          |
          |}"""
    )
  }
}