package vct.col.resolve.lang

import vct.col.ast._
import vct.col.origin._
import vct.col.ref.Ref
import vct.col.resolve._
import vct.col.resolve.ctx._
import vct.result.VerificationError.UserError

import scala.annotation.tailrec

case object Spec {
  def getContract[G](
      target: Referrable[G],
      blame: Node[G],
  ): ApplicableContract[G] =
    target match {
      case RefFunction(decl) => decl.contract
      case RefProcedure(decl) => decl.contract
      case RefInstanceFunction(decl) => decl.contract
      case RefInstanceMethod(decl) => decl.contract
      case RefCLocalDeclaration(decls, _) => decls.decl.contract
      case RefCGlobalDeclaration(decls, _) => decls.decl.contract
      case RefCFunctionDefinition(decl) => decl.contract
      case RefCPPLocalDeclaration(decls, _) => decls.decl.contract
      case RefCPPGlobalDeclaration(decls, _) => decls.decl.contract
      case RefCPPFunctionDefinition(decl) => decl.contract
      case RefCPPLambdaDefinition(decl) => decl.contract
      case RefJavaConstructor(decl) => decl.contract
      case RefJavaMethod(decl) => decl.contract
      case RefPVLConstructor(decl) => decl.contract
      case other => throw NoGivenYields(blame)
    }

  def resolveGiven[G](
      givenMap: Seq[(Ref[G, Variable[G]], Expr[G])],
      target: Referrable[G],
      blame: Node[G],
  ): Unit =
    if (givenMap.nonEmpty) {
      val contract = getContract(target, blame)
      val args = contract.givenArgs.flatMap(Referrable.from)
      givenMap.foreach { case (arg, _) =>
        arg.tryResolve(name =>
          args.collectFirst {
            case ref: RefVariable[G] if ref.name == name => ref.decl
          }.getOrElse(throw NoSuchNameError("'given' argument", name, blame))
        )
      }
    }

  def resolveYields[G](
      ctx: ReferenceResolutionContext[G],
      yields: Seq[(Expr[G], Ref[G, Variable[G]])],
      target: Referrable[G],
      blame: Node[G],
  ): Unit =
    if (yields.nonEmpty) {
      val contract = getContract(target, blame)
      val args = contract.yieldsArgs.flatMap(Referrable.from)
      yields.foreach { case (_, res) =>
        res.tryResolve(name =>
          args.collectFirst {
            case ref: RefVariable[G] if ref.name == name => ref.decl
          }.getOrElse(throw NoSuchNameError("'yields' argument", name, blame))
        )
      }
    }

  case class BuiltinArgumentCountError(obj: Expr[_], expected: Int)
      extends UserError {
    override def text: String =
      obj.o.messageInContext(
        s"Builtin method invocation has wrong argument count: expected $expected argument(s)."
      )
    override def code: String = "builtinArgCount"
  }

  def builtinField[G](
      obj: Expr[G],
      field: String,
      blame: Blame[BuiltinError],
  ): Option[BuiltinField[G]] = {
    implicit val o: Origin = obj.o

    @tailrec
    def matchBuiltin(t: Type[G], field: String): Option[Expr[G] => Expr[G]] = {
      (t, field) match {
        case (TSubtype(_, supertype, _), field) =>
          matchBuiltin(supertype, field)

        case (TArray(_), "length") => Some(Length(_)(blame))

        case (_: SizedType[G], "isEmpty") => Some(Empty(_))
        case (_: SizedType[G], "size") => Some(Size(_))

        case (TSeq(_), "head") => Some(Head(_)(blame))
        case (TSeq(_), "tail") => Some(Tail(_))

        case (TMap(_, _), "values") => Some(MapValueSet(_))
        case (TMap(_, _), "items") => Some(MapItemSet(_))
        case (TMap(_, _), "keys") => Some(MapKeySet(_))

        case (TOption(_), "get") => Some(OptGet(_)(blame))
        case (TOption(_), "isEmpty") => Some(OptEmpty(_))

        case (TTuple(_), "fst") => Some(TupGet(_, 0))
        case (TTuple(_), "snd") => Some(TupGet(_, 1))

        case (TEither(_, _), "left") => Some(GetLeft(_)(blame))
        case (TEither(_, _), "right") => Some(GetRight(_)(blame))
        case (TEither(_, _), "isLeft") => Some(IsLeft(_))
        case (TEither(_, _), "isRight") => Some(IsRight(_))

        case _ => None
      }
    }
    matchBuiltin(obj.t, field).map(BuiltinField(_))
  }

  def argCount[G](
      n: Int
  )(f: Expr[G] => Seq[Expr[G]] => Expr[G]): Expr[G] => Seq[Expr[G]] => Expr[G] =
    obj =>
      args =>
        if (args.size == n)
          f(obj)(args)
        else
          throw BuiltinArgumentCountError(obj, n)

  def builtinInstanceMethod[G](
      obj: Expr[G],
      method: String,
      blame: Blame[BuiltinError],
  ): Option[BuiltinInstanceMethod[G]] = {
    implicit val o: Origin = obj.o

    def matchBuiltin(
        t: Type[G],
        method: String,
    ): Option[Expr[G] => Seq[Expr[G]] => Expr[G]] = {
      (t, method) match {
        case (t: TNotAValue[G], _) =>
          (t.decl.get, method) match {
            case (RefModel(model), "create") =>
              Some(_ => _ => ModelNew(model.ref))
            case (_, _) => None
          }

        case (TSubtype(_, supertype, _), method) =>
          matchBuiltin(supertype, method)

        case (TModel(_), "state") =>
          Some(argCount(2)(obj => args => ModelState(obj, args(0), args(1))))
        case (TModel(_), "abstractState") =>
          Some(argCount(1)(obj => args => ModelAbstractState(obj, args.head)))
        case (TModel(_), "create") =>
          Some(argCount(1)(obj => args => ModelCreate(obj, args.head)))
        case (TModel(_), "destroy") =>
          Some(argCount(0)(obj => _ => ModelDestroy(obj)))
        case (TModel(_), "split") =>
          Some(argCount(4)(obj =>
            args => ModelSplit(obj, args(0), args(1), args(2), args(3))
          ))
        case (TModel(_), "merge") =>
          Some(argCount(4)(obj =>
            args => ModelMerge(obj, args(0), args(1), args(2), args(3))
          ))
        case (TModel(_), "choose") =>
          Some(argCount(3)(obj =>
            args => ModelChoose(obj, args(0), args(1), args(2))
          ))

        case (TSeq(_), "removeAt") =>
          Some(argCount(1)(obj => args => RemoveAt(obj, args.head)))
        case (TSeq(_), "concat") =>
          Some(argCount(1)(obj => args => Concat(obj, args.head)))
        case (TSeq(_), "prepend") =>
          Some(argCount(1)(obj => args => Cons(obj, args.head)))
        case (TSeq(_), "drop") =>
          Some(argCount(1)(obj => args => Drop(obj, args.head)))
        case (TSeq(_), "take") =>
          Some(argCount(1)(obj => args => Take(obj, args.head)))
        case (TSeq(_), "slice") =>
          Some(argCount(2)(obj => args => Slice(obj, args(0), args(1))))
        case (TSeq(_), "update") =>
          Some(argCount(2)(obj => args => SeqUpdate(obj, args(0), args(1))))
        case (TSeq(_), "contains") =>
          Some(argCount(1)(obj => args => SeqMember(args.head, obj)))

        case (TSet(_), "intersect") =>
          Some(argCount(1)(obj => args => SetIntersection(obj, args.head)))
        case (TSet(_), "contains") =>
          Some(argCount(1)(obj => args => SetMember(args.head, obj)))
        case (TSet(_), "difference") =>
          Some(argCount(1)(obj => args => SetMinus(obj, args.head)))
        case (TSet(_), "union") =>
          Some(argCount(1)(obj => args => SetUnion(obj, args.head)))
        case (TSet(_), "strictSubsetOf") =>
          Some(argCount(1)(obj => args => SubSet(obj, args.head)))
        case (TSet(_), "subsetOf") =>
          Some(argCount(1)(obj => args => SubSetEq(obj, args.head)))

        case (TBag(_), "sum") =>
          Some(argCount(1)(obj => args => BagAdd(obj, args.head)))
        case (TBag(_), "intersect") =>
          Some(argCount(1)(obj => args => BagLargestCommon(obj, args.head)))
        case (TBag(_), "count") =>
          Some(argCount(1)(obj => args => BagMemberCount(args.head, obj)))
        case (TBag(_), "difference") =>
          Some(argCount(1)(obj => args => BagMinus(obj, args.head)))
        case (TBag(_), "subbagOf") =>
          Some(argCount(1)(obj => args => SubBagEq(obj, args.head)))
        case (TBag(_), "strictSubbagOf") =>
          Some(argCount(1)(obj => args => SubBag(obj, args.head)))

        case (TMap(_, _), "add") =>
          Some(argCount(2)(obj => args => MapCons(obj, args(0), args(1))))
        case (TMap(_, _), "remove") =>
          Some(argCount(1)(obj => args => MapRemove(obj, args.head)))
        case (TMap(_, _), "get") =>
          Some(argCount(1)(obj => args => MapGet(obj, args.head)(blame)))
        case (TMap(_, _), "equals") =>
          Some(argCount(1)(obj => args => MapEq(obj, args.head)))
        case (TMap(_, _), "disjoint") =>
          Some(argCount(1)(obj => args => MapDisjoint(obj, args.head)))

        case (TOption(_), "getOrElse") =>
          Some(argCount(1)(obj => args => OptGetOrElse(obj, args.head)))

        case (_, _) => None
      }
    }

    matchBuiltin(obj.t, method).map(BuiltinInstanceMethod(_))
  }

  def findLabel[G](
      name: String,
      ctx: ReferenceResolutionContext[G],
  ): Option[LabelDecl[G]] =
    ctx.stack.flatten.collectFirst {
      case ref @ RefLabelDecl(decl) if ref.name == name => decl
    }

  def findSend[G](
      name: String,
      ctx: ReferenceResolutionContext[G],
  ): Option[SendDecl[G]] =
    ctx.stack.flatten.collectFirst {
      case ref @ RefSendDecl(decl) if ref.name == name => decl
    }

  def findLocal[G](
      name: String,
      ctx: ReferenceResolutionContext[G],
  ): Option[Variable[G]] =
    ctx.stack.flatten.collectFirst {
      case ref @ RefVariable(decl) if ref.name == name => decl
    }

  def findClass[G](
      name: String,
      ctx: TypeResolutionContext[G],
  ): Option[Class[G]] =
    ctx.stack.flatten.collectFirst {
      case ref @ RefClass(decl) if ref.name == name => decl
    }

  def findModel[G](
      name: String,
      ctx: TypeResolutionContext[G],
  ): Option[Model[G]] =
    ctx.stack.flatten.collectFirst {
      case ref @ RefModel(decl) if ref.name == name => decl
    }

  def findModelField[G](
      name: String,
      ctx: ReferenceResolutionContext[G],
  ): Option[ModelField[G]] =
    ctx.stack.flatten.collectFirst {
      case ref @ RefModelField(decl) if ref.name == name => decl
    }

  def findParBlock[G](
      name: String,
      ctx: ReferenceResolutionContext[G],
  ): Option[ParBlockDecl[G]] =
    ctx.stack.flatten.collectFirst {
      case ref @ RefParBlockDecl(decl) if ref.name == name => decl
    }

  def findParInvariant[G](
      name: String,
      ctx: ReferenceResolutionContext[G],
  ): Option[ParInvariantDecl[G]] =
    ctx.stack.flatten.collectFirst {
      case ref @ RefParInvariantDecl(decl) if ref.name == name => decl
    }

  def findAdt[G](
      name: String,
      ctx: TypeResolutionContext[G],
  ): Option[AxiomaticDataType[G]] =
    ctx.stack.flatten.collectFirst {
      case ref @ RefAxiomaticDataType(decl) if ref.name == name => decl
    }

  def findAdtFunction[G](
      decl: AxiomaticDataType[G],
      name: String,
  ): Option[ADTFunction[G]] =
    decl.decls.flatMap(Referrable.from).collectFirst {
      case ref @ RefADTFunction(f) if ref.name == name => f
    }

  def findAdtFunction[G](
      adtName: String,
      functionName: String,
      ctx: ReferenceResolutionContext[G],
  ): Option[ADTFunction[G]] = {
    ctx.stack.flatten.collectFirst {
      case ref @ RefAxiomaticDataType(adt) if ref.name == adtName => adt
    }.flatMap(findAdtFunction(_, functionName))
  }

  def findAdtFunction[G](
      name: String,
      ctx: ReferenceResolutionContext[G],
  ): Option[(AxiomaticDataType[G], ADTFunction[G])] = {
    for (
      adt <- ctx.stack.flatten.collect { case RefAxiomaticDataType(adt) => adt }
    ) {
      findAdtFunction(adt, name) match {
        case Some(func) => return Some((adt, func))
        case None =>
      }
    }

    None
  }

  def findProcedure[G](
      name: String,
      ctx: ReferenceResolutionContext[G],
  ): Option[Procedure[G]] =
    ctx.stack.flatten.collectFirst {
      case ref @ RefProcedure(decl) if ref.name == name => decl
    }

  def findFunction[G](
      name: String,
      ctx: ReferenceResolutionContext[G],
  ): Option[Function[G]] =
    ctx.stack.flatten.collectFirst {
      case ref @ RefFunction(decl) if ref.name == name => decl
    }

  def findPredicate[G](
      name: String,
      ctx: ReferenceResolutionContext[G],
  ): Option[Predicate[G]] =
    ctx.stack.flatten.collectFirst {
      case ref @ RefPredicate(decl) if ref.name == name => decl
    }

  def findGlobalSubtype[G](
      name: String,
      ctx: TypeResolutionContext[G],
  ): Option[GlobalSubtype[G]] =
    ctx.stack.flatten.collectFirst {
      case ref @ RefGlobalSubtype(decl) if ref.name == name => decl
    }

  def findSilverField[G](
      name: String,
      ctx: ReferenceResolutionContext[G],
  ): Option[SilverField[G]] =
    ctx.stack.flatten.collectFirst {
      case ref @ RefSilverField(decl) if ref.name == name => decl
    }

  def findMethod[G](obj: Expr[G], name: String): Option[InstanceMethod[G]] =
    obj.t match {
      case TClass(Ref(cls), _) =>
        cls.decls.flatMap(Referrable.from).collectFirst {
          case ref @ RefInstanceMethod(decl) if ref.name == name => decl
        }
      case _ => None
    }

  def findInstanceFunction[G](
      obj: Expr[G],
      name: String,
  ): Option[InstanceFunction[G]] =
    obj.t match {
      case TClass(Ref(cls), _) =>
        cls.decls.flatMap(Referrable.from).collectFirst {
          case ref @ RefInstanceFunction(decl) if ref.name == name => decl
        }
      case _ => None
    }

  def findInstancePredicate[G](
      obj: Expr[G],
      name: String,
  ): Option[InstancePredicate[G]] =
    obj.t match {
      case TClass(Ref(cls), _) =>
        cls.decls.flatMap(Referrable.from).collectFirst {
          case ref @ RefInstancePredicate(decl) if ref.name == name => decl
        }
      case JavaTClass(Ref(cls), _) =>
        cls.decls.flatMap(Referrable.from).collectFirst {
          case ref @ RefInstancePredicate(decl) if ref.name == name => decl
        }
      case _ => None
    }

  // using global resolution should be a temporary hack
  def findInstanceSubtype[G](
      name: String,
      ctx: TypeResolutionContext[G],
  ): Option[InstanceSubtype[G]] =
    ctx.stack.flatten.collectFirst {
      case ref @ RefInstanceSubtype(decl) if ref.name == name => decl
    }

  def findField[G](obj: Expr[G], name: String): Option[InstanceField[G]] =
    obj.t match {
      case TClass(Ref(cls), _) =>
        cls.decls.flatMap(Referrable.from).collectFirst {
          case ref @ RefField(decl) if ref.name == name => decl
        }
      case _ => None
    }

  def findModelField[G](obj: Expr[G], name: String): Option[ModelField[G]] =
    obj.t match {
      case TModel(Ref(model)) =>
        model.declarations.flatMap(Referrable.from).collectFirst {
          case ref @ RefModelField(decl) if ref.name == name => decl
        }
      case _ => None
    }

  def findAdtTypeArg[G](
      adt: AxiomaticDataType[G],
      name: String,
  ): Option[Variable[G]] =
    adt.typeArgs.flatMap(Referrable.from).collectFirst {
      case ref @ RefVariable(decl) if ref.name == name => decl
    }
}
