package semper.carbon.modules.impls

import semper.carbon.modules._
import semper.sil.{ast => sil}
import semper.carbon.boogie._
import semper.carbon.verifier.{Environment, Verifier}
import semper.carbon.boogie.Implicits._
import semper.sil.ast.utility._
import semper.carbon.modules.components.{InhaleComponent, ExhaleComponent, DefinednessComponent}
import semper.sil.verifier.{errors, PartialVerificationError}

/**
 * The default implementation of a [[semper.carbon.modules.FuncPredModule]].
 *
 * @author Stefan Heule
 */
class DefaultFuncPredModule(val verifier: Verifier) extends FuncPredModule
  with DefinednessComponent with ExhaleComponent with InhaleComponent {
  def name = "Function and predicate module"

  import verifier._
  import typeModule._
  import mainModule._
  import stateModule._
  import expModule._
  import exhaleModule._
  import inhaleModule._
  import heapModule._
  import permModule._

  implicit val fpNamespace = verifier.freshNamespace("funcpred")

  lazy val heights = Functions.heights(verifier.program)
  private val assumeFunctionsAboveName = Identifier("AssumeFunctionsAbove")
  private val assumeFunctionsAbove: Const = Const(assumeFunctionsAboveName)
  private val limitedPostfix = "'"
  private val resultName = Identifier("Result")

  override def preamble = {
    if (verifier.program.functions.isEmpty) Nil
    else {
      val m = heights.values.max
      DeclComment("Function heights (higher height means its body is available earlier):") ++
        (for (i <- m to 0 by -1) yield {
          val fs = heights.toSeq filter (p => p._2 == i) map (_._1.name)
          DeclComment(s"- height $i: ${fs.mkString(", ")}")
        }) ++
        ConstDecl(assumeFunctionsAboveName, Int)
    }
  }

  override def initialize() {
    expModule.register(this)
  }

  override def translateFunction(f: sil.Function): Seq[Decl] = {
    env = Environment(verifier, f)
    val funcionDefs: Seq[Func] = functionDefinitions(f)
    val res = MaybeCommentedDecl(s"Translation of function ${f.name}",
      MaybeCommentedDecl("Uninterpreted function definitions", funcionDefs, size = 1) ++
        MaybeCommentedDecl("Definitional axiom", definitionalAxiom(f), size = 1) ++
        MaybeCommentedDecl("Postcondition axioms", postconditionAxiom(f), size = 1) ++
        MaybeCommentedDecl("Check contract well-formedness and postcondition", checkFunctionDefinedness(f), size = 1)
      , nLines = 2)
    env = null
    res
  }

  private def functionDefinitions(f: sil.Function): Seq[Func] = {
    val typ = translateType(f.typ)
    val args = heapModule.stateContributions ++ (f.formalArgs map translateLocalVarDecl)
    val func = Func(Identifier(f.name), args, typ)
    val limitedFunc = Func(Identifier(f.name + limitedPostfix), args, typ)
    func ++ limitedFunc
  }

  override def translateFuncApp(fa: sil.FuncApp) = {
    translateFuncApp(fa.func, heapModule.currentState ++ (fa.args map translateExp), fa.typ)
  }
  def translateFuncApp(f: sil.Function, args: Seq[Exp], typ: sil.Type) = {
    FuncApp(Identifier(f.name), args, translateType(typ))
  }

  private def assumeFunctionsAbove(i: Int): Exp =
    assumeFunctionsAbove > IntLit(i)

  def assumeAllFunctionDefinitions: Stmt = {
    if (verifier.program.functions.isEmpty) Nil
    else Assume(assumeFunctionsAbove(((heights map (_._2)).max) + 1))
  }

  private def definitionalAxiom(f: sil.Function): Seq[Decl] = {
    val height = heights(f)
    val heap = heapModule.stateContributions
    val args = f.formalArgs map translateLocalVarDecl
    val fapp = translateFuncApp(f, (heap ++ args) map (_.l), f.typ)
    def transformLimited: PartialFunction[Exp, Option[Exp]] = {
      case FuncApp(recf, recargs, t) if recf.namespace == fpNamespace =>
        // change all function applications to use the limited form, and still go through all arguments
        Some(FuncApp(Identifier(recf.name + limitedPostfix), recargs map (_.transform(transformLimited)), t))
    }
    val body = translateExp(f.exp) transform transformLimited
    Axiom(Forall(
      stateModule.stateContributions ++ args,
      Trigger(Seq(staticGoodState, fapp)),
      (staticGoodState && assumeFunctionsAbove(height)) ==>
        (fapp === body)
    ))
  }

  private def postconditionAxiom(f: sil.Function): Seq[Decl] = {
    val height = heights(f)
    val heap = heapModule.stateContributions
    val args = f.formalArgs map translateLocalVarDecl
    val fapp = translateFuncApp(f, (heap ++ args) map (_.l), f.typ)
    val res = translateResult(sil.Result()(f.typ))
    for (post <- f.posts) yield {
      val bPost = translateExp(post) transform {
        case e if e == res => Some(fapp)
      }
      Axiom(Forall(
        stateModule.stateContributions ++ args,
        Trigger(Seq(staticGoodState, fapp)),
        (staticGoodState && assumeFunctionsAbove(height)) ==> bPost))
    }
  }

  private def checkFunctionDefinedness(f: sil.Function) = {
    val args = f.formalArgs map translateLocalVarDecl
    val res = sil.Result()(f.typ)
    val init = MaybeCommentBlock("Initializing the state",
      stateModule.initState ++ (f.formalArgs map allAssumptionsAboutParam) ++ assumeAllFunctionDefinitions)
    val initOld = MaybeCommentBlock("Initializing the old state", stateModule.initOldState)
    val checkPre = MaybeCommentBlock("Inhaling precondition (with checking)",
      f.pres map (e => checkDefinednessOfSpec(e, errors.FunctionNotWellformed(f))))
    val checkExp = MaybeCommentBlock("Check definedness of function body",
      expModule.checkDefinedness(f.exp, errors.FunctionNotWellformed(f)))
    val exp = MaybeCommentBlock("Translate function body",
      translateResult(res) := translateExp(f.exp))
    val checkPost = MaybeCommentBlock("Inhaling precondition (with checking)",
      f.posts map (e => checkDefinednessOfSpecAndExhale(e, errors.ContractNotWellformed(e), errors.PostconditionViolated(e, f))))
    val body = Seq(init, initOld, checkPre, checkExp, exp, checkPost)
    Procedure(Identifier(f.name + "#definedness"), args, translateResultDecl(res), body)
  }

  private def translateResultDecl(r: sil.Result) = LocalVarDecl(resultName, translateType(r.typ))
  override def translateResult(r: sil.Result) = translateResultDecl(r).l

  override def simplePartialCheckDefinedness(e: sil.Exp, error: PartialVerificationError): Stmt = {
    e match {
      case fa@sil.FuncApp(f, args) if !fa.pres.isEmpty =>
        NondetIf(
          MaybeComment("Exhale precondition of function application", exhale(fa.pres map (e => (e, errors.PreconditionInAppFalse(fa))))) ++
            MaybeComment("Stop execution", Assume(FalseLit()))
        )
      case _ => Nil
    }
  }

  private var tmpStateId = -1
  override def partialCheckDefinedness(e: sil.Exp, error: PartialVerificationError): (() => Stmt, () => Stmt) = {
    e match {
      case u@sil.Unfolding(acc@sil.PredicateAccessPredicate(loc, perm), exp) =>
        tmpStateId += 1
        val tmpStateName = if (tmpStateId == 0) "Unfolding" else s"Unfolding$tmpStateId"
        val (stmt, state) = stateModule.freshTempState(tmpStateName)
        def before() = {
          stmt ++ unfoldPredicate(acc, error)
        }
        def after() = {
          tmpStateId -= 1
          stateModule.restoreState(state)
          Nil
        }
        (before, after)
      case _ => (() => simplePartialCheckDefinedness(e, error), () => Nil)
    }
  }

  // --------------------------------------------

  override def translatePredicate(p: sil.Predicate): Seq[Decl] = {
    predicateGhostFieldDecl(p)
  }

  override def translateFold(fold: sil.Fold): Stmt = {
    fold match {
      case sil.Fold(acc@sil.PredicateAccessPredicate(pa@sil.PredicateAccess(rcv, pred), perm)) =>
        checkDefinedness(acc, errors.FoldFailed(fold)) ++
          checkDefinedness(perm, errors.FoldFailed(fold)) ++
          foldPredicate(acc, errors.FoldFailed(fold))
    }
  }

  private var duringFold = false
  private def foldPredicate(acc: sil.PredicateAccessPredicate, error: PartialVerificationError): Stmt = {
    duringFold = true
    val stmt = exhale(Seq((acc.loc.predicateBody, error))) ++
      inhale(acc)
    duringFold = false
    stmt
  }

  private var duringUnfold = false
  override def translateUnfold(unfold: sil.Unfold): Stmt = {
    duringUnfold = true
    val stmt = unfold match {
      case sil.Unfold(acc@sil.PredicateAccessPredicate(pa@sil.PredicateAccess(rcv, pred), perm)) =>
        checkDefinedness(acc, errors.UnfoldFailed(unfold)) ++
          checkDefinedness(perm, errors.UnfoldFailed(unfold)) ++
          unfoldPredicate(acc, errors.UnfoldFailed(unfold))
    }
    duringUnfold = false
    stmt
  }

  private def unfoldPredicate(acc: sil.PredicateAccessPredicate, error: PartialVerificationError): Stmt = {
    exhale(Seq((acc, error))) ++
      inhale(acc.loc.predicateBody)
  }

  override def exhaleExp(e: sil.Exp, error: PartialVerificationError): Stmt = {
    e match {
      case sil.Unfolding(perm, exp) => ???
      case sil.PredicateAccessPredicate(loc@sil.PredicateAccess(rcv, pred), perm) if duringUnfold =>
        val oldVersion = LocalVar(Identifier("oldVersion"), Int)
        val newVersion = LocalVar(Identifier("newVersion"), Int)
        val curVersion = translateExp(loc)
        val stmt = (oldVersion := curVersion) ++
          Havoc(Seq(newVersion)) ++
          Assume(oldVersion < newVersion) ++
          (curVersion := newVersion)
        If(hasDirectPerm(loc), stmt, Nil)
      case _ => Nil
    }
  }

  override def inhaleExp(e: sil.Exp): Stmt = {
    e match {
      case sil.Unfolding(perm, exp) => ???
      case _ => Nil
    }
  }
}
