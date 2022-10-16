package viper.carbon.utility

import viper.carbon.boogie._
import viper.carbon.verifier.Verifier

class Monomorphizer(val verifier: Verifier) {


  /***
    *
    * @param e
    * @param conreteInstances mapping from type constructors to concrete instances of that constructor and the concrete
    *                         type variable instantiations (given from left to right)
    * @return
    */
  def monomorphize(e: Exp, concreteInstances: Map[String, (Type, NamedType)]) : Seq[Exp] =
  {
    e match {
      case Forall(xs, triggers, body, ts) =>
        val bodyRes = monomorphize(body, concreteInstances)

        /** find all possible instances for ts */
        //TODO: there could be constraints on ts in body
    }
  }

  def findInstances(xs: Seq[LocalVarDecl], ts: Seq[TypeVar], concreteInstances: Map[String, Seq[NamedType]]) : Option[Seq[Type]] = {
    var m: Map[TypeVar, Seq[Type]] = Map.empty //track valid instances for each type variable

    xs.foreach { x =>
      val typ = x.typ

      if(!typ.freeTypeVars.isEmpty) {
        typ match {
          case NamedType(name, typeArgs) =>
            val concreteMatchCandidates = concreteInstances.get(name).get

            val concreteMatches = concreteMatchCandidates.filter(
              arg => arg.name.equals(name) && unifiable(typeArgs, arg.typVars)
            )

            val singleConstraints : Seq[Map[TypeVar, Seq[Type]]] =
              concreteMatches.map(nt => {
                  val temp : Seq[(Type, Type)] = typeArgs.zip(nt.typVars).filter(t1t2 => ts.contains(t1t2._1))
                  temp.foldLeft[Map[TypeVar, Seq[Type]]](Map.empty)(mAndT1T2 => {
                    val (m, (t1, t2)) = mAndT1T2

                  }
                }
              )

            val constraints = mergeConstraints(singleConstraints)

            /** TODO: generate all instantiations based on constraints */
          case _ =>
            return None //don't yet support monomorphization of non-named types
        }
      }
    }
  }

  def unifiable(ts1: Seq[Type], ts2: Seq[Type]) : Boolean =
  {
    ???
  }

  def mergeConstraints(singleConstraints: Seq[Map[TypeVar, Seq[Type]]]) : Seq[Map[TypeVar, Seq[Type]]] =
  {
    ???
  }

}
