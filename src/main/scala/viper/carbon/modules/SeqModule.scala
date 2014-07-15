/*
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */

package semper.carbon.modules

import semper.sil.{ast => sil}
import semper.carbon.boogie.{Type, Exp}

/**
 * A module for translating sequences.

 */
trait SeqModule extends Module {
  def translateSeqExp(exp: sil.Exp): Exp
  def translateSeqType(seqType: sil.SeqType): Type
}