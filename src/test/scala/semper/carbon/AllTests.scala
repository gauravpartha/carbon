/*
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */

package semper.carbon

import semper.sil.testing.SilSuite
import semper.sil.verifier.Verifier
import semper.sil.frontend.Frontend
import java.io.File
import io.Source
import java.nio.file.Path

/** All tests for carbon.

  */
class AllTests extends SilSuite {

  override def testDirectories: Seq[String] = Vector("all", "local"
    //, "generated"
  )

  override def frontend(verifier: Verifier, files: Seq[Path]): Frontend = {
    require(files.length == 1, "tests should consist of exactly one file")
    val fe = new CarbonFrontend()
    fe.init(verifier)
    fe.reset(files.head)
    fe
  }

  override def verifiers: Seq[Verifier] = Vector(CarbonVerifier())
}
