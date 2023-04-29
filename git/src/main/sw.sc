import java.nio.charset.StandardCharsets.UTF_8
import dev.rudiments.git.Commit

val hash = "81e30fc0c74ec502f99fd2f91784bfe1606e6a9f"

val str = """tree af1dd2b5ea96f90a06bd8b965ff784898782d7d2
            |parent 55095d03a9272ad936d8cf6e23a46e4fcc1da98d
            |author Yi-Jyun Pan <pan93412@gmail.com> 1628920089 +0800
            |committer Yi-Jyun Pan <pan93412@gmail.com> 1628920919 +0800
            |gpgsig -----BEGIN PGP SIGNATURE-----
            |
            | iQIzBAABCAAdFiEEs0GfOWTlygReNS0yQhVLGxz+M3cFAmEXXFkACgkQQhVLGxz+
            | M3d2Cw//R5H79nED3KRVmOoDNfpncopOf8m7dTc8Z+BIigtIVhQeIwnPih52u6tP
            | susk9ZyjaDXFJ6bqUBXKuXcFmttiw1Yklr15rm79HFceZyzCFZpw/HqNm/Wnx8sE
            | GYEoZ35tjuBVMog8zuRfXQNtfqwCLvUPk5xU0J1+OCM1TMkS5iLFOuvO0/x4kUNL
            | ARRi5bIvZgrS4+BqXuTMk2L64Ev+R8WdAOvUFsgwgwlH++UnHUJkIEaf9kczLgr8
            | Dg0lcZGdMlTKDEgLQJ0BDrFZX0eoXA8/yi3sBvqQ/lIBszznxZViNcFMierVvPAz
            | g1jWmyvo2s6MMmA/lVvS4/D1XyQPf5p09BhVBogqKk9U+2Rqun35QjtH2Wtb22AJ
            | xBsFt3uZ3MgenTc9TF8yXe4DZK60XHqXDIrslAUvc9LQ2QL4dXE7FsSaQVi49mls
            | 3nzfK5tEMU2cEjX7DYxhfSWsh1g7E+GA2fYelol1bhbPOmUcVk5FyGj42YVn7o3d
            | xsdwWWYAePNuToeuoMXCdkIioQHoHx1MC4k6IzEK0lFJQ/998C1MPM/K8uMOkWdZ
            | ag48tK54+0Nvek6ULAbWS9IOTyviaHtEoaUtVIfPDDhMhGekXW5OFEFV3eJwPJ/H
            | XMwETjmmWoUFoOiLomtCxR2cihdjrE9OPsgseXMIJ1oLGxPc9sE=
            | =14DB
            | -----END PGP SIGNATURE-----
            |
            |l10n: zh_TW.po: update for v2.33.0 rnd 2
            |
            |Signed-off-by: Yi-Jyun Pan <pan93412@gmail.com>
            |""".stripMargin



val treeR = """tree (?<tree>\w{40})""".r
val parentR = """parent (?<parent>\w{40})""".r
val authorR = """author (?<name>.+) <(?<email>.+)> (?<epoch>\d{10,20}) (?<zone>.+)""".r
val committerR = """committer (?<name>.+) <(?<email>.+)> (?<epoch>\d{10,20}) (?<zone>.+)""".r
val signatureR = """gpgsig -----BEGIN PGP SIGNATURE-----(.*\n)* -----END PGP SIGNATURE-----\n""".r

val tree =  treeR.findAllMatchIn(str).map(_.group(1)).toSeq.head
val parent = parentR.findAllMatchIn(str).map(_.group(1)).toSeq
val author = authorR.findAllMatchIn(str).map(_.group(1)).toSeq.head
val committer = committerR.findAllMatchIn(str).map(_.group(1)).toSeq.head

val signature = signatureR.findAllMatchIn(str).mkString("|")

val commit = Commit.apply(str.getBytes(UTF_8))