lazy val scala213 = "2.13.8"

lazy val supportedScalaVersions = List(scala213)

// Common configuration
inThisBuild(
  List(
    scalaVersion         := scala213,
    crossScalaVersions   := supportedScalaVersions,
    version              := "0.1.0",
    description          := "code de l'article automatiser la creation de documents administratifs avec zio-notion",
    organization         := "io.univalence",
    organizationName     := "Univalence",
    organizationHomepage := Some(url("https://univalence.io/")),
    startYear            := Some(2022),
    developers           := List(),
    homepage             := Some(url("https://github.com/univalence/zio-spark")),
    licenses             := List("Apache-2.0" -> url("https://github.com/univalence/zio-spark/blob/master/LICENSE")),
    versionScheme        := Some("early-semver"),
    version ~= addVersionPadding
  )
)

// Scalafix configuration
ThisBuild / scalafixScalaBinaryVersion := "2.13"
ThisBuild / semanticdbEnabled          := true
ThisBuild / semanticdbVersion          := scalafixSemanticdb.revision
ThisBuild / scalafixDependencies ++= Seq("com.github.vovapolu" %% "scaluzzi" % "0.1.21")

// SCoverage configuration
val excludedPackages: Seq[String] = Seq()

ThisBuild / coverageFailOnMinimum           := false
ThisBuild / coverageMinimumStmtTotal        := 80
ThisBuild / coverageMinimumBranchTotal      := 80
ThisBuild / coverageMinimumStmtPerPackage   := 50
ThisBuild / coverageMinimumBranchPerPackage := 50
ThisBuild / coverageMinimumStmtPerFile      := 0
ThisBuild / coverageMinimumBranchPerFile    := 0
ThisBuild / coverageExcludedPackages        := excludedPackages.mkString(";")

// Aliases
addCommandAlias("fmt", "scalafmt")
addCommandAlias("fmtCheck", "scalafmtCheckAll")
addCommandAlias("lint", "scalafix")
addCommandAlias("lintCheck", "scalafixAll --check")
addCommandAlias("check", "; fmtCheck; lintCheck;")
addCommandAlias("fixStyle", "; scalafmtAll; scalafixAll;")
addCommandAlias("prepare", "fixStyle")
addCommandAlias("testAll", "; clean;+ test;")
addCommandAlias("testSpecific", "; clean; test;")
addCommandAlias("testSpecificWithCoverage", "; clean; coverage; test; coverageReport;")

// -- Lib versions
lazy val libVersion =
  new {
    // -- Test
    val scalatest = "3.2.14"
    val zio       = "2.0.2"
    val zioNotion = "0.9.0"
    val pdfBox    = "2.0.27"
    val boxable   = "1.7.0"
  }

// -- Main project settings
lazy val source =
  (project in file("."))
    .settings(
      name := "hrops",
      scalacOptions ~= fatalWarningsAsProperties,
      libraryDependencies ++= Seq(
        "org.scalatest"      %% "scalatest"  % libVersion.scalatest % Test,
        "io.univalence"      %% "zio-notion" % libVersion.zioNotion,
        "dev.zio"            %% "zio"        % libVersion.zio,
        "org.apache.pdfbox"   % "pdfbox"     % libVersion.pdfBox,
        "com.github.dhorions" % "boxable"    % libVersion.boxable
      )
    )

/**
 * Don't fail the compilation for warnings by default, you can still
 * activate it using system properties (It should always be activated in
 * the CI).
 */
def fatalWarningsAsProperties(options: Seq[String]): Seq[String] =
  if (sys.props.getOrElse("fatal-warnings", "false") == "true") options
  else options.filterNot(Set("-Xfatal-warnings"))

/**
 * Add padding to change: 0.1.0+48-bfcea99ap20220317-1157-SNAPSHOT into
 * 0.1.0+0048-bfcea99ap20220317-1157-SNAPSHOT. It helps to retrieve the
 * latest snapshots from
 * https://oss.sonatype.org/#nexus-search;gav~io.univalence~zio-spark_2.13~~~~kw,versionexpand.
 */
def addVersionPadding(baseVersion: String): String = {
  import scala.util.matching.Regex

  val paddingSize    = 5
  val counter: Regex = "\\+([0-9]+)-".r

  counter.findFirstMatchIn(baseVersion) match {
    case Some(regex) =>
      val count          = regex.group(1)
      val snapshotNumber = "0" * (paddingSize - count.length) + count
      counter.replaceFirstIn(baseVersion, s"+$snapshotNumber-")
    case None => baseVersion
  }
}
