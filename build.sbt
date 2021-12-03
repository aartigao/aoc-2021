import scala.scalanative.build._

scalaVersion := "2.13.6"

enablePlugins(ScalaNativePlugin)

nativeConfig ~= {
  _.withLinkStubs(true) // Set to false or remove if you want to show stubs as linking errors
    .withLTO(LTO.thin)
    .withMode(Mode.debug)
    .withGC(GC.none)
}

libraryDependencies ++= Seq(
  "dev.zio"    %%% "zio-streams" % "1.0.12",
  "org.ekrich" %%% "sjavatime"   % "1.1.5"
)

addCommandAlias("day1", """set Compile / selectMainClass := Some("_01.Main"); run""")
addCommandAlias("day2", """set Compile / selectMainClass := Some("_02.Main"); run""")
addCommandAlias("day3", """set Compile / selectMainClass := Some("_03.Main"); run""")