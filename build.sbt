scalaVersion := "2.13.6"

// Set to false or remove if you want to show stubs as linking errors
nativeLinkStubs := true

enablePlugins(ScalaNativePlugin)

libraryDependencies ++= Seq(
  "dev.zio"    %%% "zio-streams" % "1.0.12",
  "org.ekrich" %%% "sjavatime"   % "1.1.5"
)
