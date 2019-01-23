name := """typerighter"""
organization := "com.gu"

version := "1.0-SNAPSHOT"

lazy val root = (project in file(".")).enablePlugins(PlayScala, RiffRaffArtifact, JDebPackaging, SystemdPlugin, GatlingPlugin)

riffRaffArtifactResources := Seq(
  (packageBin in Debian).value -> s"${name.value}/${name.value}.deb",
  baseDirectory.value / "riff-raff.yaml" -> "riff-raff.yaml",
  baseDirectory.value / "typerighter.cfn.yaml" -> "cloudformation/typerighter.cfn.yaml"
)

javaOptions in Universal ++= Seq(
  s"-Dpidfile.path=/dev/null",
  "-J-XX:MaxRAMFraction=2",
  "-J-XX:InitialRAMFraction=2",
  "-J-XX:MaxMetaspaceSize=300m",
  "-J-XX:+PrintGCDetails",
  "-J-XX:+PrintGCDateStamps",
  s"-J-Dlogs.home=/var/log/${packageName.value}",
  s"-J-Xloggc:/var/log/${packageName.value}/gc.log",
  "-Dconfig.file=/etc/gu/typerighter.conf"
)

val languageToolVersion = "4.3"

resolvers += "Spring IO" at "https://repo.spring.io/plugins-release/"

libraryDependencies ++= Seq(
  "org.languagetool" % "languagetool-core" % languageToolVersion,
  "org.languagetool" % "language-en" % languageToolVersion,
  "org.scalatestplus.play" %% "scalatestplus-play" % "3.1.2" % Test
)

scalaVersion := "2.12.8"
scalacOptions := Seq(
  "-encoding", "UTF-8", "-target:jvm-1.8", "-deprecation",
  "-feature", "-unchecked", "-language:implicitConversions", "-language:postfixOps")
libraryDependencies += "io.gatling.highcharts" % "gatling-charts-highcharts" % "3.0.2" % "test,it"
libraryDependencies += "io.gatling"            % "gatling-test-framework"    % "3.0.2" % "test,it"