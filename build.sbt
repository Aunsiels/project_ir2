scalaVersion := "2.11.7"

fork := true

javaOptions += "-Xmx8G"

libraryDependencies ++= Seq(
    "org.iq80.leveldb" % "leveldb-api" % "0.9",
    "org.fusesource.leveldbjni" % "leveldbjni-all" % "1.8",
    "tinyir" % "tinyir" % "1.1" from "http://www.da.inf.ethz.ch/files/ir2016/tinyir-1.1.jar"
)