import AssemblyKeys._

import SonatypeKeys._

sonatypeSettings

name := "scalapderiv"

organization := "com.github.luzhuomi"

version := "0.0.6"


crossScalaVersions := Seq("2.9.2", "2.10.3", "2.11.3")

resolvers += "Typesafe Repository" at "http://repo.typesafe.com/typesafe/releases/"

resolvers += "Maven Repository" at "http://mvnrepository.com/artifact/"

resolvers += "OSS Sonatype" at "https://oss.sonatype.org/content/repositories/snapshots"

resolvers += "Kenny's github repo" at "http://github.com/luzhuomi/mavenrepo/raw/master"

// resolvers += "Local Ivy Repository" at "file://"+Path.userHome.absolutePath+"/.ivy2/local"


libraryDependencies += "org.scalaz" %% "scalaz-core" % "7.0.4" // scala license

libraryDependencies += "com.github.luzhuomi" %% "scalazparsec" % "0.1.2"  // apache license

seq(assemblySettings: _*)


mergeStrategy in assembly <<= (mergeStrategy in assembly) { (old) =>
  {
    case PathList("log4j.properties") => MergeStrategy.discard
    case PathList("META-INF", xs @ _*) => MergeStrategy.discard
    case _ => MergeStrategy.last // leiningen build files
  }
}

publishTo := Some(Resolver.file("mavenLocal",  new File(Path.userHome.absolutePath+"/git/mavenrepo/")))
