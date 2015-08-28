import AssemblyKeys._

name := "scalapderiv"

organization := "com.github.luzhuomi"

version := "0.0.1"

resolvers += "Typesafe Repository" at "http://repo.typesafe.com/typesafe/releases/"

resolvers += "Maven Repository" at "http://mvnrepository.com/artifact/"

resolvers += "OSS Sonatype" at "https://oss.sonatype.org/content/repositories/snapshots"

resolvers += "Local Ivy Repository" at "file://"+Path.userHome.absolutePath+"/.ivy2/local"


libraryDependencies += "org.scalaz" %% "scalaz-core" % "7.0.4" // scala license

libraryDependencies += "com.github.luzhuomi" %% "scalazparsec" % "0.1.1"  // apache license

seq(assemblySettings: _*)


mergeStrategy in assembly <<= (mergeStrategy in assembly) { (old) =>
  {
    case PathList("log4j.properties") => MergeStrategy.discard
    case PathList("META-INF", xs @ _*) => MergeStrategy.discard
    case _ => MergeStrategy.last // leiningen build files
  }
}
