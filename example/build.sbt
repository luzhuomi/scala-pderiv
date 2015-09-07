name := "scalapderiv-examples"

organization := "com.github.luzhuomi"

version := "0.0.1"


resolvers += "Apache HBase" at "https://repository.apache.org/content/repositories/releases"

resolvers += "Typesafe Repository" at "http://repo.typesafe.com/typesafe/releases/"

resolvers += "Maven Repository" at "http://mvnrepository.com/artifact/"

resolvers += "OSS Sonatype" at "https://oss.sonatype.org/content/repositories/snapshots"


resolvers += "Local Ivy Repository" at "file://"+Path.userHome.absolutePath+"/.ivy2/local"



libraryDependencies += "com.github.luzhuomi" %% "scalapderiv" % "0.0.1"

