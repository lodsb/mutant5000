name := "mutant5000"

organization := "org.lodsb"

version := "0.1-SNAPSHOT"

scalaVersion := "2.10.3"

scalacOptions ++= Seq("-unchecked", "-deprecation", "-Dfile.encoding=ISO-8859-1") //, "-Xprint:typer")

scalacOptions <++= scalaVersion map { version =>
  val Version = """(\d+)\.(\d+)\..*"""r
  val Version(major0, minor0) = version map identity
  val (major, minor) = (major0.toInt, minor0.toInt)
  if (major < 2 || (major == 2 && minor < 10)) 
  	Seq("-Ydependent-method-types")
 	else Nil
}

resolvers += "Typesafe Repository" at "http://repo.typesafe.com/typesafe/releases/"

libraryDependencies += "com.typesafe.akka" %% "akka-actor" % "2.2.0"

unmanagedClasspath in Compile += Attributed.blank(new java.io.File("doesnotexist"))


