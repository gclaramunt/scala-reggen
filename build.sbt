name := "regular generics"

organization := "org.gclaramunt"

scalaVersion := "2.12.0"

scalacOptions ++= Seq("-unchecked", "-deprecation", "-feature" )

scalacOptions += "-language:higherKinds" //hell, yeah!


// kind projector: makes type lambdas nicer to use
// see https://github.com/non/kind-projector
resolvers += Resolver.sonatypeRepo("releases")

addCompilerPlugin("org.spire-math" %% "kind-projector" % "0.9.3")

maxErrors := 5

triggeredMessage := Watched.clearWhenTriggered
