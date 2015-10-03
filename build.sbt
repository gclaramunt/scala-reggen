name := "regular generics"

organization := "org.gclaramunt"

scalaVersion := "2.11.5"

scalacOptions ++= Seq("-unchecked", "-deprecation", "-feature" )

scalacOptions += "-language:higherKinds" //hell, yeah!

maxErrors := 5

triggeredMessage := Watched.clearWhenTriggered
