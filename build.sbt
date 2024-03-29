lazy val root = (project in file(".")).
  settings(
    name := "kaggle-bimbo",
    organization := "com.github.danielkorzekwa",
    version := "0.1-SNAPSHOT",
    scalaVersion := "2.11.4",
    scalacOptions ++= Seq(
      "-feature",
      "-deprecation",
      "-encoding", "UTF-8",       // yes, this is 2 args
      "-unchecked",
      "-Xfuture"
      //"-Ywarn-unused-import"     // 2.11 only
    ),
    
    // Include only src/main/scala in the compile configuration
    unmanagedSourceDirectories in Compile := (scalaSource in Compile).value :: Nil,

    // Include only src/test/scala in the test configuration
    unmanagedSourceDirectories in Test := (scalaSource in Test).value :: Nil,
    
    libraryDependencies ++= Seq(
      "com.github.danielkorzekwa" %% "bayes-scala-gp" % "0.1-SNAPSHOT", 
      "org.scalanlp" %% "breeze" % "0.12",
      "com.typesafe.scala-logging" %% "scala-logging-slf4j" % "2.1.2",
      "joda-time" % "joda-time" % "2.7",
      "org.apache.spark" % "spark-core_2.11" % "1.6.1",
      "com.github.haifengl" % "smile-core" % "1.1.0",
      // test scoped
      "com.novocode" % "junit-interface" % "0.11" % Test
    ),
    
    resolvers ++= Seq(
  // other resolvers here
  // if you want to use snapshot builds (currently 0.12-SNAPSHOT), use this.
  "Sonatype Snapshots" at "https://oss.sonatype.org/content/repositories/snapshots/",
  "Sonatype Releases" at "https://oss.sonatype.org/content/repositories/releases/"
)
  )
