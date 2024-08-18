val zioVersion = "2.0.22"

lazy val root = project
  .in(file("."))
  .settings(
    name := "zlox",
    version := "0.0.1",
    scalaVersion := "3.4.1",
    libraryDependencies ++= Seq(
      "dev.zio" %% "zio"               % zioVersion,
      "dev.zio" %% "zio-test"          % zioVersion % Test,
      "dev.zio" %% "zio-test-sbt"      % zioVersion % Test,
      "dev.zio" %% "zio-test-junit"    % zioVersion % Test,
      "dev.zio" %% "zio-test-magnolia" % zioVersion % Test,
      "com.lihaoyi" %% "pprint" % "0.9.0"
    ),
    testFrameworks := Seq(new TestFramework("zio.test.sbt.ZTestFramework"))
  )