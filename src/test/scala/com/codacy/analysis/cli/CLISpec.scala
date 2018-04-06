package com.codacy.analysis.cli

import java.nio.file.{Path, Paths}

import better.files.File
import caseapp.RemainingArgs
import codacy.docker.api
import com.codacy.analysis.cli.command.ArgumentParsers._
import com.codacy.analysis.cli.command.{Command, CommandAppWithBaseCommand, DefaultCommand}
import com.codacy.analysis.cli.model.Result
import io.circe.generic.auto._
import io.circe.{Decoder, parser}
import org.specs2.control.NoLanguageFeatures
import org.specs2.mutable.Specification

import scala.sys.process._

class CLISpec extends Specification with NoLanguageFeatures {

  private val cli = new CommandAppWithBaseCommand[DefaultCommand, Command] {
    override def exit(code: Int): Unit = ()

    override final def run(command: Command, remainingArgs: RemainingArgs): Unit = {
      command.run()
    }

    override def defaultCommand(command: DefaultCommand, remainingArgs: Seq[String]): Unit = {
      if (command.version.isDefined) {
        command.run()
      } else {
        helpAsked()
      }
    }
  }

  implicit val categoryDencoder: Decoder[api.Pattern.Category.Value] = Decoder.enumDecoder(api.Pattern.Category)
  implicit val levelDencoder: Decoder[api.Result.Level.Value] = Decoder.enumDecoder(api.Result.Level)
  implicit val fileDencoder: Decoder[Path] = Decoder[String].map(Paths.get(_))

  "CLIApp" should {
    "parse correctly" in {
      cli.parse(Array()) must beRight
      cli.parse(Array("--version")) must beRight
      cli.parse(Array("analyse", "--directory", "/tmp", "--tool", "pylint")) must beRight
      cli.parse(Array("analyse", "--directory", "/tmp", "--tool", "pylint", "--output", "/tmp/test.txt")) must beRight
      cli.parse(Array("analyse", "--directory", "/tmp", "--tool", "pylint", "--verbose")) must beRight
      cli.parse(Array("analyse", "--directory", "/tmp", "--tool", "pylint", "--format", "json")) must beRight
    }

    "fail parse" in {
      cli.parse(Array("bad-command", "--directory", "/tmp", "--tool", "pylint")) must beEqualTo(
        Right(errorMsg("Command not found: bad-command")))
      cli.parse(Array("analyse", "--bad-parameter", "/tmp", "--tool", "pylint")) must beEqualTo(
        Right(errorMsg("Unrecognized argument: --bad-parameter")))
    }

    "output text to file" in {
      (for {
        file <- File.temporaryFile()
      } yield {
        cli.main(Array("analyse", "--directory", "/tmp", "--tool", "pylint", "--output", file.pathAsString))

        file.contentAsString must beEqualTo("""|Starting analysis ...
             |Analysis complete
             |""".stripMargin)
      }).get()
    }

    "output json to file" in {
      (for {
        file <- File.temporaryFile()
      } yield {
        cli.main(
          Array(
            "analyse",
            "--directory",
            "/tmp",
            "--tool",
            "pylint",
            "--format",
            "json",
            "--output",
            file.pathAsString))

        file.contentAsString must beEqualTo("""|[]
             |""".stripMargin)
      }).get()
    }

    "output correct issues for sample project without remote configuration" in {
      (for {
        directory <- File.temporaryDirectory()
        file <- File.temporaryFile()
      } yield {

        Process(Seq("git", "clone", "git://github.com/codacy/codacy-brakeman", directory.pathAsString)).!
        Process(Seq("git", "reset", "--hard", "b10790d724e5fd2ca98e8ba3711b6cb10d7f5e38"), directory.toJava).!

        cli.main(
          Array(
            "analyse",
            "--directory",
            directory./("src/main/resources/docs/directory-tests/rails4").pathAsString,
            "--tool",
            "brakeman",
            "--format",
            "json",
            "--output",
            file.pathAsString))

        val result = for {
          responseJson <- parser.parse(file.contentAsString)
          response <- responseJson.as[Set[Result]]
          expectedJson <- parser.parse(
            File.resource("com/codacy/analysis/cli/cli-output-brakeman-1.json").contentAsString)
          expected <- expectedJson.as[Set[Result]]
        } yield (response, expected)

        result must beRight
        result must beLike { case Right((response, expected)) => response must beEqualTo(expected) }
      }).get()
    }

  }

  private def errorMsg(message: String)
    : (DefaultCommand, List[String], Option[Either[String, (String, Command, Seq[String], Seq[String])]]) = {
    (DefaultCommand(None), List.empty[String], Some(Left(message)))
  }

}
