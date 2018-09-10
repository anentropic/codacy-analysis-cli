package com.codacy.analysis.core.files

import better.files.File
import org.specs2.control.NoLanguageFeatures
import org.specs2.mutable.Specification

import scala.util.{Failure, Success, Try}

class FallbackFileCollectorSpec extends Specification with NoLanguageFeatures {

  val failingCompanion = new FileCollectorCompanion[Try] {
    override def name: String = ""

    override def apply(): FileCollector[Try] = new FileCollector[Try] {
      override def list(directory: File): Try[FilesTarget] = {
        Failure(new Exception("because fail"))
      }
    }
  }

  val successfulCompanion = new FileCollectorCompanion[Try] {
    override def name: String = ""

    override def apply(): FileCollector[Try] = new FileCollector[Try] {
      override def list(directory: File): Try[FilesTarget] = {
        Success(FilesTarget(directory, Set.empty, Set.empty))
      }
    }
  }

  "FallbackFileCollectorSpec" should {
    "not fallback" in {
      new FallbackFileCollector(List(successfulCompanion, failingCompanion)).list(File("")) must beSuccessfulTry
    }

    "fallback" in {
      new FallbackFileCollector(List(failingCompanion, successfulCompanion)).list(File("")) must beSuccessfulTry
    }

    "fail when all fail" in {
      new FallbackFileCollector(List(failingCompanion, failingCompanion)).list(File("")) must beFailedTry
    }
  }
}
