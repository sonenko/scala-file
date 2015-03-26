package com.github.sonenko.sfile

import java.io.FileNotFoundException
import java.nio.file.FileAlreadyExistsException

import org.specs2.mutable.Specification
import scala.concurrent.duration._
import scala.concurrent.Await
import scala.language.postfixOps


class SfileTest  extends Specification {
  sequential

  val timeout = Duration(1000, MILLISECONDS)
  val readdir = "src/test/resources/readdir"
  val modifying = "src/test/resources/modifying"

  def isFileExists(path: String): Boolean = new java.io.File(path).exists()

  "Sfile.dir" should {
    "return failed future in file/folder does not exists" in {
      def res = Await.result(Sfile.dir("unknown_folder"), timeout)
      res must throwA[Exception]
    }

    "return file by path" in {
      val fileName = "a1.txt"
      val res = Await.result(Sfile.dir(s"$readdir/$fileName"), timeout)
      res.name mustEqual fileName
    }

    "return folder by path" in {
      val res = Await.result(Sfile.dir(readdir), timeout)
      val rootDir = "readdir"
      val a1Dir = "a1"
      val a2Dir = "a2"
      val a1File = "a1.txt"
      val b1Dir = "b1"
      val b1File = "b1.txt"
      val c1File = "c1.txt"

      res match {
        case
          Folder(`rootDir`, _, List(
            Folder(`a1Dir`, _, List(
              Folder(`b1Dir`, _, List(
                File(`c1File`, _)
              )),
              File(`b1File`, _)
            )),
            File(`a1File`, _),
            Folder(`a2Dir`, _, Nil)
          )
          ) => success
        case x => failure(s"$x has unexpected structure")
      }
    }
  }

  "Sfile.mv" should {
    "hate if source file does not exists" in {
      Sfile.mv("unknown_folder", "unknown_folder") must beFailedTry
    }

    "move files" in {
      val from = s"$modifying/a1.txt"
      val to = s"$modifying/a2.txt"
      val toName = "a2.txt"
      Sfile.mv(from, to) must beSuccessfulTry
      isFileExists(to) must beTrue
      Sfile.mv(to, from) must beSuccessfulTry
    }

    "move directories" in {
      val from = s"$modifying/a1"
      val to = s"$modifying/a2"
      val innerFileName = "b1.txt"
      val innerFile = s"$to/$innerFileName"
      Sfile.mv(from, to) must beSuccessfulTry
      isFileExists(to) must beTrue
      Sfile.mv(to, from) must beSuccessfulTry
    }
  }

  "Sfile.touch" should {
    "hate if file already exists" in {
      val path = s"$modifying/a1.txt"
      Sfile.touch(path) must beFailedTry
    }
    "create new file" in {
      val path = s"$modifying/new.txt"
      Sfile.touch(path) must beSuccessfulTry
      isFileExists(path) must beTrue
      Sfile.rm(path) must beSuccessfulTry
    }
  }

  "Sfile.rm" should {
    "hate if no file" in {
      val path = s"$modifying/UNKNOWN.txt"
      Sfile.rm(path) must beFailedTry
    }
    "remove file" in {
      val path = s"$modifying/new.txt"
      Sfile.touch(path) must beSuccessfulTry
      Sfile.rm(path) must beSuccessfulTry
      isFileExists(path) must beFalse
    }
    "remove folder" in {
      val folder1 = s"$modifying/new-folder0"
      Sfile.mkdir(folder1) must beSuccessfulTry
      isFileExists(folder1) must beTrue

      Sfile.rm(folder1) must beSuccessfulTry
      isFileExists(folder1) must beFalse
    }
    "recursively remove folder" in {
      val folder1 = s"$modifying/new-folder1"
      val folder2 = s"$folder1/new-folder2"
      val fileInFolder2 = s"$folder2/new.txt"
      Sfile.mkdir(folder1)
      Sfile.mkdir(folder2)
      Sfile.touch(fileInFolder2)
      isFileExists(fileInFolder2) must beTrue

      Sfile.rm(folder1) must beSuccessfulTry
      isFileExists(folder1) must beFalse
    }
  }

}


















































