package com.github.sonenko.sfile

import org.specs2.mutable.Specification
import scala.concurrent.duration._
import scala.concurrent.Await
import scala.language.postfixOps


class SfileTest  extends Specification {
  sequential

  val timeout = Duration(1000, MILLISECONDS)
  val readdir = "src/test/resources/readdir"
  val mvdir = "src/test/resources/mvdir"

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
      def res = Await.result(Sfile.mv("unknown_folder", "unknown_folder"), timeout)
      res must throwA[Exception]
    }

    "move files" in {
      val from = s"$mvdir/a1.txt"
      val to = s"$mvdir/a2.txt"
      val toName = "a2.txt"
      Await.result(Sfile.mv(from, to), timeout) mustEqual true
      Await.result(Sfile.dir(to), timeout).name mustEqual toName
      Await.result(Sfile.mv(to, from), timeout) mustEqual true
    }

    "move directories" in {
      val from = s"$mvdir/a1"
      val to = s"$mvdir/a2"
      val innerFileName = "b1.txt"
      val innerFile = s"$to/$innerFileName"
      Await.result(Sfile.mv(from, to), timeout) mustEqual true
      Await.result(Sfile.dir(innerFile), timeout).name mustEqual innerFileName
      Await.result(Sfile.mv(to, from), timeout) mustEqual true
    }
  }
}



























