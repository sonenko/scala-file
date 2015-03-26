package com.github.sonenko.sfile

import java.io.{FileOutputStream, FileInputStream, FileNotFoundException}
import java.nio.file.FileAlreadyExistsException
import scala.concurrent.Future
import scala.concurrent.ExecutionContext.Implicits.global
import scala.util.{Success, Failure, Try}


sealed trait Sfile{
  def name: String
  def path: String
}
case class File(name: String, path: String) extends Sfile
case class Folder(name: String, path: String, children: List[Sfile]) extends Sfile


object Sfile {

  def dir(path: String): Future[Sfile] =
    Future{new java.io.File(path)}.flatMap{ f =>
      if (!f.exists) throw new FileNotFoundException(path)
      else if (f.isFile) Future(File(f.getName, f.getAbsolutePath))
      else {
        val children: Future[List[Sfile]] = Future.sequence(f.listFiles.toList.map { c =>
          dir(c.getAbsolutePath)
        })
        children.map{ ch => Folder(f.getName, f.getAbsolutePath, ch)}
      }
    }

  def mv(from: String, to:String): Try[Boolean] = Try {
    val f = new java.io.File(from)
    val t = new java.io.File(to)
    if (f.exists()) f.renameTo(t)
    else throw new FileNotFoundException(from)
  }

  def touch(path: String): Try[Boolean] = Try{
    val f = new java.io.File(path)
    if (f.exists) throw new FileAlreadyExistsException(path)
    else f.createNewFile()
  }

  def rm(path: String): Try[Unit] = {
    val f = new java.io.File(path)
    if (!f.exists) Failure(new FileNotFoundException(path))
    else if (f.isFile) Try(f.delete())
    else {
      val results: List[Try[Unit]] = f.listFiles().toList.map { c =>
        rm(c.getAbsolutePath)
      } ::: Try{ f.delete(); () } :: Nil

      listTryToTry(results)
    }
  }

  def mkdir(path: String): Try[Boolean] = Try {
    val f = new java.io.File(path)
    if (f.exists) throw new FileAlreadyExistsException(path)
    else f.mkdir()
  }


  def cp(from: String, to: String): Try[Unit] = {
    val source = new java.io.File(from)
    val target = new java.io.File(from)
    if (!source.exists) Failure(new FileNotFoundException(from))
    else if (source.isFile) copyFile(source, target)
    else {
      val results = mkdir(to).map(_ => ()) ::
        source.listFiles().toList.map{x => cp(x.getAbsolutePath, s"${target.getAbsolutePath}/${x.getName}")}
      listTryToTry(results)
    }
  }

  // rewrite
  private def copyFile(sourceFile: java.io.File, destFile: java.io.File): Try[Unit] = Try {
    if(!destFile.exists()) {
      destFile.createNewFile()
    }
    new FileOutputStream(destFile).getChannel.transferFrom(new FileInputStream(sourceFile).getChannel, 0, Long.MaxValue )
  }

  private def listTryToTry(x: List[Try[Unit]]): Try[Unit] = Try{x.foreach(_.get)}
}








































