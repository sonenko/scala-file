package com.github.sonenko.sfile

import java.io
import java.io.FileNotFoundException

import scala.concurrent.{Await, Future}
import scala.concurrent.ExecutionContext.Implicits.global

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
        children.map{ ch =>
          Folder(f.getName, f.getAbsolutePath, ch)
        }
      }
    }

  def mv(from: String, to:String): Future[Boolean] = Future {
    val f = new java.io.File(from)
    val t = new java.io.File(to)
    if (f.exists()) f.renameTo(t)
    else throw new FileNotFoundException(from)
  }

  def touch(path: String): Future[Boolean] = ???

  def cp(from: String, to: String): Future[Boolean] = ???

  def rm(path: String): Future[Boolean] = ???

  def mkdir(path: String): Future[Boolean] = ???
}

