package com.htmlism

import cats.implicits._
import java.io._
import mouse.any._

object RecurseRepo extends App {
  val base = args.head

  val all = recursiveListFiles(new File(base))

  val typeClassPattern = """ ([A-Z][a-z]+)\[""".r

  val banList = Set(
    "Int", "String", "Option", "List", "Either", "Boolean", "Arbitrary", "Some", "Unit", "Right", "Left", "None", "Vector", "Long")

  val hitList = Set("Semigroup", "Monoid", "Functor", "Applicative", "Monad", "Apply", "FlatMap", "Traverse")

  all
    .map(contentsOf)
    .map(allTypeClassesFrom)
    .reduce(_ |+| _)
    .toList
    .foreach { case (s, n) => println(s"1 $s") }

  def recursiveListFiles(f: File): List[File] = {
    val these = f
      .listFiles
      .toList
      .filter(_.getName.endsWith(".scala"))

    these ++ f.listFiles.filter(_.isDirectory)
      .flatMap(recursiveListFiles)
  }

  def contentsOf(f: File): String = {
    val reader = new java.io.BufferedReader(new FileReader(f))

    val s = Iterator.continually(reader.readLine()).takeWhile(_ != null).mkString("\n")

    reader.close()

    s
  }

  def allTypeClassesFrom(s: String): Map[String, Int] =
    typeClassPattern
      .findAllIn(s)
      .toList
      .map(_.replace("[", ""))
      .map(s => Map(s -> 1))
      .foldLeft(Map[String, Int]())(_ |+| _)
}
