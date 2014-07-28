package com.cloop.levelgen

import scala.util.Random

object Generator {
  type Dot = (Int, Int)

  object Field {
    def apply(width: Int, height: Int) = new Field((for (i <- 0 until width) yield new Array[Boolean](height)).toArray)
  }

  // direct transformation: * * . -> . . *
  // reverse transformation: . . * -> * * .
  type Transform = List[Dot]

  object Transformations {
    def id: Field => Field = { f => f }

    def available(f: Field) = {
      val candidates = f.leftRightPieces ++ f.upDownPieces
      val free: Dot => Boolean = f.isFree
      val occ: Dot => Boolean = f.isOccupied

      (candidates ++ candidates.map(_.reverse)).filter(checkPattern(_, List(free, free, occ)))
    }

    def randomTransform(f: Field): (Field, Transform, Boolean) =
      if (available(f).isEmpty) (f, Nil, false)
      else {
        val l = available(f).toList
        val positions = l(new Random().nextInt(l.length))
        (f.transform(positions.zip(List(true, true, false))), positions, true)
      }

    private def checkPattern(dots: List[Dot], checkers: List[Dot => Boolean]): Boolean = dots.zip(checkers).forall { case (d, f) => f(d)}

  }

  // it is assumed that field is a rectangle
  class Field(val dots: Array[Array[Boolean]]) extends Iterable[Dot] {

    val width = dots.length
    val height = dots(0).length

    def isInside(dot: Dot): Boolean = dot match {
      case (x, y) => x < width && x >= 0 && y <= height && y >= 0
    }

    def isOccupied(dot: Dot): Boolean = dot match {
      case (x, y) => dots(x)(y)
    }

    def isFree(dot: Dot) = !isOccupied(dot)

    override def iterator: Iterator[Dot] = (for (x <- 0 until width; y <- 0 until height) yield (x, y)).iterator

    lazy val randomXgen = new Random()
    lazy val randomYgen = new Random()

    def randomX = randomXgen.nextInt(width)

    def randomY = randomYgen.nextInt(height)

    def randomDot = (randomX, randomY)

    def upDown(base: Dot): List[Dot] = base match {
      case (x, y) => (x, y) ::(x, y + 1) ::(x, y + 2) :: Nil
    }

    def leftRight(base: Dot): List[Dot] = base match {
      case (x, y) => (x, y) ::(x + 1, y) ::(x + 2, y) :: Nil
    }

    def upDownPieces = for (x <- 0 until width; y <- 0 until height - 2) yield upDown(x, y)

    def leftRightPieces = for (x <- 0 until width - 2; y <- 0 until height) yield leftRight(x, y)

    def transform(todo: List[(Dot, Boolean)]): Field = {
      val newArray: Array[Array[Boolean]] = dots.map(_.clone)
      todo.foreach { case ((x, y), b) => newArray(x)(y) = b}
      new Field(newArray)
    }

    override def toString(): String = {
      val sb = new StringBuilder
      for (x <- 0 until width) {
        for (y <- 0 until height) {
          sb ++= " "
          sb ++= (if (isOccupied(x, y)) "1" else "0")
        }
        sb ++= "\n"
      }
      sb.toString()
    }

    def withRandomDot = transform(List((randomDot, true)))
  }

  def generateLevel(width: Int, height: Int, steps: Int): (List[Transform], Field) = {

    def loop(f: Field, stepsLeft: Int, history: List[Transform] = Nil): (List[Transform], Field) =
      Transformations.randomTransform(f) match {
        case (f, pos, true) if stepsLeft > 0 => loop(f, stepsLeft - 1, pos :: history)
        case (f, pos, false) => println(s"I can't generate it further, I still have $stepsLeft steps left."); (history, f)
        case (f, pos, true) if stepsLeft == 0 => (pos::history, f)

      }
    loop(Field(width, height).withRandomDot, steps-1)
  }

}