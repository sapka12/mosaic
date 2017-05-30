package mosaic

import java.awt.image.BufferedImage

import com.sksamuel.scrimage.{Color, Image, ImageMetadata}
import mosaic.Mosaic.Tile

import scala.util.Random

object ThumbnailMaker {

  def thumbnail(image: Image, size: Int): Image = {
    val horizontal = image.width > image.height
    val remaining = (image.width + image.height - 2 * image.width.min(image.height)) / 2

    def cut(horizontal: Boolean) = if (horizontal) remaining else 0

    val cutX = cut(horizontal)
    val cutY = cut(!horizontal)

    image
      .trim(cutX, cutY, cutX, cutY)
      .scaleTo(size, size)
  }

  def rgb(image: Image): Color = {
    val colors = for {
      x <- 0 until image.width
      y <- 0 until image.height
    } yield image.color(x, y)

    def avg(prevAvg: Double, count: Int, newVal: Double): Double = (prevAvg * count + newVal) / (count + 1)

    colors
      .map(c => (c.red.toDouble, c.green.toDouble, c.blue.toDouble))
      .foldLeft[Option[(Int, (Double, Double, Double))]](None)(
      (b, a) => {
        b match {
          case None => Some((1, a))
          case Some((count, (r, g, b))) => Some((count + 1,
            (
              avg(r, count, a._1), avg(g, count, a._2), avg(b, count, a._3)
            )
          ))
        }
      }
    )
      .map(_._2)
      .map(c => (c._1.toInt, c._2.toInt, c._3.toInt))
      .map(c => Color(c._1, c._2, c._3))
      .getOrElse(Color.Black)
  }

  def segment(tileSize: Int, image: Image): Option[List[Tile]] = {

    val maxPos: Option[(Int, Int)] = for {
      img <- Some(image)
      if (img.width % tileSize == 0)
      if (img.height % tileSize == 0)
    } yield (img.width / tileSize, img.height / tileSize)

    println(s"maxpos: $maxPos")

    val tiles: Option[(List[((Int, Int), Image)])] = maxPos.map(xy => {

      println(s"xy: $xy")

      val subImages = for {
        x <- 0 until  xy._1
        y <- 0 until  xy._2
      } yield ((x, y), image.subimage(x * tileSize, y * tileSize, tileSize, tileSize))

      subImages.toList
    })

    tiles.map(_.map(x => Tile(x._1, x._2)))
    //      .map(randomize)
  }

  def randomize[A](as: List[A]): List[A] = as.foldLeft(
    List.empty[A])((actualList, a) => {
      val split = actualList.splitAt(Random.nextInt(actualList.size))
      split._1 ::: List(a) ::: split._2
    })

  //TODO remove tile duplication
  def bestMatchStrategy(album: List[Image])(img: Image): Image = {

    def sq(i: Int): Double = i.toDouble * i

    def diff(c1: Color, c2: Color): Double = {
      sq(c1.red - c2.red)
      +sq(c1.green - c2.green)
      +sq(c1.blue - c2.blue)
    }

    def closerAvgColor(i1: Image, i2: Image): Image = {
      val rgbBaseImg = rgb(img)

      if (diff(rgbBaseImg, rgb(i1)) < diff(rgbBaseImg, rgb(i2))) i1
      else i2
    }

    album.foldLeft(img)(closerAvgColor)
  }

  def change(tiles: List[Tile], strategy: Image => Image): List[Tile] =
    tiles.map(tile => (tile.copy(image = strategy(tile.image))))

  def bestMatchChange(input: Image, tileSize: Int, thumbnails: List[Image]): List[Tile] =
    segment(tileSize, input)
      .map(tiles => change(tiles, bestMatchStrategy(thumbnails)))
      .getOrElse(List())

  def desegment(tiles: List[Tile]): Image = {
    println("desegmentation")
    val maxHeight = tiles.map(t => (1 + t.image.height) * t.position._1).max
    val maxWidth = tiles.map(t => (1 + t.image.width) * t.position._2).max

    val baseImage = new Image(new BufferedImage(maxWidth, maxHeight, BufferedImage.TYPE_INT_ARGB), ImageMetadata(List()))

    tiles.foldLeft(baseImage)((image, tile) => {
      image.underlay(
        tile.image,
        tile.position._1 * tile.image.width,
        tile.position._2 * tile.image.height
      )
    })
  }
}