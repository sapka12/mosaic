package mosaic

import java.awt.image.BufferedImage
import java.io.File

import com.sksamuel.scrimage.{Color, Image, ImageMetadata, Pixel}

import scala.util.Random

object ThumbnailMaker {

  case class IndexedImage(path: String, color: Color) {
    lazy val image = Image.fromFile(new File(path))
  }

  case class Tile(position: (Int, Int), image: Image)

  case class ConsumeState(tilesToConsume: List[Tile],
                          consumedTiles: List[Tile],
                          thumbs: List[IndexedImage])

  def thumbnail(image: Image, size: Int): Image = {
    val horizontal = image.width > image.height
    val remaining = (image.width + image.height - 2 * image.width.min(
      image.height)) / 2

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

    val (r, g, b) = colors
      .map(c => (c.red.toDouble, c.green.toDouble, c.blue.toDouble))
      .unzip3

    def avg(colorComponents: Seq[Double]): Int =
      (colorComponents.sum / colors.size).toInt

    Color(avg(r), avg(g), avg(b))
  }

  def pixelize(image: Image): Seq[Seq[Color]] = {

    for {
      x <- 0 until image.width
    } yield
      for {
        y <- 0 until image.height
      } yield image.pixel(x, y).toColor
  }

  def segmentation(tileSize: Int, image: Image): List[Tile] = {
    println(s"input image: $image")
    segmentation(tileSize, pixelize(image))
  }

  def segmentation(tileSize: Int, image: Seq[Seq[Color]]): List[Tile] = {

    def imageOf(posX: Int, posY: Int): Image = {

      val pixels = for {
        x <- posX until (posX + tileSize)
        y <- posY until (posY + tileSize)
      } yield Pixel(image(x)(y))

      Image(tileSize, tileSize, pixels.toArray)
    }

    val positions = for {
      x <- 0 until image.size
      y <- 0 until image.head.size
      if (x % tileSize == 0)
      if (y % tileSize == 0)

      if (x + tileSize <= image.size)
      if (y + tileSize <= image(x).size)

    } yield Tile((x / tileSize, y / tileSize), imageOf(x, y))

    println(s"number of tiles: ${positions.size}")
    positions.toList
  }

  def randomize[A](as: List[A]): List[A] = {
    def go(list: List[A], aggr: List[A]): List[A] =
      if (list.isEmpty) aggr
      else {
        val randomIdx = Random.nextInt(list.size)
        go(
          list.take(randomIdx) ::: list.drop(randomIdx + 1),
          aggr ::: List(list(randomIdx))
        )
      }

    go(as, List())
  }

  //TODO remove tile duplication
  def bestMatchStrategy(album: List[IndexedImage])(tile: Image): IndexedImage = {

    def sq(i: Int): Double = i.toDouble * i

    def diff(c1: Color, c2: Color): Double = {
      sq(c1.red - c2.red)
      +sq(c1.green - c2.green)
      +sq(c1.blue - c2.blue)
    }

    val rgbTile = rgb(tile)

    album.foldLeft(album.head)((a, b) => {
      if (diff(rgbTile, a.color) < diff(rgbTile, b.color)) a
      else b
    })
  }

  def change(tiles: List[Tile], strategy: Image => Image): List[Tile] =
    tiles.map(i => i.copy(image = strategy(i.image)))

  def desegmentation(tiles: List[Tile]): Image = {

    println(s"tiles: ${tiles.size}")

    val maxWidth = tiles
      .map(_ match {
        case Tile((x, _), img) => img.width * (x + 1)
      })
      .max

    val maxHeight = tiles
      .map(_ match {
        case Tile((_, y), img) => img.height * (y + 1)
      })
      .max

    println(s"maxWidth: $maxWidth")
    println(s"maxHeight: $maxHeight")

    val baseImage = new Image(
      new BufferedImage(maxWidth, maxHeight, BufferedImage.TYPE_INT_ARGB),
      ImageMetadata(List()))

    //TODO Referential transparency?
    tiles.foreach {
      _ match {
        case Tile((tilePosX, tilePosY), img) =>
          for {
            x <- 0 until img.width
            y <- 0 until img.height
          } yield {
            baseImage.setPixel(
              tilePosX * img.width + x,
              tilePosY * img.height + y,
              img.pixel(x, y)
            )
          }
      }
    }

    baseImage
  }

  def flipTiles(in: List[Tile])(
      implicit allThumbnails: List[IndexedImage]): List[Tile] = {

    def nextState(cs: ConsumeState)(
      implicit allThumbnails: List[IndexedImage]): ConsumeState = cs match {
      case ConsumeState(tilesToConsume, consumedTiles, thumbs) =>
        val consuming = tilesToConsume.head
        val bestMatch = bestMatchStrategy(thumbs)(consuming.image)
        val remainingThumbnails = thumbs.filterNot(_ == bestMatch) match {
          case List() => allThumbnails
          case remaining: List[IndexedImage] => remaining
        }

        println(s"to consume: ${tilesToConsume.size}, consumed: ${consumedTiles.size}, thumbnails: ${thumbs.size}")

        ConsumeState(
          tilesToConsume.tail,
          consuming.copy(image = bestMatch.image) :: consumedTiles,
          remainingThumbnails
        )
    }

    def go(cs: ConsumeState): ConsumeState =
      if (cs.tilesToConsume.isEmpty) cs
      else {
        go(nextState(cs))
      }

    go(ConsumeState(in, List(), allThumbnails)).consumedTiles
  }

  def inputImage(filepath: String, tileSize: Int) = {
    val in = Image.fromFile(new File(filepath))

    val w = in.width - in.width % tileSize
    val h = in.height - in.height % tileSize

    in.resizeTo(w, h)
  }

}
