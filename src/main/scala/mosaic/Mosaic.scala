package mosaic

import com.sksamuel.scrimage.{Color, Image}
import mosaic.Mosaic.{ChangedTile, Tile}

object Mosaic {
  case class Tile(position: (Int, Int), image: Image)
  case class ChangedTile(position: (Int, Int), before: Image, after: Image)
}

trait Mosaic {

  def tiling(bigPictures: Set[Image], tileSize: Int): Set[Image]

  def indexing(images: Set[Image]): Set[(Image, Color)]

  def segmentation(input: Image, tileSize: Int): Option[List[Tile]]

  def change(tiles: List[Tile], palette: Set[(Image, Color)]): List[ChangedTile]

  def desegmentation(tiles: List[Tile]): Image

  def build(input: Image, tileSize: Int, bigPictures: Set[Image]): Option[Image] = {
    val thumbnails = indexing(tiling(bigPictures, tileSize))

    for {
      segmented <- segmentation(input, tileSize)
    } yield desegmentation(
      change(segmented, thumbnails)
        .map(f => Tile(f.position, f.after))
    )
  }
}

object MosaicBuilder extends Mosaic {
  import ThumbnailMaker._

  override def tiling(bigPictures: Set[Image], tileSize: Int): Set[Image] = bigPictures
    .map(thumbnail(_, tileSize))

  override def indexing(images: Set[Image]): Set[(Image, Color)] = images
    .map(image => (image, rgb(image)))

  override def segmentation(input: Image, tileSize: Int): Option[List[Tile]] =
    segment(tileSize, input)

  override def change(tiles: List[Tile], palette: Set[(Image, Color)]): List[ChangedTile] =
    tiles.map(tile => ChangedTile(
      tile.position,
      tile.image,
      bestMatchStrategy(palette.map(_ match {
        case (img, _) => img
      }).toList)(tile.image)
    ))

  override def desegmentation(tiles: List[Tile]): Image = desegment(tiles)
}
