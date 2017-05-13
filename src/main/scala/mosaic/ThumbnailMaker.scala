import com.sksamuel.scrimage.{Color, Image}

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
}