package mosaic

import com.sksamuel.scrimage.{Color, Image, Pixel}
import org.scalatest.FlatSpec

class ThumbnailMakerSpec extends FlatSpec {

  "pixelize" should "work :) " in {

    val img: Image = Image(3, 2, Array(
      Pixel(0, 0, 0, 0),
      Pixel(1, 0, 0, 0),
      Pixel(2, 0, 0, 0),
      Pixel(3, 0, 0, 0),
      Pixel(4, 0, 0, 0),
      Pixel(5, 0, 0, 0)
    ))

    val pixels = ThumbnailMaker.pixelize(img)

    assert(pixels(0)(0).toRGB.red === 0)
    assert(pixels(1)(0).toRGB.red === 1)
    assert(pixels(2)(0).toRGB.red === 2)
    assert(pixels(0)(1).toRGB.red === 3)
    assert(pixels(1)(1).toRGB.red === 4)
    assert(pixels(2)(1).toRGB.red === 5)
  }

  "segmentation" should "work :) " in {

    val img = Seq(
      Seq(
        Color(1, 0, 0, 0),
        Color(2, 0, 0, 0),
        Color(3, 0, 0, 0),
        Color(4, 0, 0, 0),
        Color(5, 0, 0, 0)
      ),
      Seq(
        Color(6, 0, 0, 0),
        Color(7, 0, 0, 0),
        Color(8, 0, 0, 0),
        Color(9, 0, 0, 0),
        Color(10, 0, 0, 0)
      )
    )

    val pixels = ThumbnailMaker.segmentation(2, img)

    assert(pixels.size == 2)

    assert(pixels(0).position === (0, 0))
    assert(pixels(0).image.width === 2)
    assert(pixels(0).image.height === 2)
    assert(pixels(0).image.pixel(0, 0).toColor.red === 1)
    assert(pixels(0).image.pixel(0, 1).toColor.red === 6)
    assert(pixels(0).image.pixel(1, 0).toColor.red === 2)
    assert(pixels(0).image.pixel(1, 1).toColor.red === 7)

    assert(pixels(1).position === (0, 1))
    assert(pixels(1).image.width === 2)
    assert(pixels(1).image.height === 2)
    assert(pixels(1).image.pixel(0, 0).toColor.red === 3)
    assert(pixels(1).image.pixel(0, 1).toColor.red === 8)
    assert(pixels(1).image.pixel(1, 0).toColor.red === 4)
    assert(pixels(1).image.pixel(1, 1).toColor.red === 9)


  }

}
