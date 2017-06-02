import java.io.File

import akka.actor.ActorSystem
import com.sksamuel.scrimage.Image
import mosaic.MosaicBuilder

import scala.util.Try

object Example {

  val extensions = List("jpg", "jpeg").map("." + _)

  case class Input(inputImagePath: String, tileSize: Int, tileFolderPath: String, outputImagePath: String)

  case class MosaicInput(inputImage: Image, tileSize: Int, bigPictures: Set[Image], outputFile: File)

  def inputs(args: Array[String]): Option[Input] = Try {
    Input(
      inputImagePath = args(0),
      tileSize = args(1).toInt,
      tileFolderPath = args(2),
      outputImagePath = args(3)
    )
  }.toOption

  def toMosaicInput(input: Input): Option[MosaicInput] = Try {
    val inputImage: Image = Image.fromFile(new File(input.inputImagePath))
    val bigPictures: Set[Image] = new File(input.tileFolderPath)
      .listFiles
      .filter(_.isFile)
      .filter(file => extensions.exists(file.getName.toLowerCase.endsWith(_)))
      .map(Image.fromFile(_))
      .toSet
    val outputFile: File = new File(input.outputImagePath)

    MosaicInput(inputImage, input.tileSize, bigPictures, outputFile)
  }.toOption

  def main(args: Array[String]): Unit =
  {
   val inputs =  inputs(args)

    val system = ActorSystem("mosaic")


  }


}
