import java.io.{File, PrintWriter}

import mosaic.ThumbnailMaker._
import com.sksamuel.scrimage.{Color, Image}
import org.apache.commons.io.FileUtils

import scala.io.Source
import scala.util.{Random, Try}

object Example {

  val extensions = List("jpg", "jpeg").map("." + _)

  def main(args: Array[String]): Unit = {

    args(0) match {
      case "tiling" => tiling(args)
      case "indexing" => indexing(args)
      case "assemble" => assemble(args)
    }
  }

  private def assemble(args: Array[String]): Unit = {
    val tileSize = args(3).toInt
    val masterPicture = inputImage(args(1), tileSize)
    val outputPicture = args(2)
    val indexFile = new File(args(4))
    val alpha = Try(args(5).toDouble).toOption.getOrElse(0.0)

    implicit val thumbnails: List[IndexedImage] = Source
      .fromFile(indexFile)
      .getLines
      .toList
      .map(line => {
        val data = line.split(",")
        IndexedImage(data(0), Color(data(1).toInt, data(2).toInt, data(3).toInt))
      })

    val segmented = Random.shuffle(segmentation(tileSize, masterPicture))
    val changed = flipTiles(segmented, alpha)
    val mosaic = desegmentation(changed)

    mosaic.output(new File(outputPicture))
  }

  private def tiling(args: Array[String]): Unit = {
    val thumbnailSize = args(1).toInt
    val inputFolder = new File(args(2))
    val outputFolderPath = args(3)

    println(s"thumbnailSize: $thumbnailSize")
    println(s"inputFolder: $inputFolder")
    println(s"outputFolderPath: $outputFolderPath")

    FileUtils.forceMkdir(new File((outputFolderPath)))

    inputFolder.listFiles
      .filter(_.isFile)
      .filter(file => extensions.exists(file.getName.toLowerCase.endsWith(_)))
      .foreach(f => {
        val inputPath = f.getAbsolutePath
        val outputPath = outputFolderPath + f.getName

        thumbnail(
          Image.fromFile(new File(inputPath)),
          thumbnailSize
        ).output(outputPath)
      })
  }

  private def indexing(args: Array[String]) = {
    val tileFolderPath = args(1)
    val outputFilePath = args(2)

    println(s"tileFolderPath: $tileFolderPath")
    println(s"outputFilePath: $outputFilePath")

    val csvRows = new File(tileFolderPath)
      .listFiles()
      .map(_.getAbsolutePath)
      .map(path => (path, rgb(Image.fromFile(new File(path)))))

    val output = csvRows
      .map(_ match {
        case (path, color) =>
          s"${path},${color.red},${color.green},${color.blue}"
      })
      .mkString("\n")

    new PrintWriter(outputFilePath) {
      write(output);
      close
    }
  }

}
