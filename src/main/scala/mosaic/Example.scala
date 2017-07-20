import java.io.{File, PrintWriter}

import mosaic.ThumbnailMaker._
import com.sksamuel.scrimage.Image

import scala.io.Source

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
    val masterPicture = Image.fromFile(new File(args(1)))
    val outputPicture = args(2)
    val tileSize = args(3).toInt
    val indexFile = new File(args(4))

    val thumbnails: List[Image] = Source.fromFile(indexFile).getLines.toList
      .map(line => Image.fromFile(new File(line.split(",").head)))

    val segments = bestMatchChange(masterPicture, tileSize, thumbnails)

    val out = desegmentation(segments)

    out.output(new File(outputPicture))
  }

  private def tiling(args: Array[String]): Unit = {
    val thumbnailSize = args(1).toInt
    val inputFolder = new File(args(2))
    val outputFolderPath = args(3)

    println(s"thumbnailSize: $thumbnailSize")
    println(s"inputFolder: $inputFolder")
    println(s"outputFolderPath: $outputFolderPath")

    inputFolder.listFiles
      .filter(_.isFile)
      .filter(file => extensions.exists(file.getName.toLowerCase.endsWith(_)))
      .foreach(f => {
        val inputPath = f.getAbsolutePath
        val outputPath = outputFolderPath + f.getName

        println(outputPath)

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
      .map(row => s"${row._1},${row._2.red},${row._2.green},${row._2.blue}")
      .mkString("\n")

    new PrintWriter(outputFilePath) {
      write(output);
      close
    }
  }

}
