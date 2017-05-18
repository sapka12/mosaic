import java.io.{File, PrintWriter}

import mosaic.ThumbnailMaker._
import com.sksamuel.scrimage.Image

object Example {

  val extensions = List("jpg", "jpeg").map("." + _)

  def main(args: Array[String]): Unit = {

    args(0) match {
      case "tiling" => tiling(args)
      case "indexing" => indexing(args)
    }
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

    val output = csvRows.map(row => s"${row._1},${row._2.red},${row._2.green},${row._2.blue}").mkString("\n")

    new PrintWriter(outputFilePath) {
      write(output);
      close
    }
  }

}
