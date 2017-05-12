import java.io.File
import com.sksamuel.scrimage.Image
import ThumbnailMaker._

object Example {
  def main(args: Array[String]): Unit = {
    val thumbnailSize = args(0).toInt
    val inputFolder = new File(args(1))
    val outputFolderPath = args(2)

    inputFolder
      .listFiles()
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
}
