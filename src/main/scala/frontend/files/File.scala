package frontend
package files

import java.io.File
import scala.io.Source
import scala.jdk.CollectionConverters.*

sealed trait File {
    def content: String

    def lines(): Iterator[String] = {
        content.lines().iterator().asScala.to(Iterator)
    }

}

case class PhysicalFile(content: String, file: java.io.File) extends File

case class VirtualFile(content: String) extends File

case class ResourceFile(content: String, path: String) extends File

def fileFromResource(path: String): File = {
    val source: String = Source.fromResource(path).mkString
    ResourceFile(source, path)
}
