package frontend
package files

import frontend.lexer.{Position, Region}

import java.io
import java.io.File
import scala.io.Source
import scala.jdk.CollectionConverters.*

sealed trait File {
    def content: String

    final def lines(): Iterator[String] = {
        content.lines().iterator().asScala.to(Iterator)
    }

    final def startRegion(): Region = {
        Region(this, Position(0, 0), Position(0, 1))
    }

    def URIString(): Option[String]
}

case class PhysicalFile(content: String, file: java.io.File) extends File {
    def URIString(): Option[String] = Some("file:///"+file.getAbsolutePath.replace("\\", "/"))
}

case class VirtualFile(content: String, name: String) extends File {
    def URIString(): Option[String] = Some(name)
}

case class ResourceFile(content: String, path: String) extends File {
    def URIString(): Option[String] = {
        toPhysical.URIString()
    }
    
    private def toPhysical: PhysicalFile = {
        PhysicalFile(content, io.File("src/main/resources/" + path))
    } 
}

//TODO error handling
def fileFromResource(path: String): File = {
    val source: String = Source.fromResource(path).mkString
    ResourceFile(source, path)
}

//TODO error handling
def fileFromPath(path: String): File = {
    val file = io.File(path)
    val bufferedSource = Source.fromFile(file)
    val source: String = bufferedSource.mkString
    bufferedSource.close()
    PhysicalFile(source, file)
}