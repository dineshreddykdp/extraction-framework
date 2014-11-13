package org.dbpedia.extraction.mappings

import java.io.BufferedReader

import org.dbpedia.extraction.destinations.DBpediaDatasets
import org.dbpedia.extraction.destinations.Quad
import org.dbpedia.extraction.wikiparser._
import org.dbpedia.extraction.ontology.Ontology
import org.dbpedia.extraction.util.{Language, UriUtils, ExtractorUtils}
import scala.collection.mutable.ArrayBuffer
import scala.language.reflectiveCalls


/**
 * Linking Wikipedia pages to Wikimedia Commons pages
 */
class CommonsLinksExtractor (
                            context : {
                              def ontology : Ontology
                              def language : Language
                            }
                            )
  extends PageNodeExtractor
{
  val wikiPageCommonsLinkProperty = context.ontology.properties("wikiPageCommonsLink")

  override val datasets = Set(DBpediaDatasets.CommonsLinks)



  override def extract(node : PageNode, subjectUri : String, pageContext : PageContext) : Seq[Quad] =
  {
    if(node.title.namespace != Namespace.Main && !ExtractorUtils.titleContainsCommonsMetadata(node.title))
      return Seq.empty


    var quads = new ArrayBuffer[Quad]()

    for (link <- collectCommonsLinks(node)) {


      if(link.toString.contains("TemplateNode(title=Commons category;") || link.toString.contains("TemplateNode(title=Commonscat;")
      || link.toString.contains("TemplateNode(title=Commons cat;") ){

        println(link)
        val childNode = link.children

        if(childNode.size > 0) for(child <- link.children ) {

          val childMap = child

          for(ch <- childMap.children) {

            //consider only first property node for commons template
            if(childMap.key.equals("1")) {

              val commonsresource = ch.toWikiText.replaceAll(" ","_")

              val uri = "http://commons.dbpedia.org/resource/Category:"+commonsresource
              val links = collectCommonsLinks(node)

              links.map(link => new Quad(context.language, DBpediaDatasets.CommonsLinks, subjectUri, wikiPageCommonsLinkProperty, uri, link.sourceUri))
            }
          }
        }

      }

    }

    quads
  }

  private def collectCommonsLinks(node : Node) : List[TemplateNode] =
  {

    node match
    {
      case linkNode : TemplateNode => List(linkNode)
      case _ => node.children.flatMap(collectCommonsLinks)
    }
  }

  private def getUri(destination : uri) : String =
  {
    val fileNamespace = Namespace.Category.name(context.language)
    context.language.commonsResourceUri.append(fileNamespace)


  }
}