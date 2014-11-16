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
 * Extracts commons links from wikipedia pages.
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

  private val commonsTemplate = "TemplateNode(title=Commons"

  private val commonsCategoryTemplate = "TemplateNode(title=Commons category"

  private val emptyString = ""

  private val categorySuffix = "Category:"


  override val datasets = Set(DBpediaDatasets.CommonsLinks)

  override def extract(node : PageNode, subjectUri : String, pageContext : PageContext) : Seq[Quad] =
  {
    if(node.title.namespace != Namespace.Main && !ExtractorUtils.titleContainsCommonsMetadata(node.title))
      return Seq.empty

    var quads = new ArrayBuffer[Quad]()

    for (link <- collectCommonsLinks(node)) {

      if(link.toString.contains(commonsTemplate)) {

        var string = emptyString

        //If the template node is commons category add "Category:" to the commonsResourceURI
        if(link.toString.contains(commonsCategoryTemplate)) {

          string = categorySuffix
        }

        val propertyNodeList = link.children

        if(propertyNodeList.size > 0) for(propertyNode <- propertyNodeList ) {

          for(textNode <- propertyNode.children) {

            //Consider only first child of property node.  For example {{Commons|pagename|showname}}, this extracts only pagename and links to the wikipedia page
            if(propertyNode.key.equals("1")) {

              val destination = textNode.toWikiText.replaceAll(" ","_")

              val destinationUri = context.language.commonsResourceUri.append(string+destination)


              quads += new Quad(context.language, DBpediaDatasets.CommonsLinks, subjectUri, wikiPageCommonsLinkProperty, destinationUri, node.sourceUri, null)
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

}