package org.dbpedia.extraction.mappings

import org.dbpedia.extraction.destinations.{DBpediaDatasets, Quad}
import org.dbpedia.extraction.wikiparser.impl.wikipedia.Namespaces
import org.dbpedia.extraction.ontology.Ontology
import org.dbpedia.extraction.util.{Language, ExtractorUtils}
import org.dbpedia.extraction.wikiparser._
import scala.language.reflectiveCalls

/**
 * Extracts Image files from wikipedia page and links to commons image files.
 */
class CommonsFileExtractor( context : {
                                      def ontology : Ontology
                                      def language : Language } ) extends PageNodeExtractor
{
    private val wikiPageCommonsFileProperty = context.ontology.properties("wikiPageCommonsFile")

    override val datasets = Set(DBpediaDatasets.CommonsFiles)

    override def extract(node : PageNode, subjectUri : String, pageContext : PageContext) : Seq[Quad] =
    {
        if(node.title.namespace != Namespace.Main && !ExtractorUtils.titleContainsCommonsMetadata(node.title)) 
            return Seq.empty

        val links = collectFileLinks(node).filter(isFileForArticle(_))

      links.map(link => new Quad(context.language, DBpediaDatasets.CommonsFiles, subjectUri, wikiPageCommonsFileProperty, getUri(link.destination), link.sourceUri))
    }


    private def isFileForArticle(linkNode : InternalLinkNode) = linkNode.destinationNodes match
    {
        case TextNode(text, _) :: Nil  => !text.startsWith(":")  // links starting wih ':' are actually only related, not the category of this article
        case _ => true
    }

    private def collectFileLinks(node : Node) : List[InternalLinkNode] =
    {
        node match
        {
            case linkNode : InternalLinkNode if linkNode.destination.namespace == Namespace.File => List(linkNode)
            case _ => node.children.flatMap(collectFileLinks)
        }
    }

    private def getUri(destination : WikiTitle) : String =
    {
        val fileNamespace = Namespace.File.name(context.language)
        context.language.commonsResourceUri.append(fileNamespace+':'+destination.decoded)


    }

}
