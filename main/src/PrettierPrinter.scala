object PrettierPrinter {
  import scala.annotation.{tailrec}

  /** Implementation of Philip Wadler's "Prettier Printer"
    * See http://homepages.inf.ed.ac.uk/wadler/papers/prettier/prettier.pdf
    *
    * However the combinators given by Philip Wadler are not expressive enough
    * to allow the final layout we need for most languages. For example, literal
    * new lines in multiline strings & comments must be preserved.
    *
    * To allow to force the layout, we provide a NoFlat combinator that prevent
    * any flatten. To reflect that we change the signature of flatten to indicate
    * that some doc cannot be flattened. In this case the group combinator
    * does not have any effect and acts as identity.
    */
  sealed trait Doc {
    /** Concatenates this and the given document. */
    def <>(y: Doc): Doc = Concat(this, y)
    /** Concatenates this and the given document with a `space' in between. */
    def <+>(y: Doc): Doc = this <> Doc.space <> y
    /** Concatenates this and the given document with a `line' in between. */
    def </>(y: Doc): Doc = this <> Doc.line <> y
    /** Concatenates this and the given document with a `linebreak' in between. */
    def />(y: Doc): Doc = this <> Doc.linebreak <> y
    /** Concatenates this and the given document with a `hardline' in between. */
    def <|>(y: Doc): Doc = this <> Doc.noFlat <> Doc.line <> y

    private[PrettierPrinter] def flatten: Option[Doc] = this match {
      case NoFlat =>
        None
      case NilDoc | Text(_) =>
        Some(this)
      case Concat(x, y) =>
        for {
          x0 <- x.flatten
          y0 <- y.flatten
        } yield Concat(x0, y0)
      case Nest(i, x) =>
        x.flatten
      case Line(flattenTo) =>
        Some(if(flattenTo == "") NilDoc else Text(flattenTo))
      case Union(x, _) =>
        Some(x)
    }

    /** Pretty print this document with a page width `maxWidth' */
    def pretty(maxWidth: Int): String = {
      def best(k: Int, z: List[(Int,Doc)]): Chunk = z match {
        case Nil =>
          NilChunk
        case (i, NilDoc | NoFlat) :: z =>
          best(k, z)
        case (i, Concat(x, y)) :: z =>
          best(k, (i, x) :: (i, y) :: z)
        case (i, Nest(j, x)) :: z =>
          best(k, (i + j.getOrElse(k - i), x) :: z)
        case (i, Text(s)) :: z =>
          TextChunk(s, best(k + s.size, z))
        case (i, Line(_)) :: z =>
          LineChunk(i, best(i, z))
        case (i, Union(x, y)) :: z =>
          val x0 = best(k, (i, x) :: z)
          lazy val y0 = best(k, (i, y) :: z)
          if(Chunk.fits(maxWidth - k, x0)) x0 else y0
      }
      Chunk.layout(best(0, List(0 -> this)))
        .lines
        .map(_.replaceAll("""\s*$""", ""))
        .mkString("\n")
    }
  }

  object Doc {
    /** Empty document */
    val empty: Doc = NilDoc

    /** A new line flattened as space if possible */
    val line: Doc = Line(" ")

    /** A new line flattened as empty if possible */
    val linebreak: Doc = Line("")

    /** A document that cannot be flattened */
    val noFlat = NoFlat

    /** A new line never flattened */
    val hardline: Doc = noFlat <> linebreak

    /** A text fragment (cannot contain any new line) */
    def text(chars: String): Doc =
      if(chars.contains('\n'))
        sys.error(s"PANIC: Text doc cannot contain new lines")
      else Text(chars)

    /** Handle optinal formats */
    implicit def maybeDoc(doc: Option[Doc]): Doc =
      doc.getOrElse(empty)

    /** A space */
    val space: Doc = Text(" ")

    /** Nest the document at the current nesting level plus the specified indentation **/
    def nest(indentation: Int)(x: Doc): Doc = Nest(Some(indentation), x)

    /** Nest the document at the current column **/
    def nest(x: Doc): Doc = Nest(None, x)

    /** Group specify that an alternative layout is allowed.
      * It flatten all line breaks in the given document if that fits the page
      */
    def group(x: Doc): Doc =
      x.flatten.map(x0 => Union(x0, x)).getOrElse(x)

    def fold(xs: List[Doc])(f: (Doc,Doc) => Doc): Doc =
      xs match {
        case Nil =>
          NilDoc
        case x :: Nil =>
          x
        case x :: xs =>
          f(x, fold(xs)(f))
      }

    /** Create a text block of several lines combined with hard new lines */
    def textBlock(lines: TraversableOnce[String]): Doc =
      nest(fold(lines.toList.map(Text))(_ <|> _))

    /** Stack all documents with hard new lines in between */
    def stack(xs: Doc*): Doc = fold(xs.toList)(_ <|> _)

    /** Merge all documents by using the specified seperator */
    def spread(xs: Seq[Doc], sep: Doc = "," <> line): Doc =
      group(fold(xs.toList)(_ <> sep <> _))

    /** Create a layout for a binary operator, allowing new line after
      * the operator if the expression does not fit the page
      * @param indent The indentation to use when a new line is inserted
      */
    def binary(x: Doc, op: Doc, y: Doc, indent: Int = 0): Doc =
      group(x <+> op <> nest(indent)(line <> y))

    /** Enclose doc into left and right bracked
      * @param indent The indentation to use when a new line is inserted
      */
    def bracket(l: Doc, x: Doc, r: Doc, indent: Int = 2, tight: Boolean = true): Doc = {
      val nl = if(tight) linebreak else line
      group(l <> nest(indent)(nl <> x) <> nl <> r)
    }
  }

  implicit def stringToDoc(chars: String): Doc = Doc.text(chars)

  private[PrettierPrinter] case object NilDoc extends Doc
  private[PrettierPrinter] case object NoFlat extends Doc
  private[PrettierPrinter] case class Text(chars: String) extends Doc
  private[PrettierPrinter] case class Line(flattenTo: String) extends Doc
  private[PrettierPrinter] case class Concat(x: Doc, y: Doc) extends Doc
  private[PrettierPrinter] case class Nest(indentation: Option[Int], doc: Doc) extends Doc
  private[PrettierPrinter] case class Union(x: Doc, y: Doc) extends Doc

  // -- Chunks are just a stream of text/new line
  private[PrettierPrinter] sealed trait Chunk { def next: Chunk = NilChunk }
  private[PrettierPrinter] case object NilChunk extends Chunk
  private[PrettierPrinter] case class TextChunk(chars: String, override val next: Chunk) extends Chunk
  private[PrettierPrinter] case class LineChunk(indentation: Int, override val next: Chunk) extends Chunk

  private[PrettierPrinter] object Chunk {
    // Check if a chunk fits in line
    @tailrec def fits(w: Int, x: Chunk): Boolean = x match {
      case _ if w < 0 =>
        false
      case TextChunk(s, x) =>
        fits(w - s.size, x)
      case NilChunk | LineChunk(_, _) =>
        true
    }

    // output a chunk stream to a string buffer
    @tailrec def layout(x: Chunk, buffer: StringBuilder = new StringBuilder): StringBuilder  = x match {
      case NilChunk =>
        buffer
      case TextChunk(s, x) =>
        layout(x, buffer.append(s))
      case LineChunk(i, x) =>
        layout(x, buffer.append("\n").append(" " * i))
    }
  }
}