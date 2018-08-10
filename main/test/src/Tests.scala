import utest._

object Tests extends TestSuite {

  val tests = Tests {

    import PrettierPrinter._
    import Doc._

    def check(width: Int, expected: String)(implicit doc: Doc) =
      doc.pretty(width) ==> expected.stripMargin.trim

    * - {
      implicit val doc = "1" <+> "+" <+> "2"
      * - check(10, """1 + 2""")
      * - check( 2, """1 + 2""")
    }

    * - {
      implicit val doc = group("1" <+> "+" <+> "2")
      * - check(10, """1 + 2""")
      * - check( 2, """1 + 2""")
    }

    * - {
      implicit val doc = "1" <+> "+" </> "2"
      * -check(10, """1 +
                     |2
                   """)
      * -check( 2, """1 +
                     |2
                   """)
    }

    * - {
      implicit val doc = group("1" <+> "+" </> "2")
      * -check(10, """1 + 2""")
      * -check( 2, """1 +
                     |2
                   """)
    }

    * - {
      implicit val doc = group("1" <+> "+" <|> "2")
      * -check(10, """1 +
                     |2
                   """)
      * -check( 2, """1 +
                     |2
                   """)
    }

    * - {
      implicit val doc = "println(" <> nest(spread(List("lol", "very long thing", "toto"))) <> ")"
      * - check(80, """println(lol, very long thing, toto)""")
      * - check( 5, """println(lol,
                      |        very long thing,
                      |        toto)
                    """)
    }

    * - {
      implicit val doc =
        "println(" <> textBlock(Seq(
          """"Hello,""",
          """|""",
          """|This, is a multiline string.""""
        )) <> ")"
      * - check(80, """println("Hello,
                      |        |
                      |        |This, is a multiline string.")
                    """)
    }

    * - {
      implicit val doc =
        "math.min(" <> nest(2)(line <> "1," </> "2," </> "3") </> ")" <+> "+" <+> "10"
      * - check(40, """math.min(
                      |  1,
                      |  2,
                      |  3
                      |) + 10
                    """
      )
    }

    * - {
      implicit val doc =
        "math.min(" <> nest("1," </> "2," </> "3") <> ")" <+> "+" <+> "10"
      * - check(40, """math.min(1,
                      |         2,
                      |         3) + 10
                    """
      )
    }
  }
}