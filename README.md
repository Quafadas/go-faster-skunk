# go-faster-skunk
Will code gen a string which you could use as your codec given a case class. See tests... very simple. Not recommded for ~~production~~   use, I learned lots about `Mirror` and limitations of derivation.


```
test("small number of known types") {
    case class DeriveTest(
        s: String,
        i: Int,
        so: Option[Short],
        io: Option[Boolean]
    )
    assertEquals(deriveCodec[DeriveTest], "text *: int4 *: int2.opt *: bool.opt")

    val c = (text *: int4 *: int2.opt *: bool.opt).pimap[DeriveTest]
    
    assertEquals(
      c.encode(DeriveTest("a", 1, Some(2), Some(true))), 
      List(Some("a"),Some("1"),Some("2"),Some("t"))
    )

  }
  ```