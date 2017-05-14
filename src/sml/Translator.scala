package sml

import java.util.ArrayList

/*
 * The translator of a <b>S</b><b>M</b>al<b>L</b> program.
 */
class Translator(fileName: String) {
  private final val ADD = "add"
  private final val LIN = "lin"
  private final val BNZ = "bnz"
  private final val MUL = "mul"
  private final val SUB = "sub"
  private final val OUT = "out"

  /**
   * translate the small program in the file into lab (the labels) and prog (the program)
   */
  def readAndTranslate(m: Machine): Machine = {
    val labels = m.labels
    var program = m.prog
    import scala.io.Source
    val lines = Source.fromFile(fileName).getLines
    for (line <- lines) {
      val fields = line.split(" ")
      if (fields.length > 0) {
        labels.add(fields(0))
        val cl = getClass(fields(1))
        val apply = cl.getMethods().find(m => m.getName().equals("apply")).get
        val types = apply.getParameterTypes
        var args = new ArrayList[Object]()
        for (i <- fields.indices) {

          if (i == 0) {
            args.add(fields(i))
          } else if (i != 1) {
            types(i - 1).toString match {
              case "int"                    => args.add(fields(i).asInstanceOf[Object])
              case "class java.lang.String" => args.add(fields(i))
            }
          }
        }
        import scala.collection.JavaConversions._
        val instance = apply.invoke(this, args.toSeq: _*).asInstanceOf[Instruction]
        program = program :+ instance
      }else
        println("Unknown instruction " + fields(1))
    }
    new Machine(labels, program)
  }
  private def getClass(arg: String) = {
    val head = arg.charAt(0).toUpper;
    val tail = arg.substring(1);
    Class.forName("sml." + head + tail + "Instruction")
  }
}

object Translator {
  def apply(file: String) = new Translator(file)
}
