package sml

import java.util.ArrayList

/*
 * The translator of a <b>S</b><b>M</b>al<b>L</b> programme.
 */
class Translator(fileName: String) {
  private final val ADD = "add"
  private final val LIN = "lin"
  private final val BNZ = "bnz"
  private final val MUL = "mul"
  private final val SUB = "sub"
  private final val OUT = "out"

  /**
   * translate the small program in the file into lab (the labels) and prog (the programme)
   */
  def readAndTranslate(m: Machine): Machine = {
    val labels = m.labels
    var program = m.prog
    import scala.io.Source
    //reading the input file with programme instructions
    val lines = Source.fromFile(fileName).getLines
    //iterate over every line
    for (line <- lines) {
      //split each line by space
      val fields = line.split(" ")
      //check if it is not a empty line
      if (fields.length > 0) {
        //add first field into labels array
        labels.add(fields(0))
        // call the private method to get class for the instruction
        val cl = getClass(fields(1))
        //get the apply method from companion object
        val apply = cl.getMethods().find(m => m.getName().equals("apply")).get
        //get types of the parameters using reflection
        val types = apply.getParameterTypes
        //declare a list to store parameters
        var args = new ArrayList[Object]()
        //iterate over every field
        for (i <- fields.indices) {
          //first argument is not an instruction argument
          if (i == 0) {
            args.add(fields(i))
          } else if (i != 1) {
            //get types of all the arguments and add them in the list
            types(i - 1).toString match {
              case "int"                    => args.add(fields(i).asInstanceOf[Object])
              case "class java.lang.String" => args.add(fields(i))
            }
          }
        }
        import scala.collection.JavaConversions._
        //now invoking the apply method to get actual instruction class object using reflection
        val instance = apply.invoke(this, args.toSeq: _*).asInstanceOf[Instruction]
        //add this new instance to the program
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
