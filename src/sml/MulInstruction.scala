package sml

class MulInstruction(label: String, op: String, val result: Int, val op1: Int, val op2: Int)
  extends Instruction(label, op) {

  override def execute(m: Machine) {
    //get first value of register
    val value1 = m.regs(op1)
    //get value of second register
    val value2 = m.regs(op2)
    //multiply and store result in third register
    m.regs(result) = value1 * value2
  }

  override def toString(): String = {
    super.toString + " " + op1 + " * " + op2 + " to " + result+ "\n"
  }
}

//companion object to create object
object MulInstruction {
  def apply(label: String, result: Int, op1: Int, op2: Int) =
    new MulInstruction(label, "mul", result, op1, op2)
}
