package sml

class BnzInstruction(label: String, op: String, val reg: Int, val lab2: String)
    extends Instruction(label, op) {

  override def execute(m: Machine) {
    //get the value of the register
    val value1 = m.regs(reg)
    //a boolean variable to check if new instruction is found or not
    var isStop = false
    //copy value of program counter
    var pc=m.pc
    //iterate over the programme and check for next instruction with label lab2
    while (pc < m.prog.length && !isStop) {
      //get instruction 
      val ins = m.prog(m.pc)
      // check if the instruction starts with label
      if (ins.toString().startsWith(lab2))
        isStop = true
      else
        pc+=1
    }
    //if the instruction is found, increment programme counter
    if (isStop)
      m.pc=pc
    
  }
   
  override def toString(): String = {
    super.toString + " Execute next " + lab2 + " statement if " + reg + " is not zero" + "\n"
  }
}
//companion object to create object
object BnzInstruction {
  def apply(label: String, reg: Int, lab2: String) =
    new BnzInstruction(label, "bnz", reg, lab2)
}
