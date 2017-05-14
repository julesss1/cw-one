package sml

class BnzInstruction(label: String, op: String, val reg: Int, val lab2: String)
    extends Instruction(label, op) {

  override def execute(m: Machine) {
    val value1 = m.regs(reg)
    var isStop = false
    var pc=m.pc
    while (pc < m.prog.length && !isStop) {
      val ins = m.prog(m.pc)
      if (ins.toString().startsWith(lab2))
        isStop = true
      else
        pc+=1
    }
    if (isStop)
      m.pc=pc
    
  }

  override def toString(): String = {
    super.toString + " Execute next " + lab2 + " statement if " + reg + " is not zero" + "\n"
  }
}

object BnzInstruction {
  def apply(label: String, reg: Int, lab2: String) =
    new BnzInstruction(label, "bnz", reg, lab2)
}