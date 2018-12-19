object Day19{
  val regex = """(\w+) (\d+) (\d+) (\d+)""".r



  def instruction(opcode: String, a: Int, b: Int, c: Int,reg: Vector[Int]): Vector[Int] ={
      reg.updated(c, opcode match {
        case "addr" => reg(a) + reg(b)
        case "addi" => reg(a) + b
        case "mulr" => reg(a) * reg(b)
        case "muli" => reg(a) * b
        case "banr" => reg(a) & reg(b)
        case "bani" => reg(a) & b
        case "borr" => reg(a) | reg(b)
        case "bori" => reg(a) | b
        case "setr" => reg(a)
        case "seti" => a
        case "gtir" => if (a > reg(b)) 1 else 0
        case "gtri" => if (reg(a) > b) 1 else 0
        case "gtrr" => if (reg(a) > reg(b)) 1 else 0
        case "eqir" => if (a == reg(b)) 1 else 0
        case "eqri" => if (reg(a) == b) 1 else 0
        case "eqrr" => if (reg(a) == reg(b)) 1 else 0
      })
  }


  def factors(num: Int) = {
    (1 to num).filter { divisor =>
      num % divisor == 0
    }
  }


  def runInstruction(registers: Vector[Int], program: Seq[(String, Int, Int, Int)], ip: Int):Vector[Int] = {

    val instructionParam = program(registers(ip))
    val newRegisters: Vector[Int] = instruction(instructionParam._1, instructionParam._2, instructionParam._3, instructionParam._4,registers)
    newRegisters.updated(ip, newRegisters(ip) + 1)
  }

  def main(args: Array[String]): Unit = {
    lazy val input = io.Source.fromInputStream(getClass.getResourceAsStream("files/inputday19.txt")).getLines()
    val ip: Int = input.take(1).mkString.split(" ")(1).toInt
    val program: Seq[(String, Int, Int, Int)] = input.map {
      case regex(opcode, a, b, c) => (opcode, a.toInt, b.toInt, c.toInt)
    }.toVector
    var registers = Vector.fill(6)(0)
    while(registers(ip) < program.size) { registers = runInstruction(registers, program, ip) }
    println(registers.head)
    var registers2 = Vector(1, 0, 0, 0, 0, 0)
    lazy val input2 = io.Source.fromInputStream(getClass.getResourceAsStream("files/inputday19.txt")).getLines()
    val ip2: Int = input2.take(1).mkString.split(" ")(1).toInt
    val program2: Seq[(String, Int, Int, Int)] = input2.map {
      case regex(opcode, a, b, c) => (opcode, a.toInt, b.toInt, c.toInt)
    }.toVector
    while(registers2(ip2) != 1) { registers2 = runInstruction(registers2,program2,ip2) }
    println(factors(registers2(1)).sum)
  }

}