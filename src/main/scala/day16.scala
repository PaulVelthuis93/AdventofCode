object Day16 {

  private val sampleRegex =
    """Before: \[(\d+), (\d+), (\d+), (\d+)\]
      |(\d+) (\d+) (\d+) (\d+)
      |After:  \[(\d+), (\d+), (\d+), (\d+)\]""".stripMargin.r

  type Registers = Seq[Int]

  case class Instruction(opcode: String, a: Int, b: Int, c: Int) {
    def apply(registers: Registers): Registers = {
      registers.updated(c, opcode match {
        case "addr" => registers(a) + registers(b)
        case "addi" => registers(a) + b
        case "mulr" => registers(a) * registers(b)
        case "muli" => registers(a) * b
        case "banr" => registers(a) & registers(b)
        case "bani" => registers(a) & b
        case "borr" => registers(a) | registers(b)
        case "bori" => registers(a) | b
        case "setr" => registers(a)
        case "seti" => a
        case "gtir" => if (a > registers(b)) 1 else 0
        case "gtri" => if (registers(a) > b) 1 else 0
        case "gtrr" => if (registers(a) > registers(b)) 1 else 0
        case "eqir" => if (a == registers(b)) 1 else 0
        case "eqri" => if (registers(a) == b) 1 else 0
        case "eqrr" => if (registers(a) == registers(b)) 1 else 0
      })
    }
  }

  case class InputInstruction(opcode: Int, a: Int, b: Int, c: Int) {
    def toInstruction(opcode: String): Instruction = Instruction(opcode, a, b, c)
  }

  case class Sample(before: Registers, instruction: InputInstruction, after: Registers)

  val opcodes = Set("addr", "addi", "mulr", "muli", "banr", "bani", "borr", "bori", "setr", "seti", "gtir", "gtri", "gtrr", "eqir", "eqri", "eqrr")

  def countSampleOpcodes(sample: Sample): Int = {
    opcodes.count { opcode =>
      sample.instruction.toInstruction(opcode)(sample.before) == sample.after
    }
  }

  def parseSample(s: String): Sample = s match {
    case sampleRegex(before0, before1, before2, before3, opcode, a, b, c, after0, after1, after2, after3) =>
      Sample(
        Seq(before0, before1, before2, before3).map(_.toInt),
        InputInstruction(opcode.toInt, a.toInt, b.toInt, c.toInt),
        Seq(after0, after1, after2, after3).map(_.toInt)
      )
  }

  def parseInputInstruction(s: String): InputInstruction = {
    val Seq(opcode, a, b, c) = s.split(" ").toSeq.map(_.toInt)
    InputInstruction(opcode, a, b, c)
  }

  def parseInput(input: String): (Seq[Sample], Seq[InputInstruction]) = {
    val (samples, program) = input.splitAt(input.indexOf("\n\n\n\n"))
    (samples.split("""\n\n""").map(parseSample), program.trim.lines.map(parseInputInstruction).toSeq)
  }

  def countSampleThreeOrMore(input: String): Int = {
    val (samples, _) = parseInput(input)
    samples.count(countSampleOpcodes(_) >= 3)
  }

  def sampleOpcodes(sample: Sample): Set[String] ={
    opcodes.filter({ opcode =>
      sample.instruction.toInstruction(opcode)(sample.before) == sample.after
    })
  }

  def opcodeMap(samples: Seq[Sample]): Map[Int, String] = {
    val initialOpcodeMap = (0 to 15).map(_ -> opcodes).toMap
    val finalOpcodeMap = samples.foldLeft(initialOpcodeMap)({ (opcodeMap, sample) =>
      val opcode = sample.instruction.opcode
      val opcodes = opcodeMap(opcode) intersect sampleOpcodes(sample)
      val newOpcodeMap = opcodeMap.updated(opcode, opcodes)
      if (opcodes.size == 1)
        newOpcodeMap.mapValues(o => if (o == opcodes) o else o -- opcodes)
      else
        newOpcodeMap
    })
    finalOpcodeMap.mapValues(_.head)

  }

  def valueRegistr0(input: String): Int = {
    val (samples, program) = parseInput(input)
    val opcodeMapping = opcodeMap(samples)
    program.foldLeft(Seq(0, 0, 0, 0))({ case (registers, inputInstruction) =>
      val opcode = opcodeMapping(inputInstruction.opcode)
      val instruction = inputInstruction.toInstruction(opcode)
      instruction(registers)
    }).head
  }

  def main(args: Array[String]): Unit = {
    lazy val input: String = io.Source.fromInputStream(getClass.getResourceAsStream("files/inputday16.txt")).mkString.trim
    println(countSampleThreeOrMore(input))
    println(valueRegistr0(input))
  }
}
