import java.io.File
import java.lang.IllegalArgumentException
import java.util.*

enum class Operator(val opName: String){
    ADD("add"),
    SUB("sub"),
    NEG("neg"),
    AND("and"),
    OR("or"),
    NOT("not"),
    GT("gt"),
    LT("lt"),
    EQ("eq");
}

enum class Command(val cmdName: String){
    ARGUMENT("argument"),
    LOCAL("local"),
    THIS("this"),
    THAT("that"),
    POINTER("pointer"),
    TEMP("temp"),
    CONSTANT("constant"),
    STATIC("static"),
    PUSH("push");
   // POP("pop");
}

class CodeWriter (private val file: File) {
    private var eqIncrement = 0
    private var ltIncrement = 0
    private var gtIncrement = 0

    fun writeArithmetic(operator: String) {
        if(isInEnumOperator(operator)){
            val asmCode = when(operator){
                Operator.ADD.opName, Operator.SUB.opName, Operator.AND.opName, Operator.OR.opName -> writeBinaryArithmetic(operator)
                Operator.NEG.opName, Operator.NOT.opName -> writeUnaryArithmetic(operator)
                Operator.LT.opName, Operator.GT.opName, Operator.EQ.opName -> writeRelationalArithmetic(operator)
                else -> throw IllegalArgumentException()
            }
            file.appendText(asmCode)
        }
    }

    private fun writeBinaryArithmetic(operator: String): String {
        val cmd = when(operator) {
            Operator.ADD.opName -> "M=D+M"
            Operator.SUB.opName -> "M=M-D"
            Operator.AND.opName -> "M=D&M"
            Operator.OR.opName -> "M=D|M"
            else -> throw IllegalArgumentException()
        }
        return generateLine(onlySP(), "AM=M-1", "D=M", "A=A-1", cmd)
    }

    private fun writeUnaryArithmetic(operator: String): String {
        val cmd = when(operator) {
            Operator.NOT.opName -> "M=!M"
            Operator.NEG.opName -> "M=-M"
            else -> throw IllegalArgumentException()
        }
        return generateLine(stackPointer(), "A=A-1", cmd)
    }

    private fun writeRelationalArithmetic(operator: String): String {
        val cmd = when(operator) {
            Operator.GT.opName -> gt()
            Operator.LT.opName -> lt()
            Operator.EQ.opName -> eq()
            else -> throw IllegalArgumentException()
        }
        return generateLine(onlySP(), "AM=M-1", "D=M", "A=A-1", "D=M-D", cmd)
    }

    fun writePushOrPop(cmd: String, segment: String, id: Int) {
        val asmCode = if(cmd == Command.PUSH.cmdName) {
            when(segment) {
                Command.CONSTANT.cmdName -> pushConst(id)
                Command.ARGUMENT.cmdName, Command.LOCAL.cmdName, Command.THIS.cmdName, Command.THAT.cmdName -> pushSegment(segment, id)
                Command.POINTER.cmdName, Command.TEMP.cmdName -> pushPointerOrTemp(segment, id)
                Command.STATIC.cmdName -> generateLine("@Foo.$id // push static $id", "D=M", push())
                else -> throw IllegalArgumentException()
            }
        } else {
            when(segment) {
                Command.ARGUMENT.cmdName, Command.LOCAL.cmdName, Command.THIS.cmdName, Command.THAT.cmdName -> pop2Seg(segment, id)
                Command.STATIC.cmdName, Command.POINTER.cmdName, Command.TEMP.cmdName -> popStaticPointerOrTemp(segment, id)
                else -> throw IllegalArgumentException()
            }
        }
        file.appendText(asmCode)
    }

    private fun isInEnumOperator(value: String?): Boolean {
        return Arrays.stream(Operator.values()).anyMatch { e -> e.opName == value }
    }

    private fun pushConst(const: Int) = generateLine("@$const // push constant $const ", "D=A") + push()
    private fun pushPointerOrTemp(segment: String, id: Int): String {
        val cmd = when(segment) {
            Command.POINTER.cmdName -> generateLine("@R${3+id} // push pointer $id", "D=M")
            Command.TEMP.cmdName -> generateLine("@R${5+id} // push temp $id", "D=M")
            else -> throw IllegalArgumentException()
        }
        return cmd + push()
    }

    private fun pushSegment(segment: String, id: Int): String {
        val cmd = when(segment) {
            Command.ARGUMENT.cmdName -> generateLine("@ARG // push $segment $id", "D=M")
            Command.LOCAL.cmdName -> generateLine("@LCL // push $segment $id", "D=M")
            Command.THIS.cmdName -> generateLine("@THIS // push $segment $id", "D=M")
            Command.THAT.cmdName -> generateLine("@THAT // push $segment $id", "D=M")
            else -> throw IllegalArgumentException()
        }
        return generateLine(cmd, "@$id", "A=D+A", "D=M", stackPointer(), "M=D", increaseStackPointer())
    }

    private fun popStaticPointerOrTemp(segment: String, id: Int): String {
        val cmd = when(segment) {
            Command.POINTER.cmdName -> generateLine("@R${3+id} // pop $segment $id")
            Command.TEMP.cmdName -> generateLine("@R${5+id} // pop $segment $id")
            Command.STATIC.cmdName -> generateLine("@Foo.$id")
            else -> throw IllegalArgumentException()
        }
        return generateLine(decreaseStackPointer(), "A=M", "D=M", cmd, "M=D")
    }

    private fun pop2Seg(segment: String, id: Int): String {
        val cmd = when(segment) {
            Command.ARGUMENT.cmdName -> generateLine("@ARG // pop $segment $id", "D=M")
            Command.LOCAL.cmdName -> generateLine("@LCL // pop $segment $id", "D=M")
            Command.THIS.cmdName -> generateLine("@THIS // pop $segment $id", "D=M")
            Command.THAT.cmdName -> generateLine("@THAT // pop $segment $id", "D=M")
            else -> throw IllegalArgumentException()
        }
        return generateLine(cmd, "@$id", "D=D+A", "@R13", "M=D", decreaseStackPointer(), "A=M", "D=M", "@R13", "A=M", "M=D")
    }

    private fun generateLine(vararg cmds: String): String = cmds.joinToString ("\n", postfix = "\n") {it.replace("(\\n)+$".toRegex(), "")}
    private fun stackPointer() = generateLine("@SP", "A=M")
    private fun onlySP() = generateLine("@SP")
    private fun increaseStackPointer() = generateLine("@SP", "M=M+1")
    private fun decreaseStackPointer() = generateLine("@SP", "M=M-1")
    private fun push() = generateLine(stackPointer(), "M=D", increaseStackPointer())
    private fun eq(): String {
        val cmd = generateLine(
            "@EQ$eqIncrement",
            "D;JEQ",
            onlySP(),
            "A=M-1",
            "M=0",
            "@EQSTACK$eqIncrement",
            "0;JMP",
            "(EQ$eqIncrement)",
            onlySP(),
            "A=M-1",
            "M=-1",
            "(EQSTACK$eqIncrement)"
        )
        eqIncrement++
        return cmd
    }
    private fun lt(): String {
        val cmd = generateLine(
            "@LT$ltIncrement",
            "D;JLT",
            onlySP(),
            "A=M-1",
            "M=0",
            "@LTSTACK$ltIncrement",
            "0;JMP",
            "(LT$ltIncrement)",
            onlySP(),
            "A=M-1",
            "M=-1",
            "(LTSTACK$ltIncrement)"
        )
        ltIncrement++
        return cmd
    }
    private fun gt(): String {
        val cmd = generateLine(
            "@GT$gtIncrement",
            "D;JGT",
            onlySP(),
            "A=M-1",
            "M=0",
            "@GTSTACK$gtIncrement",
            "0;JMP",
            "(GT$gtIncrement)",
            onlySP(),
            "A=M-1",
            "M=-1",
            "(GTSTACK$gtIncrement)"
        )
        gtIncrement++
        return cmd
    }
}
