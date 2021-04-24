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
    PUSH("push"),
   // POP("pop");
}

class CodeWriter (private val file: File) {
    private var eqIncrement = 0
    private var ltIncrement = 0
    private var gtIncrement = 0

    fun writeArithmetic(operator: String) {
        if(isInEnumOperator(operator)){
            val asmCode = when(operator){
                Operator.ADD.opName, Operator.SUB.opName, Operator.AND.opName, Operator.OR.opName, Operator.EQ.opName -> writeBinaryArithmetic(operator)
                Operator.NEG.opName, Operator.NOT.opName -> writeUnaryArithmetic(operator)
                Operator.LT.opName, Operator.GT.opName -> writeRelationalArithmetic(operator)
                else -> throw IllegalArgumentException()
            }
            //val asmCode = generateLine(pop(), decreaseStackPointer(), stackPointer(), cmd, increaseStackPointer())
            //val asmCode = generateLine(onlySP(), "AM=M-1", "D=M", "A=A-1", cmd)
            file.appendText(asmCode)
        }
    }

    private fun writeBinaryArithmetic(operator: String): String {
        val cmd = when(operator) {
            Operator.ADD.opName -> "M=D+M"
            Operator.SUB.opName -> "M=M-D"
            Operator.AND.opName -> "M=D&M"
            Operator.OR.opName -> "M=D|M"
            Operator.EQ.opName -> eq()
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
            else -> throw IllegalArgumentException()
        }
        return generateLine(onlySP(), "AM=M-1", "D=M", onlySP(), "AM=M-1", "D=M-D", cmd)
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
        //return generateLine("@$id", "D=A", cmd, push())
        return generateLine(cmd, "@$id", "A=D+A", "D=M", stackPointer(), "M=D", increaseStackPointer())
    }

    private fun popStaticPointerOrTemp(segment: String, id: Int): String {
        val cmd = when(segment) {
            Command.POINTER.cmdName -> generateLine("@R${3+id} // pop $segment $id") //verificar
            Command.TEMP.cmdName -> generateLine("@R${5+id} // pop $segment $id") //verificar
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
        //return generateLine("@$id", "D=A", cmd, "@R13", "M=D", "", pop(), "@R13", "A=M", "", "M=D" )
        return generateLine(cmd, "@$id", "D=D+A", "@R13", "M=D", decreaseStackPointer(), "A=M", "D=M", "@R13", "A=M", "M=D")
    }

    private fun generateLine(vararg cmds: String): String = cmds.joinToString ("\n", postfix = "\n") {it.replace("(\\n)+$".toRegex(), "")}
    private fun stackPointer() = generateLine("@SP", "A=M")
    private fun onlySP() = generateLine("@SP")
    private fun increaseStackPointer() = generateLine("@SP", "M=M+1")
    private fun decreaseStackPointer() = generateLine("@SP", "M=M-1")
    private fun push() = generateLine(stackPointer(), "M=D", increaseStackPointer())
    //private fun pop() = generateLine(decreaseStackPointer(), stackPointer(), "D=M", "M=0")
    private fun pop() = generateLine(decreaseStackPointer(), onlySP(), "D=M", "M=0")
    private fun eq(): String {
        val cmd = generateLine(
            "D=M-D",
            "@EQ$eqIncrement",
            "D;JEQ",
            "D=1",
            "(EQ$eqIncrement)",
            "D=D-1",
            stackPointer(),
            "M=D",
            increaseStackPointer()
        )
        eqIncrement++
        return cmd
    }
    private fun lt(): String {
        val cmd = generateLine(
            "@LT$ltIncrement",
            "D;JLT",
            "D=0",
            "@NLT$ltIncrement",
            "0;JMP",
            "(LT$ltIncrement)",
            "D=-1",
            "(NLT$ltIncrement)",
            stackPointer(),
            "M=D",
            increaseStackPointer()
        )
        ltIncrement++
        return cmd
    }
    private fun gt(): String {
        val cmd = generateLine(
            "@GT$gtIncrement",
            "D;JGT",
            "D=0",
            "@NGT$gtIncrement",
            "0;JMP",
            "(GT$gtIncrement)",
            "D=-1",
            "(NGT$gtIncrement)",
            stackPointer(),
            "M=D",
            increaseStackPointer()
        )
        gtIncrement++
        return cmd
    }
}
