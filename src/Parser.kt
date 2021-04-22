import java.io.File
import java.lang.IllegalArgumentException

enum class CommandType (val commandName: String){
    ARITHMETIC("arithmetic"),
    PUSH("push"),
    POP("pop"),
    LABEL("label"),
    GOTO("goto"),
    IF("if-goto"),
    FUNCTION("function"),
    RETURN("return"),
    CALL("call");

    fun hasArg2(): Boolean {
        return this == PUSH || this == POP || this == FUNCTION || this == CALL;
    }
}

class Parser (file: File) {
    private val arq = if (file.exists()) file.bufferedReader() else throw IllegalArgumentException()
    private var currentCommand = ""
    private var nextCommand = ""
    private var currentCommandElements = emptyList<String>()
    private lateinit var currentCmdType: CommandType

    fun hasMoreCommands(): Boolean {
        var line = arq.readLine() ?: return false
        nextCommand = line.substringBefore("//").trim()
        while (nextCommand.isEmpty()) {
            line = arq.readLine() ?: return false
            nextCommand = line.substringBefore("//").trim()
        }
        return true
    }

    fun advance() {
        currentCommand = nextCommand
    }

    fun commandType(): CommandType {
        currentCommandElements = currentCommand.split("(\\s)+".toRegex())
        currentCmdType = when(currentCommandElements[0]) {
            CommandType.PUSH.commandName -> CommandType.PUSH
            CommandType.POP.commandName -> CommandType.POP
            CommandType.LABEL.commandName -> CommandType.LABEL
            CommandType.GOTO.commandName -> CommandType.GOTO
            CommandType.IF.commandName -> CommandType.IF
            CommandType.FUNCTION.commandName -> CommandType.FUNCTION
            CommandType.CALL.commandName -> CommandType.CALL
            CommandType.RETURN.commandName -> CommandType.RETURN
            else -> CommandType.ARITHMETIC
        }
        return currentCmdType
    }

    fun arg1(): String {
        return if(currentCmdType == CommandType.ARITHMETIC) currentCommandElements[0] else currentCommandElements[1]
    }

    fun arg2(): Int {
        if(!currentCmdType.hasArg2()) throw UnsupportedOperationException()
        return currentCommandElements[2].toInt()
    }
}