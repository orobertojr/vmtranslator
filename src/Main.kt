import java.io.File

fun main(vararg args: String) {
    val arq = File(args[0])
    if(arq.isDirectory) {
        val files = arq.listFiles {file -> file.extension == "vm"} ?: throw IllegalArgumentException()
        files.forEach { inputArq -> translate(inputArq) }
    } else{
        translate(arq)
    }
}

fun translate(inputArq: File) {
    val outputArq = File("${inputArq.parent}/${inputArq.nameWithoutExtension}.asm")
    if(outputArq.exists()) outputArq.delete()
    val parser = Parser(inputArq)
    val translator = CodeWriter(outputArq)
    while (parser.hasMoreCommands()) {
        parser.advance()
        when(parser.commandType()) {
            CommandType.ARITHMETIC -> {
                val cmd = parser.arg1()
                translator.writeArithmetic(cmd)
            }
            CommandType.PUSH -> {
                val seg = parser.arg1()
                val id = parser.arg2()
                translator.writePushOrPop("push", seg, id)
            }
            CommandType.POP -> {
                val seg = parser.arg1()
                val id = parser.arg2()
                translator.writePushOrPop("pop", seg, id)
            }
        }
    }
}