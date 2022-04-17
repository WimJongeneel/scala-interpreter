package runtime

type MemoryFrame = Map[String, MemoryValue]

case class Memory(val memory: List[MemoryFrame]) {

    def addFrame: Memory = Memory(memory.prepended(Map.empty))

    def dropFrame: Memory = Memory(memory.splitAt(1)._2)

    def declare(name: String, value: MemoryValue): Memory = {
        if memory.isEmpty then throw new Exception("No frame left on memory")
        Memory(memory.updated(0, memory(0).updated(name, value)))
    }

    def read(name: String): MemoryValue = memory
        .find(frame => frame.contains(name))
        .flatMap(frame => frame.get(name))
        .getOrElse(MemoryValue.runtimeNull)

    def assign(name: String, value: MemoryValue): Memory = {
        val frameIndex = memory.indexWhere(frame => frame.contains(name))
        if frameIndex == -1 then throw new Exception("Variable not found: " + name.replace("_", ""))
        Memory(memory.updated(frameIndex, memory(frameIndex).updated(name, value)))
    }
}

object Memory {

    def empty: Memory = Memory(List(Map.empty))

    val RUNTIME_TRUE = 1.0f
    val RUNTIME_FAlSE = 0.0f
    val RUNTIME_NULL = RUNTIME_FAlSE
}
