package task

import java.io.File
import java.io.InputStream
import java.util.LinkedList


const val EOF_SYMBOL = -1
const val ERROR_STATE = 0
const val SKIP_VALUE = 0
const val FLOAT  = 1
const val VARIABLE = 2
const val PLUS = 3
const val MINUS = 4
const val TIMES = 5
const val DIVIDE = 6
const val POW = 7
const val LPAREN = 8
const val RPAREN = 9

const val NEWLINE = '\n'.code

interface Automaton {
    val states: Set<Int>
    val alphabet: IntRange
    fun next(state: Int, symbol: Int): Int
    fun value(state: Int): Int
    val startState: Int
    val finalStates: Set<Int>
}

object Example : Automaton {
    override val states = setOf(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14)
    override val alphabet = 0..255
    override val startState = 1
    override val finalStates = setOf(2, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14)

    private val numberOfStates = states.maxOrNull()!! + 1
    private val numberOfSymbols = alphabet.maxOrNull()!! + 1
    private val transitions = Array(numberOfStates) { IntArray(numberOfSymbols) }
    private val values: Array<Int> = Array(numberOfStates) { 0 }

    private fun setTransition(from: Int, symbol: Char, to: Int) {
        transitions[from][symbol.code] = to
    }

    private fun setValue(state: Int, terminal: Int) {
        values[state] = terminal
    }

    override fun next(state: Int, symbol: Int): Int =
        if (symbol == EOF_SYMBOL) ERROR_STATE
        else {
            assert(states.contains(state))
            assert(alphabet.contains(symbol))
            transitions[state][symbol]
        }

    override fun value(state: Int): Int {
        assert(states.contains(state))
        return values[state]
    }

    init {
        for (n in 48..57) {
            setTransition(1, n.toChar(), 2)
            setTransition(2, n.toChar(), 2)
            setTransition(3, n.toChar(), 4)
            setTransition(4, n.toChar(), 4)
            setTransition(5, n.toChar(), 6)
            setTransition(6, n.toChar(), 6)
        }
        for (n in 65..90) {
            setTransition(1, n.toChar(), 5)
            setTransition(5, n.toChar(), 5)
        }
        for (n in 97..122) {
            setTransition(1, n.toChar(), 5)
            setTransition(5, n.toChar(), 5)
        }
        setTransition(2, '.', 3)

        setTransition(1, '+', 7)
        setTransition(1, '-', 8)
        setTransition(1, '*', 9)
        setTransition(1, '/', 10)
        setTransition(1, '^', 11)
        setTransition(1, '(', 12)
        setTransition(1, ')', 13)
        setTransition(1, '\r', 14)
        setTransition(1, ' ', 14)
        setTransition(1, '\n', 14)
        setTransition(14, '\n', 14)
        setTransition(14, '\r', 14)
        setTransition(14, ' ', 14)

        setValue(2, 1)
        setValue(4, 1)
        setValue(5, 2)
        setValue(6, 2)
        setValue(7, 3)
        setValue(8, 4)
        setValue(9, 5)
        setValue(10, 6)
        setValue(11, 7)
        setValue(12, 8)
        setValue(13, 9)
        setValue(14, 0)
    }
}

data class Token(val value: Int, val lexeme: String, val startRow: Int, val startColumn: Int)

class Scanner(private val automaton: Automaton, private val stream: InputStream) {
    private var state = automaton.startState
    private var last: Int? = null
    private var buffer = LinkedList<Byte>()
    private var row = 1
    private var column = 1

    private fun updatePosition(symbol: Int) {
        if (symbol == NEWLINE) {
            row += 1
            column = 1
        } else {
            column += 1
        }
    }

    private fun getValue(): Int {
        var symbol = last ?: stream.read()
        state = automaton.startState

        while (true) {
            updatePosition(symbol)

            val nextState = automaton.next(state, symbol)
            if (nextState == ERROR_STATE) {
                if (automaton.finalStates.contains(state)) {
                    last = symbol
                    return automaton.value(state)
                } else throw Error("Invalid pattern at ${row}:${column}")
            }
            state = nextState
            buffer.add(symbol.toByte())
            symbol = stream.read()
        }
    }

    fun eof(): Boolean =
        last == EOF_SYMBOL

    fun getToken(): Token? {
        if (eof()) return null

        val startRow = row
        val startColumn = column
        buffer.clear()

        val value = getValue()
        return if (value == SKIP_VALUE)
            getToken()
        else
            Token(value, String(buffer.toByteArray()), startRow, startColumn)
    }
}

fun name(value: Int) =
    when (value) {
        1 -> "float"
        2 -> "variable"
        3 -> "plus"
        4 -> "minus"
        5 -> "times"
        6 -> "divide"
        7 -> "pow"
        8 -> "lparen"
        9 -> "rparen"
        else -> throw Error("Invalid value")
    }

class Rezognizer(private val scanner: Scanner) {
    private var last: Token? = null

    fun recognize(): Boolean {
        last = scanner.getToken()
        val status = recognizeE()
        return if (last == null) status
        else false
    }
    fun recognizeE() = recognizeT() && recognizeEE()

    fun recognizeEE():Boolean {
        val lookahead = last?.value ?: return true
        return when(lookahead) {
            PLUS -> recognizeTerminal(PLUS) && recognizeT() && recognizeEE()
            MINUS -> recognizeTerminal(MINUS) && recognizeT() &&recognizeEE()
            else -> true
        }
    }

    fun recognizeT():Boolean = recognizeX() && recognizeTT()

    fun recognizeTT():Boolean {
        val lookahead = last?.value ?: return true
        return when(lookahead) {
            TIMES -> recognizeTerminal(TIMES) && recognizeX() && recognizeTT()
            DIVIDE -> recognizeTerminal(DIVIDE) && recognizeX() && recognizeTT()
            else -> true
        }
    }
    fun recognizeX():Boolean = recognizeY() && recognizeXX()

    fun recognizeXX():Boolean{
        val lookahead = last?.value ?: return true
        return when(lookahead) {
            POW -> recognizeTerminal(POW) && recognizeX()
            else -> true
        }
    }

    fun recognizeY():Boolean{
        val lookahead = last?.value ?: return true
        if(lookahead == MINUS){
            recognizeTerminal(MINUS) && recognizeF()
            return true
        } else if(lookahead == PLUS){
            recognizeTerminal(PLUS) && recognizeF()
            return true
        } else if(recognizeF()){
            return true
        }
        return false
    }

    fun recognizeF():Boolean{
        val lookahead = last?.value ?: return true
        return when(lookahead) {
            LPAREN -> recognizeTerminal(LPAREN) && recognizeE() && recognizeTerminal(RPAREN)
            FLOAT -> recognizeTerminal(FLOAT)
            VARIABLE -> recognizeTerminal(VARIABLE)
            else -> false
        }
    }

    private fun recognizeTerminal(value: Int):Boolean =
        if (last?.value == value) {
            last = scanner.getToken()
            true
        }
        else false
}


fun main(args: Array<String>) {

    if (Rezognizer(Scanner(Example,File(args[0]).inputStream())).recognize()) {
        print("accept")
    } else {
        print("reject")
    }
}