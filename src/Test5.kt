fun main() {
    val a: Int = 10
    val b: Int = 5
    val c: Int = 8
    val d: Boolean = true
    val e: Boolean = false
    val f: Double
	
    // Testando operadores de comparação
    val comparison1 = a > b
    val comparison2 = a <= c
    val comparison3 = b == c
    val comparison4 = a != b
    val comparison5 = c >= a
    val comparison6 = b < a

    // Testando operadores lógicos com variáveis booleanas
    val logical1 = d && e
    val logical2 = d || e
    val logical3 = !d
    val logical4 = (a > b) && (c < a)
    val logical5 = (a == 10) || (b != 5) && !e
    val logical6 = !(a < b) || (c >= a) && d

    // Estrutura condicional com combinações de operadores lógicos e de comparação
    if ((a > b && d) || (c <= a && !e)) {
        print("Logical and comparison operators tested.")
    } else {
        print("Alternative path.")
    }
}
