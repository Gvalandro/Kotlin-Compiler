fun main() {
    val a: Int = 10
    val b: Int = 5
    val c: Boolean = (a > b) && (a != b) || !(a < b)

    if (c) {
        print("Logical expression evaluated to true")
    } else {
        print("Logical expression evaluated to false")
    }
}
