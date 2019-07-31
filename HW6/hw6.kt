package hello


fun everyNth(L: List<Any>, N:Int ): List<Any> {
    return L.filterIndexed{Index, _ -> Index % N == N - 1}
}

fun main() {
    var testList = List(20){i->i+1}
    println("\nOriginal List: $testList \n")
    println("After calling everyNth: ${everyNth(testList, 5)} \n")
}
