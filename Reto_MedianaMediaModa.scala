// definicion de funcion para calcular la mediana

def medianaRes(x: Seq[Double])  = {
    val y = x.sorted
    val longitud = x.size

    if (longitud%2 == 0){
        val pos1 = longitud/2
        val pos2 = pos1 - 1
        val med = (y(pos1) + y(pos2))/2
        val res = Map("Mediana" -> med)
        println(res)
    }
    else{
        val pos1 = longitud/2
        val med = y(pos1)
        val res = Map("Mediana" -> med)
        println(res)
    }
}

def mediaRes(x: Seq[Double]) = {
    val longitud = x.size
    var pos = 0
    var sum = 0.0

    while (pos < longitud) {
        sum = x(pos) + sum
        pos = pos + 1
    }

    val res = Map("Media" -> sum / longitud)
    println(res)
}


val l = Seq(2,5,5,5,4,6,4,6)
val l0 = Seq(7.0,8.0,9.0,10.0,11.0,12.0)
val l1 = Seq(3.0,10.0,36.0,255.0,79.0,24.0,5.0,8.0)
val l2 = Seq(2,5,5,6,8,8,9,11)

medianaRes(l1)
mediaRes(l1)