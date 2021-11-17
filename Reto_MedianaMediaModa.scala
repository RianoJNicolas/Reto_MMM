// definicion de funcion para calcular la mediana

def medianaRes(x: Seq[Double])  = {
    val y = x.sorted
    val longitud = x.size

    if (longitud%2 == 0){
        val pos1 = longitud/2
        val pos2 = pos1 - 1
        val med = (y(pos1) + y(pos2))/2
        val res = Map("Mediana" -> med)
        res
    }
    else{
        val pos1 = longitud/2
        val med = y(pos1)
        val res = Map("Mediana" -> med)
        res
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
    res
}

def modaRes(x: Seq[Double]) ={
    val freq = x.groupBy(x => x).mapValues(_.size)
    val maximum = freq.valuesIterator.max
    val res0 = freq.filter(_._2 == maximum).map{case(k,v) => k}.toList
    val res = Map("Moda" -> res0)
    res  
}

def operEstad(x: Seq[Double]) = {
    val mediana = medianaRes(x)
    val media = mediaRes(x)
    val moda = modaRes(x)
    
    val resultados = mediana .++ (moda) .++ (media)
    resultados  
}

val l = Seq(2,5,5,5,4,6,4,6)
val l0 = Seq(7.0,8.0,9.0,10.0,11.0,12.0)
val l1 = Seq(3.0,10.0,3.0,255.0,79.0,24.0,5.0,8.0,1.5,255.0,79.0,3.0,79.0)
val l2 = Seq(2,5,5,6,8,8,9,11)

println(operEstad(l1))
