// scala 2.13.3
// ammonite 2.2.0

println("ok")

def cpu(rps: Double, q: Double): Double          = 2 / (1 + Math.pow(q, -rps)) - 1
def efficiency(rps: Double, cpu: Double): Double = Math.pow((1 + cpu) / (1 - cpu), 1 / rps)
def rps(cpu: Double, eff: Double): Double        = Math.log10((1 + cpu) / (1 - cpu)) / Math.log10(eff)

val aCPU = 0.1
val c    = efficiency(1000, 0.1)

println(s"${cpu(1000, c) * 100}%, efficiency ${(1 / c)}")
println(rps(0.1, c))
