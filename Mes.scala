import io.Source
import java.awt.image.{RenderedImage, BufferedImage}
import java.io.FileWriter

object Helpers {
  def time[A](a: => A) = {
       val now = System.nanoTime
       val result = a
       val micros = (System.nanoTime - now) / 1000
       println("\n\n%f seconds".format(micros/1000000.0))
       result
    }

  def sat(a:Double) = if (a < 0.0) 0.0 else if (a > 1.0) 1.0 else a
}

object Functions {
  type Fun2D = (Double, Double) => Double
  val Phi = List(
    (x:Double, y:Double) => (1.0-x)*(1.0-y),
    (x:Double, y:Double) => x*(1.0-y),
    (x:Double, y:Double) => x*y,
    (x:Double, y:Double) => (1.0-x)*y,
    (x:Double, y:Double) => (1.0-x)*x*(1.0-y),
    (x:Double, y:Double) => x*(1.0-y)*y,
    (x:Double, y:Double) => y*(1.0-x)*x,
    (x:Double, y:Double) => (1.0-y)*(1.0-x)*y,
    (x:Double, y:Double) => (1.0-x)*x*y*(1.0-y)
 )

  val dPhi_dx = List(
    (x:Double, y:Double) => y-1.0,
    (x:Double, y:Double) => 1.0-y,
    (x:Double, y:Double) => y,
    (x:Double, y:Double) => -y,
    (x:Double, y:Double) => 1.0-y-2*x*(1.0-y),
    (x:Double, y:Double) => y-y*y,
    (x:Double, y:Double) => y-2*x*y,
    (x:Double, y:Double) => y*y-y,
    (x:Double, y:Double) => y-y*y-2.0*x*(y-y*y)
  )

  val dPhi_dy = List(
    (x:Double, y:Double) => x-1.0,
    (x:Double, y:Double) => -x,
    (x:Double, y:Double) => x,
    (x:Double, y:Double) => 1.0-x,
    (x:Double, y:Double) => x*x-x,
    (x:Double, y:Double) => x-2*x*y,
    (x:Double, y:Double) => x-x*x,
    (x:Double, y:Double) => 1.0-x-2.0*y*(1.0-x),
    (x:Double, y:Double) => x-x*x-2.0*y*(x-x*x)
  )

  val d2Phi_d2x = List(
    (x:Double, y:Double) => 0.0,
    (x:Double, y:Double) => 0.0,
    (x:Double, y:Double) => 0.0,
    (x:Double, y:Double) => 0.0,
    (x:Double, y:Double) => -2.0*(1.0-y),
    (x:Double, y:Double) => 0.0,
    (x:Double, y:Double) => -2.0*y,
    (x:Double, y:Double) => 0.0,
    (x:Double, y:Double) => -2.0*(y-y*y)
  )




  val d2Phi_d2y = List(
    (x:Double, y:Double) => 0.0,
    (x:Double, y:Double) => 0.0,
    (x:Double, y:Double) => 0.0,
    (x:Double, y:Double) => 0.0,
    (x:Double, y:Double) => 0.0,
    (x:Double, y:Double) => -2.0*x,
    (x:Double, y:Double) => 0.0,
    (x:Double, y:Double) => -2.0*(1.0-x),
    (x:Double, y:Double) => -2.0*(x-x*x)
  )


//  val d3f_d3x = List(
//    (x:Double, y:Double) => 0.0,
//    (x:Double, y:Double) => 0.0,
//    (x:Double, y:Double) => 0.0,
//    (x:Double, y:Double) => 0.0,
//    (x:Double, y:Double) => 0.0,
//    (x:Double, y:Double) => 0.0,
//    (x:Double, y:Double) => 0.0,
//    (x:Double, y:Double) => 0.0,
//    (x:Double, y:Double) => 0.0
//  )

  val d3Phi_dy_dx = List(
    (x:Double, y:Double) => 0.0,
    (x:Double, y:Double) => 0.0,
    (x:Double, y:Double) => 0.0,
    (x:Double, y:Double) => 0.0,
    (x:Double, y:Double) => 0.0,
    (x:Double, y:Double) => -2.0,
    (x:Double, y:Double) => 0.0,
    (x:Double, y:Double) => 2.0,
    (x:Double, y:Double) => -2.0+2.0*x
  )

  val d3Phi_dx_dy = List(
    (x:Double, y:Double) => 0.0,
    (x:Double, y:Double) => 0.0,
    (x:Double, y:Double) => 0.0,
    (x:Double, y:Double) => 0.0,
    (x:Double, y:Double) => 2.0,
    (x:Double, y:Double) => 0.0,
    (x:Double, y:Double) => -2.0,
    (x:Double, y:Double) => 0.0,
    (x:Double, y:Double) => -2.0+2.0*y
  )


//  val d3f_d3y = List(
//    (x:Double, y:Double) => 0.0,
//    (x:Double, y:Double) => 0.0,
//    (x:Double, y:Double) => 0.0,
//    (x:Double, y:Double) => 0.0,
//    (x:Double, y:Double) => 0.0,
//    (x:Double, y:Double) => 0.0,
//    (x:Double, y:Double) => 0.0,
//    (x:Double, y:Double) => 0.0,
//    (x:Double, y:Double) => 0.0
//  )

//  def shift_me_baby(f:(Double, Double) => Double, a:Double, b:Double, c:Double, d:Double)(x:Double, y: Double) = f((x-a)/(b-a), (y-c)/(d-c))

  def shiftMeBaby(a:Double, b:Double, c:Double, d:Double)(f: (Double, Double) => Double)(x:Double, y: Double) = f((x-a)/(b-a), (y-c)/(d-c))

  val point = 1.0/math.sqrt(3)
  def integrate(a:Double, b: Double)(f:(Double) => Double) = {
    val bma = 0.5*(b-a)
    val bpa = 0.5*(b+a)
    bma*( f( bma*point+bpa ) + f( bma*(-point) + bpa ) )
  }
}

object Edge extends Enumeration {
  type Edge = Value
  val Top, Bottom, Left, Right = Value
}
object Corner extends Enumeration {
  type Corner = Value
  val TopLeft, TopRight, BottomLeft, BottomRight = Value
}

class MesProblem(val N:Int, val dt:Double) {
  import Edge._
  import Corner._
  import Functions._
  import Helpers.sat

  //val N = 16 // Liczba elementow!
  //val dt = 0.1


  var a:Array[Double] = Array.fill((N+1)*(N+1))(0.0)
  var a_new:Array[Double] = Array.fill((N+1)*(N+1))(0.0)
  val b:Array[Double] = Array.fill(2*N*(N+1))(0.0)
  val b_new:Array[Double] = Array.fill(2*N*(N+1))(0.0)
  val c:Array[Double] = Array.fill(N*N)(0.0)
  val c_new:Array[Double] = Array.fill(N*N)(0.0)


  def element(i: Int, j: Int) = j*N+i
  def edge(i: Int, j:Int, idx: Edge) = idx match {
    case Top    => (j+1) * (N) + i
    case Bottom =>  j    * (N) + i
    case Left   => N*(N+1) + j*(N+1) + i
    case Right  => N*(N+1) + j*(N+1) + i + 1
  }
  def vertex(i: Int, j: Int, idx: Corner):Int = idx match {
    case TopLeft     => (j+1) * (N+1) + i
    case TopRight    => (j+1) * (N+1) +(i+1)
    case BottomLeft  =>  j    * (N+1) + i
    case BottomRight =>  j    * (N+1) +(i+1)
  }
  def vertexC(i: Int, j: Int, idx: Corner):(Double, Double) = idx match {
    case TopLeft     => (    i.toDouble/(N+1).toDouble, (j+1).toDouble/(N+1).toDouble)
    case TopRight    => ((i+1).toDouble/(N+1).toDouble, (j+1).toDouble/(N+1).toDouble)
    case BottomLeft  => (    i.toDouble/(N+1).toDouble,     j.toDouble/(N+1).toDouble)
    case BottomRight => ((i+1).toDouble/(N+1).toDouble,     j.toDouble/(N+1).toDouble)
  }

  def xyToIj(x: Double, y: Double):(Int, Int) = (if(x == 1.0) N-1 else (x*N).floor.toInt, if (y == 1.0) N-1 else (y*N).floor.toInt)


  def calculate(funs:List[Fun2D])(x: Double, y: Double):Double = {
    if(x < 0.5 && y < 0.5) 0.0 else
    {
      assert(x >= 0)
      assert(y >= 0)
      assert(x <= 1)
      assert(y <= 1)


      val (i, j) = xyToIj(x, y)
      val multiplier = if(funs == Phi) 1.0 else 1.0/(N.toDouble)
      val z = 1/(N.toDouble)
      val x0 = i*z
      val y0 = j*z

      assert(i < N)
      assert(j < N)
      // println("%f %f %f".format(x0,y0, z))
      val shiftedFuns = funs.map((f) => (x: Double, y: Double) => f((x-x0)/z, (y-y0)/z))

      val coeff:List[Double] = List(
        a(vertex(i, j, BottomLeft)),
        a(vertex(i, j, BottomRight)),
        a(vertex(i, j, TopRight)),
        a(vertex(i, j, TopLeft)),
        b(edge(i,j, Bottom)),
        b(edge(i,j, Right)),
        b(edge(i,j, Top)),
        b(edge(i,j, Left)),
        c(element(i,j))
      )
      multiplier*(shiftedFuns, coeff).zipped.map((f:Fun2D, c:Double) => c*f(x,y)).sum
    }
  }


  val u = calculate(Phi) _
  val u4 = calculate(Phi.slice(0,4)) _
  val u8 = calculate(Phi.slice(0,8)) _
  val du_dx = calculate(dPhi_dx) _
  val du_dy = calculate(dPhi_dy) _
  def lapl(x: Double, y: Double) =
    if (x < 0.5 && y < 0.5) 0.0 else {
      val h:Double = 0.3/N.toDouble
//      calculate(d2Phi_d2x)(x,y)  + calculate(d2Phi_d2y)(x,y)
//      val h2 = 1.0/N.toDouble
//      val x = if(xx < h2) h2 else if (xx > 1.0-h2) 1.0-h2 else xx
//      val y = if(yx < h2) h2 else if (yx > 1.0-h2) 1.0-h2 else yx
//
//      val h = 0.01/N.toDouble
      (u(sat(x-h), y) + u(sat(x+h),y) + u(x, sat(y-h)) + u(x,sat(y+h)) - 4*u(x,y))/(h*h)
//      (
//          if(x < h || 1.0-x < h)
//            calculate(d2Phi_d2x)(x,y)
//          else
//          if(y < h || 1.0-y < h)
//            calculate(d2Phi_d2x)(x,y)
//          else
//            (u(x-h, y) + u(x+h,y) - 2*u(x,y))/(h*h)
//        ) + (
//          if(y < h || 1.0-y < h)
//            calculate(d2Phi_d2y)(x,y)
//          else
//          if(x < h || 1.0-x < h)
//            calculate(d2Phi_d2y)(x,y)
//          else
//            (u(x, y-h) + u(x,y+h) - 2*u(x,y))/(h*h)
//        )
    }

//    (
//      if(x < h || 1.0-x < h)
//        calculate(d2Phi_d2x)(x,y)
//      else
//        if(y < h || 1.0-y < h)
//          calculate(d2Phi_d2x)(x,y)
//        else
//          (u(x-h, y) + u(x+h,y) - 2*u(x,y))/(h*h)
//    ) + (
//      if(y < h || 1.0-y < h)
//        calculate(d2Phi_d2x)(x,y)
//      else
//        if(x < h || 1.0-x < h)
//        calculate(d2Phi_d2x)(x,y)
//          else
//
//        (u(x, y-h) + u(x,y+h) - 2*u(x,y))/(h*h)
//      )
//  val dlalp_dx = calculate(d3Phi_dy_dx) _
//  val dlalp_dy = calculate(d3Phi_dx_dy) _

  def f(x:Double, y: Double) =
    if(x == 0 && y >= 0.5) 1.0 - u(x,y)
    else if (y == 0 && x >= 0.5) - 1.0 - u(x,y)
    else 0.0

  def df_dx(x: Double, y: Double) = 0.0
  def df_dy(x: Double, y: Double) = 0.0

  def bitmap(c:(Double,Double)):Double = c match {
    case(x:Double, y:Double) => {
//      println("x=%f y=%f u=%f f=%f lapl=%f".format(x,y,u(x,y), f(x,y), lapl(x,y)))
      u(x, y) + dt*f(x,y) + dt*lapl(x,y)
    }
  }

  // i,j nr wierzcholka
  def updateA(i:Int, j:Int) = a_new(j*(N+1)+i)  =  {
    assert(i <= N)
    assert(j <= N)
    assert(i >= 0)
    assert(j >= 0)
    bitmap(i.toDouble/N.toDouble, j.toDouble/N.toDouble)
  }


  val lower = (1/(N.toDouble))*integrate(0.0, 1.0)((x) => math.pow(Phi(0)(x, 0.0),2))
  // i,j nr elementu
  def updateB(i:Int, j:Int) {
    val z = 1.0/(N.toDouble)
    val x0 = i*z
    val y0 = j*z

    def smb = shiftMeBaby(x0,x0+z,y0,y0+z) _

    lazy val upperBottom = integrate(x0, x0+z)((x) => (bitmap(x, y0)   - u4(x, y0))*smb(Phi(4))(x,y0)   )
    lazy val upperTop    = integrate(x0, x0+z)((x) => (bitmap(x, y0+z) - u4(x, y0+z) )*smb(Phi(6))(x,y0+z)   )

    lazy val upperRight  = integrate(y0, y0+z)((y) => (bitmap(x0+z, y)   - u4(x0+z, y) )*smb(Phi(5))(x0+z,y)   )
    lazy val upperLeft   = integrate(y0, y0+z)((y) => (bitmap(x0  , y)   - u4(x0,   y) )*smb(Phi(7))(x0,y)   )



//    val lower = integrate(x0,x0+z)((x) => math.pow(smb(Phi(0))(x,y0),2) )
      b_new(edge(i,j,Left)) = upperLeft/lower
      b_new(edge(i,j,Bottom)) = upperBottom/lower


    if(j == N-1)
      b_new(edge(i,j,Top)) = upperTop/lower
    if(i == N-1)
      b_new(edge(i,j,Right)) = upperRight/lower
  }

  def updateC(i:Int, j:Int) {
    val z = 1/(N.toDouble)
    val x0 = i*z
    val y0 = j*z
    def smb = shiftMeBaby(x0,x0+z,y0,y0+z) _


    val upper = integrate(x0,x0+z)((x) => z*integrate(y0,y0+z)((y) => (bitmap(x,y) - u8(x,y))*smb(Phi(8))(x,y) ))
    val lower = integrate(x0,x0+z)((x) => z*integrate(y0,y0+z)((y) => math.pow(smb(Phi(8))(x,y),2)))

    b_new(element(i,j)) = upper/lower
  }

  def simulate() {
      // Pętla po wierzchołkach
      (0 until N+1) foreach ( (j) => {
        (0 until N+1) foreach ( (i) => {
          updateA(i,j)
        })
      })

      (0 until N) foreach ( (i) => {
        (0 until N) foreach ( (j) => {
          updateB(i,j)
        })
      })


//      (0 until N) foreach ( (i) => {
//        (0 until N) foreach ( (j) => {
//          updateC(i,j)
//        })
//      })

      a_new.zipWithIndex.foreach((x) => a(x._2) = x._1)
      b_new.zipWithIndex.foreach((x) => b(x._2) = x._1)
      c_new.zipWithIndex.foreach((x) => c(x._2) = x._1)
  }
}
object Mes {
  val outputRes = 64


  def main(args:Array[String]) {
    val myArgs = if(args.length > 0) args else Array("20", "8", "0.01")
    val problem = new MesProblem(myArgs(2).toInt, myArgs(3).toDouble)
    val totalSteps = myArgs(0).toInt
    val renderStep = myArgs(1).toInt

    def writePlot(step:Int) = {
      val fw = new FileWriter("plots/test-%09d.plt".format(step))
      fw.write(
        "set size square\n" +
        "set xrange [0:1]\n" +
        "set yrange [0:1]\n" +
//        "set cbrange [-1:1]\n" +
        "set title '" + step.toString + "'\n" +
        "plot '-' with image\n"
      )
      (0 until outputRes) foreach ( (j) => {
        (0 until outputRes) foreach ( (i) => {
          val uc = problem.u(j.toDouble/(outputRes - 1).toDouble, i.toDouble/(outputRes - 1).toDouble)
          fw.write("%s %s %s\n".format((j/(outputRes-1).toDouble).toString, (i/(outputRes-1).toDouble).toString, uc.toString))
        })
        fw.write("\n")
      })
      fw.write("e\n")
      fw.write("!sleep 0.03\n")
      fw.close()
    }

//    writePlot(-1)

    (0 until totalSteps by renderStep).foreach( (step) => {
      (step until step+renderStep).foreach( (step) => {
        print("\r%10d %3.1f%%".format(step, 100.0*step/totalSteps.toDouble))
        problem.simulate()
      })
      writePlot(step)
    })
    writePlot(9999999)


  }
}
