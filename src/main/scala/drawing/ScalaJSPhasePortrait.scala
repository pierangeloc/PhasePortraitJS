package drawing

import org.scalajs.dom.html
import html.Canvas

import org.scalajs.dom
import org.scalajs.dom.html

import scala.scalajs.js.annotation.JSExport
import scala.util.Random

//an abstract point in R2
case class Point (x: Double, y: Double) {
  def +(other: Point) = Point(this.x + other.x, this.y + other.y)
  def -(other: Point) = Point(this.x - other.x, this.y - other.y)
  def *(factor: Double) = Point(this.x * factor, this.y * factor)
  def abs = Math.sqrt(x * x + y * y)
}

@JSExport
object ScalaJSPhasePortrait {
  def omega = 1
  def simplePendulum = (x: Point) => Point(x.y, - omega * omega * x.x)
  def realPendulum = (x: Point) => Point(x.y, - omega * omega * Math.sin(x.x))
  def lotkaVolterra = (x: Point) => Point(x.x - x.x * x.y, x.x * x.y - x.y)
  def vanDerPol = (x: Point) => Point(x.y, 2 * (1 - x.x * x.x) * x.y - x.x)




  @JSExport
  def realPendulumPhasePortrait(canvas: Canvas): Unit = phasePortrait(DrawingWindow(canvas, 0, 0, 10, 5), realPendulum)

  @JSExport
  def simplePendulumPhasePortrait(canvas: Canvas): Unit = phasePortrait(DrawingWindow(canvas, 0, 0, 10, 5), vanDerPol)

  @JSExport
  def lotkaVolterraPhasePortrait(canvas: Canvas): Unit = phasePortrait(DrawingWindow(canvas, 4, 2, 4, 2), lotkaVolterra)

  
  def phasePortrait(drawingWindow: DrawingWindow, system: (Point) => Point = lotkaVolterra): Unit = {

    val canvas = drawingWindow.canvas
    /**
     * GENERAL PLOTTING FEATURES
     */
    println(s"canvas ${canvas.width} x ${canvas.height}")


    def plotter = Plot(drawingWindow)

    /**
     * ODE related features
     */

    //evolution of a dynamic system in R2 (RK 4th order)
    def evolution(f: Point => Point, x0: Point, deltaT: Double = 0.05): Stream[(Double, Point)] = Stream.iterate((0.0, x0)) {
      case (tn: Double, xn: Point) => {
        val an = f(xn)
        val bn = f(xn + (an * (deltaT / 2)))
        val cn = f(xn + (bn * (deltaT / 2)))
        val dn = f(xn + (cn * (deltaT)))

        val next = (tn + deltaT, xn + (an + bn * 2 + cn * 2 + dn) * (deltaT / 6))
//        println(s"evolving ($tn, $xn) => $next")
        next
      }
    }

     def phasePortrait(f: Point => Point) = {
      val verticalLines = (drawingWindow.centerX - drawingWindow.halfWidth) to (drawingWindow.centerX + drawingWindow.halfWidth)
      val horizontalLines = (drawingWindow.centerY - drawingWindow.halfHeight) to (drawingWindow.centerY + drawingWindow.halfHeight)

      for {
        x <- verticalLines
        y <- horizontalLines
      } {
          println(s"x0 = ${Point(x, y)}")
          //rgb to plot the orbit with 1 color
          val rgb = s"rgb(${Random.nextInt(256)},${Random.nextInt(256)},${Random.nextInt(256)})"

          evolution(f, Point(x, y)).map {
            case (t, p) => {
              plotter.draw(p, rgb)
              p
            }
          } take(3000) toList

        //alternative implementation, drawing line between 2 consecutive points in evolution
//          evolution(f, Point(x, y)).take(2000).foldLeft(Point(x, y)) {
//            case (from: Point, (at: Double, to: Point)) => plotter.drawLine(from, to); to
//          }
        }
    }

    plotter.drawGrid()
    phasePortrait(system)
  }






}
