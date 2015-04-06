package drawing

import org.scalajs.dom.html
import html.Canvas

import org.scalajs.dom
import org.scalajs.dom.html

import scala.scalajs.js.annotation.JSExport

//an abstract point in R2
sealed case class Point (x: Double, y: Double) {
  def +(other: Point) = Point(this.x + other.x, this.y + other.y)
  def -(other: Point) = Point(this.x - other.x, this.y - other.y)
  def /(factor: Double) = Point(this.x / factor, this.y / factor)
  def *(factor: Double) = Point(this.x * factor, this.y * factor)
}
//a point in the canvas
sealed case class CanvasPoint(x: Int, y: Int)

@JSExport
object ScalaJSPhasePortrait {


  @JSExport
  def main(implicit canvas: Canvas): Unit = {

    println(s"canvas ${canvas.width} x ${canvas.height}")
    case class DrawingWindow(centerX: Int, centerY: Int, halfWidth: Int, halfHeight: Int)(implicit canvas: Canvas) {
      val deltaX = halfWidth * 2.0 / canvas.width
      val deltaY = halfHeight * 2.0 / canvas.height
    }

    implicit val renderer = canvas.getContext("2d")
      .asInstanceOf[dom.CanvasRenderingContext2D]


    //a representation of the window of R2 we want the canvas to show. Center and sizes are integers to make things more standard
    implicit val drawingWindow = DrawingWindow(0, 0, 20, 10)

    def drawGrid()(implicit window: DrawingWindow) = {
      val verticalLines = (window.centerX - window.halfWidth) to (window.centerX + window.halfWidth)
      val horizontalLines = (window.centerY - window.halfHeight) to (window.centerY + window.halfHeight)
      renderer.font = "normal normal 20px Verdana"

      for {
        x <- verticalLines
      } {
          if(x % 5 == 0) {
            val canvasPosition = point2CanvasPoint(Point(x, 0))(window)
//            renderer.fillText(x.toString, canvasPosition.x, canvasPosition.y + 20)
            drawLine(Point(x, window.centerY - window.halfHeight), Point(x, window.centerY + window.halfHeight), "red", 0.3)
          } else {
            drawLine(Point(x, window.centerY - window.halfHeight), Point(x, window.centerY + window.halfHeight), "blue", 0.1)
          }
        }

      for {
          y <- horizontalLines
      } {
          if(y % 5 == 0) {
            val canvasPosition = point2CanvasPoint(Point(0, y))(window)
//            renderer.fillText(y.toString, canvasPosition.x, canvasPosition.y)
            drawLine(Point(window.centerX - window.halfWidth, y), Point(window.centerX + window.halfWidth, y), "red", 0.3)
          } else {
            drawLine(Point(window.centerX - window.halfWidth, y), Point(window.centerX + window.halfWidth, y), "blue", 0.1)
          }
        }
    }


    def point2CanvasPoint(p: Point)(implicit window: DrawingWindow): CanvasPoint = {
      val windowLeftEdge = window.centerX - window.halfWidth
      val windowUpEdge = window.centerY + window.halfHeight

      CanvasPoint(((p.x - windowLeftEdge) / window.deltaX).toInt, (-(p.y - windowUpEdge) / window.deltaY).toInt)
    }


    def draw(c: Point, fillStyle: String = "black", dotSize: Double = 1.0) (implicit renderer: dom.CanvasRenderingContext2D)= {
      renderer.fillStyle = fillStyle
      drawDot(point2CanvasPoint(c)) { p =>
        renderer.beginPath()
        renderer.arc(p.x, p.y, dotSize, 0, 2 * Math.PI)
        renderer.fill
      }
    }

    def drawDot(p: CanvasPoint)(f: CanvasPoint => Unit) = f(p)

    def drawLine(from: Point, to: Point, strokeStyle: String = "black", lineWidth: Double = 2.0)(implicit renderer: dom.CanvasRenderingContext2D) = {
//      println(s"drawing line from $from to $to")
      renderer.beginPath()
      renderer.lineWidth = lineWidth
      renderer.strokeStyle = strokeStyle
      val fromPoint = point2CanvasPoint(from)
      val toPoint = point2CanvasPoint(to)

//      println(s"drawing actual line on canvas from $fromPoint to $toPoint in a canvas ${renderer.canvas.width} x ${renderer.canvas.height}")
      renderer.moveTo(fromPoint.x, fromPoint.y)
      renderer.lineTo(toPoint.x, toPoint.y)
      renderer.stroke()
    }


    //evolution of a dynamic system in R2 (RK 4th order)
    def evolution(f: Point => Point, x0: Point, deltaT: Double = 0.1): Stream[(Double, Point)] = Stream.iterate((0.0, x0)) {
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

    def omega = 1
    def simplePendulum = (x: Point) => Point(x.y, - omega * omega * x.x)
    def realPendulum = (x: Point) => Point(x.y, - omega * omega * Math.sin(x.x))
    def lotkaVolterra = (x: Point) => Point(x.x - x.x * x.y, x.x * x.y - x.y)




    def phasePortrait(f: Point => Point)(implicit window: DrawingWindow) = {
      val verticalLines = (window.centerX - window.halfWidth) to (window.centerX + window.halfWidth)
      val horizontalLines = (window.centerY - window.halfHeight) to (window.centerY + window.halfHeight)

      for {
        x <- verticalLines
        y <- horizontalLines
      } {
          println(s"x0 = ${Point(x, y)}")
          evolution(f, Point(x, y)).map {
            case (t, p) => {
              draw(p)
              p
            }
          } take(2000) toList
        }
    }

    drawGrid()
//    phasePortrait(realPendulum)
//    evolution(realPendulum, Point(0, 3)) take(2000) toList
    phasePortrait(lotkaVolterra)
  }






}
