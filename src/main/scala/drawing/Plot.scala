package drawing

import org.scalajs.dom
import org.scalajs.dom.html._

import scala.scalajs.js.annotation.JSExport

/**
 * Created by pierangeloc on 6-4-15.
 */
case class DrawingWindow(canvas: Canvas, centerX: Int, centerY: Int, halfWidth: Int, halfHeight: Int) {
  val deltaX = halfWidth * 2.0 / canvas.width
  val deltaY = halfHeight * 2.0 / canvas.height
}

case class Plot(val drawingWindow: DrawingWindow)
{
  val canvas = drawingWindow.canvas
  //a point in the canvas
  sealed case class CanvasPoint(x: Int, y: Int)


    /**
     * GENERAL PLOTTING FEATURES
     */


    val renderer = canvas.getContext("2d")
      .asInstanceOf[dom.CanvasRenderingContext2D]


    //a representation of the window of R2 we want the canvas to show. Center and sizes are integers to make things more standard

    def drawGrid() = {
      val verticalLines = (drawingWindow.centerX - drawingWindow.halfWidth) to (drawingWindow.centerX + drawingWindow.halfWidth)
      val horizontalLines = (drawingWindow.centerY - drawingWindow.halfHeight) to (drawingWindow.centerY + drawingWindow.halfHeight)
      renderer.font = "normal normal 20px Verdana"

      for {
        x <- verticalLines
      } {
        if(x % 5 == 0) {
          val canvasPosition = point2CanvasPoint(Point(x, 0))
          //            renderer.fillText(x.toString, canvasPosition.x, canvasPosition.y + 20)
          drawLine(Point(x, drawingWindow.centerY - drawingWindow.halfHeight), Point(x, drawingWindow.centerY + drawingWindow.halfHeight), "red", 0.3)
        } else {
          drawLine(Point(x, drawingWindow.centerY - drawingWindow.halfHeight), Point(x, drawingWindow.centerY + drawingWindow.halfHeight), "blue", 0.1)
        }
      }

      for {
        y <- horizontalLines
      } {
        if(y % 5 == 0) {
          val canvasPosition = point2CanvasPoint(Point(0, y))
          //            renderer.fillText(y.toString, canvasPosition.x, canvasPosition.y)
          drawLine(Point(drawingWindow.centerX - drawingWindow.halfWidth, y), Point(drawingWindow.centerX + drawingWindow.halfWidth, y), "red", 0.3)
        } else {
          drawLine(Point(drawingWindow.centerX - drawingWindow.halfWidth, y), Point(drawingWindow.centerX + drawingWindow.halfWidth, y), "blue", 0.1)
        }
      }
    }


    def point2CanvasPoint(p: Point): CanvasPoint = {
      val windowLeftEdge = drawingWindow.centerX - drawingWindow.halfWidth
      val windowUpEdge = drawingWindow.centerY + drawingWindow.halfHeight

      CanvasPoint(((p.x - windowLeftEdge) / drawingWindow.deltaX).toInt, (-(p.y - windowUpEdge) / drawingWindow.deltaY).toInt)
    }


    def draw(c: Point, fillStyle: String = "black", dotSize: Double = 1.0) = {
      renderer.fillStyle = fillStyle
      drawDot(point2CanvasPoint(c)) { p =>
        renderer.beginPath()
        renderer.arc(p.x, p.y, dotSize, 0, 2 * Math.PI)
        renderer.fill
      }
    }

    def drawDot(p: CanvasPoint)(f: CanvasPoint => Unit) = f(p)

    def drawLine(from: Point, to: Point, strokeStyle: String = "black", lineWidth: Double = 2.0) = {
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
}
