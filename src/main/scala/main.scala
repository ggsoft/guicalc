import calc.Calc
import scala.swing._
import javax.swing.ImageIcon

object Main {

  val ta1,ta2 = new TextArea
  ta1.rows = 8
  ta1.border = Swing.EmptyBorder(5, 5, 5, 5)
  ta2.border = ta1.border

  val bt1 = new Button(Action("Calculate"){
    val expr = ta1.text.trim
    if (expr!="") {
      Calc.calculate(expr)
      if (ta2.text != "") ta2.text += '\n'
      ta2.text += (if (Calc.isError) Calc.errorStr else expr +" = "+Calc.result)
    }
  })

  val bt2 = new Button(Action("Clear") {
    ta2.text = ""
  })

  bt1.icon = new ImageIcon(getClass.getResource("/images/play.png"))
  bt2.icon = new ImageIcon(getClass.getResource("/images/stop.png"))

  val frame = new MainFrame {
    title = "Calculator"
    import BorderPanel.Position._
    contents = new BorderPanel {
      layout += new BorderPanel {
        layout += new BoxPanel(Orientation.Horizontal) {
          border = Swing.EmptyBorder(3, 5, 3, 0)
          contents += bt1
          contents += bt2
        } -> South
        layout += ta1 -> Center
      } -> North
      layout += ta2 -> Center
    }
    size = new Dimension(600,500)
    centerOnScreen
    ta1.text = "(5 * (7 + 8) + 25)^(1/2)"
  }

  def main(args: Array[String]): Unit = {
    frame.visible = true
  }
}