import scala.swing.Orientation

trait Orientation{
  def left: Orientation
  def right: Orientation
}

case object North extends Orientation{
  override def left = West
  override def right = East
}

case object West extends Orientation{
  override def left = South
  override def right= North
}

case object South extends Orientation{
  override def left = East
  override def right = West
}

case object East extends Orientation{
  override def left = North
  override def right = South
}

case class PlateauSize(x:Int,y:Int)

case class Location(x:Int,y:Int)

case class Position(location:Location,orientation:Orientation) {
  def move(plateauSize: PlateauSize): Position = {
    orientation match {
      case North => Position(Location(location.x, (location.y + plateauSize.y + 1) % plateauSize.y), orientation)
      case West => Position(Location((location.x + plateauSize.x - 1) % plateauSize.x, location.y), orientation)
      case South => Position(Location(location.x, (location.y + plateauSize.y - 1) % plateauSize.y), orientation)
      case East => Position(Location((location.x + plateauSize.x + 1) % plateauSize.x, location.y), orientation)
    }
  }
}

class MyRover(curPos:Position,maxsize: PlateauSize) {
  def getNewPosition(movePattern:String):Position={
    var position: Position =curPos
    val patternArray=movePattern.toCharArray()
    for( singleMove <- patternArray){
      singleMove match {
        case 'L' => position = position.copy(orientation = position.orientation.left)
        case 'R' => position = position.copy(orientation = position.orientation.right)
        case 'M' => position = position.move(maxsize)
      }
    }
    position
  }
}

object Rover {
  def main(args: Array[String]) {
    println("Enter Plateau Size for x and y co-ordinates : ")
    var x=scala.io.StdIn.readInt()
    var y=scala.io.StdIn.readInt()
    val plateausize = new PlateauSize(x+1, y+1)
    println("Enter the current position for Rover : ")
    x=scala.io.StdIn.readInt()
    y=scala.io.StdIn.readInt()
    val face =scala.io.StdIn.readChar()
    val location = new Location(x, y)
    var direction: AnyRef=null
    face match {
      case 'N' => direction=North
      case 'S' => direction=South
      case 'W' => direction=West
      case 'E' => direction=East
    }
    val position = new Position(location, direction.asInstanceOf[Orientation])
    val myRover = new MyRover(position, plateausize)
    println("Enter the moving pattern : ")
    val patternString = scala.io.StdIn.readLine()
    val newlocation = myRover.getNewPosition(patternString)
    println("The new location of Rover :  ")
    print(newlocation)
  }
}
