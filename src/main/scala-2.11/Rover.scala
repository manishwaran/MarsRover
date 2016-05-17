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
    val plateausize = new PlateauSize(5+1, 5+1)
    val location = new Location(1, 2)
    val position = new Position(location, North)
    val myRover = new MyRover(position, plateausize)
    val newlocation = myRover.getNewPosition("LMLMLMLMM")
    print("new location of the rover : ")
    print(newlocation)
  }
}
