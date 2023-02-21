import scala.annotation.tailrec
case class RoverState(
    val x: Int,
    val y: Int,
    val orientation: Orientation,
    val lost: Boolean = false
)

case class Grid(
    val gridEdgeX: Int,
    val gridEdgeY: Int
)

enum Orientation:
  case N, E, S, W

enum Action:
  case F, L, R

private def moveRoverForward(state: RoverState): RoverState =
  state.orientation match {
    case Orientation.N => state.copy(y = state.y + 1)
    case Orientation.E => state.copy(x = state.x + 1)
    case Orientation.S => state.copy(y = state.y - 1)
    case Orientation.W => state.copy(x = state.x - 1)
  }

private def outOfGrid(coord: Int, edgeCoord: Int): Boolean =
  coord < 0 || coord > edgeCoord

private def roverLocationUnknown(state: RoverState, grid: Grid): Boolean =
  outOfGrid(state.x, grid.gridEdgeX) || outOfGrid(state.y, grid.gridEdgeY)

private def handleForwardMove(state: RoverState, grid: Grid): RoverState = {
  val newState = moveRoverForward(state)
  if (roverLocationUnknown(newState, grid))
    state.copy(lost = true)
  else
    newState
}

private def rotateRoverLeft(state: RoverState): RoverState =
  state.orientation match {
    case Orientation.N => state.copy(orientation = Orientation.W)
    case Orientation.E => state.copy(orientation = Orientation.N)
    case Orientation.S => state.copy(orientation = Orientation.E)
    case Orientation.W => state.copy(orientation = Orientation.S)
  }

private def rotateRoverRight(state: RoverState): RoverState =
  state.orientation match {
    case Orientation.N => state.copy(orientation = Orientation.E)
    case Orientation.E => state.copy(orientation = Orientation.S)
    case Orientation.S => state.copy(orientation = Orientation.W)
    case Orientation.W => state.copy(orientation = Orientation.N)
  }

private def explore(
    grid: Grid,
    roverState: RoverState,
    action: Action
): RoverState =
  action match {
    case Action.F => handleForwardMove(roverState, grid)
    case Action.L => rotateRoverLeft(roverState)
    case Action.R => rotateRoverRight(roverState)
  }

@tailrec
def run(grid: Grid, roverState: RoverState, actions: List[Action]): RoverState =
  roverState.lost match {
    case true => roverState
    case false =>
      actions match {
        case action :: tail =>
          run(grid, explore(grid, roverState, action), tail)
        case Nil => roverState
      }
  }

private def convertCharToAction(charInput: Char): Action =
  charInput match {
    case 'F' => Action.F
    case 'L' => Action.L
    case 'R' => Action.R
    case _ => {
      println(s"Error with action input $charInput")
      throw Error("Unsupported Rover Action")
    }
  }

private def formatReport(roverState: RoverState): String =
  s"(${roverState.x}, ${roverState.y}, ${roverState.orientation})".concat(
    if (roverState.lost) {
      " LOST"
    } else ""
  )

def runWithReport(
    gridEdgeX: Int,
    gridMaxY: Int,
    roverState: RoverState,
    actions: String
): String =
  formatReport(
    run(
      Grid(gridEdgeX, gridMaxY),
      roverState,
      actions.map(convertCharToAction(_)).toList
    )
  )

case class TestInput(
    val initialState: RoverState,
    val grid: Grid,
    val actions: List[Action],
    val expectedState: RoverState
)

val basicActionTests = List(
  TestInput(
    initialState = RoverState(0, 0, Orientation.E),
    grid = Grid(0, 0),
    actions = List(),
    expectedState = RoverState(0, 0, Orientation.E)
  ),
  TestInput(
    initialState = RoverState(0, 0, Orientation.E),
    grid = Grid(0, 0),
    actions = List(Action.L),
    expectedState = RoverState(0, 0, Orientation.N)
  ),
  TestInput(
    initialState = RoverState(0, 0, Orientation.E),
    grid = Grid(0, 0),
    actions = List(Action.R),
    expectedState = RoverState(0, 0, Orientation.S)
  ),
  TestInput(
    initialState = RoverState(0, 0, Orientation.E),
    grid = Grid(1, 0),
    actions = List(Action.F),
    expectedState = RoverState(1, 0, Orientation.E)
  ),
  TestInput(
    initialState = RoverState(0, 0, Orientation.E),
    grid = Grid(0, 0),
    actions = List(Action.F),
    expectedState = RoverState(0, 0, Orientation.E, true)
  ),
  TestInput(
    initialState = RoverState(0, 0, Orientation.E),
    grid = Grid(1, 0),
    actions = List(Action.F, Action.F, Action.L, Action.L, Action.F, Action.F),
    expectedState = RoverState(1, 0, Orientation.E, true)
  )
)

case class TestReportInput(
    val initialState: RoverState,
    val grid: Grid,
    val actions: String,
    val expectedResponse: String
)

val exampleTests = List(
  TestReportInput(
    initialState = RoverState(2, 3, Orientation.E),
    grid = Grid(4, 8),
    actions = "LFRFF",
    expectedResponse = "(4, 4, E)"
  ),
  TestReportInput(
    initialState = RoverState(0, 2, Orientation.N),
    grid = Grid(4, 8),
    actions = "FFLFRFF",
    expectedResponse = "(0, 4, W) LOST"
  ),
  TestReportInput(
    initialState = RoverState(2, 3, Orientation.N),
    grid = Grid(4, 8),
    actions = "FLLFR",
    expectedResponse = "(2, 3, W)"
  ),
  TestReportInput(
    initialState = RoverState(1, 0, Orientation.S),
    grid = Grid(4, 8),
    actions = "FFRLF",
    expectedResponse = "(1, 0, S) LOST"
  ),
  TestReportInput(
    initialState = RoverState(5, 5, Orientation.W),
    grid = Grid(5, 6),
    actions = "FFLFFRFFLLL",
    expectedResponse = "(1, 3, N)"
  ),
  TestReportInput(
    initialState = RoverState(0, 0, Orientation.N),
    grid = Grid(5, 5),
    actions = "FFFFRFFFFRFFFFRFFFFR",
    expectedResponse = "(0, 0, N)"
  ),
  TestReportInput(
    initialState = RoverState(0, 0, Orientation.N),
    grid = Grid(5, 5),
    actions = "FFFFFFRRFF",
    expectedResponse = "(0, 5, N) LOST"
  )
)

basicActionTests.foreach { test =>
  val output = run(test.grid, test.initialState, test.actions)
  if (output != test.expectedState)
    println(
      s"Basic Action Test Failed. Expected ${test.expectedState}. Final State: $output"
    )
}

exampleTests.foreach { test =>
  val output = runWithReport(
    test.grid.gridEdgeX,
    test.grid.gridEdgeY,
    test.initialState,
    test.actions
  )

  if (output != test.expectedResponse) {
    println(
      s"Error Running Test. Expected: ${test.expectedResponse}. Actual: $output"
    )
  }

  println(output)
}