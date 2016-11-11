package macphail

trait GridOps extends GridCopy {
  type GridOp[A] = Either[String, A]

  def insertAt(world: World, n: Int, position: (Int, Int)): GridOp[World] = {
    val (unsafeX, unsafeY) = position
    val safePos = (clamp(unsafeX), clamp(unsafeY))

    for {
      _ <- checkSpaceIsAvailable(world, safePos)
      _ <- checkNumberIsNotAlreadyInRow(world, n, safePos)
      _ <- checkNumberIsNotAlreadyInColumn(world, n, safePos)
      _ <- checkNumberIsNotAlreadyInSquare(world, n, safePos)
      w <- placeAt(world, n.toString, safePos)
    } yield w
  }

  def eraseAt(world: World, position: (Int, Int)): GridOp[World] = {
    val (unsafeX, unsafeY) = position
    val safePos = (clamp(unsafeX), clamp(unsafeY))

    for {
      _ <- checkIfNotAlreadyBlank(world, safePos)
      _ <- checkIfPositionCanBeErased(world, safePos)
      w <- placeAt(world, Symbols.blank, safePos)
    } yield w
  }

  def checkIfNotAlreadyBlank(world: World, position: (Int, Int)): GridOp[Unit] = {
    val (x, y) = position
    if(world.grid(y)(x) != Symbols.blank)
      Right(())
    else
      Left("This position can't be erased : it's already blank")
  }

  def checkIfPositionCanBeErased(world: World, position: (Int, Int)): GridOp[Unit] = {
    val (x, y) = position
    if(world.startingGrid(y)(x) == Symbols.blank)
      Right(())
    else
      Left("This number can't be erased : it's from the original grid")
  }

  def checkSpaceIsAvailable(world: World, position: (Int, Int)): GridOp[Unit] = {
    val (x, y) = position
    val currentSymbol = world.grid(y)(x)

    currentSymbol match {
      case Symbols.blank   => Right(())
      case other           => Left("This space is already taken")
    }
  }

  def checkNumberIsNotAlreadyInRow(world: World, n: Int, position: (Int, Int)): GridOp[Unit] = {
    val (_, y) = position
    val lineContainsN = world.grid(y).contains(n.toString)
    if(lineContainsN)
      Left(s"Number $n is already in this line")
    else
      Right(())
  }

  def checkNumberIsNotAlreadyInColumn(world: World, n: Int, position: (Int, Int)): GridOp[Unit] = {
    val (x, _) = position
    val rotated = rotate(world.grid)
    val lineContainsN = rotated(x).contains(n.toString)
    if(lineContainsN)
      Left(s"Number $n is already in this column")
    else
      Right(())
  }

  def checkNumberIsNotAlreadyInSquare(world: World, n: Int, position: (Int, Int)): GridOp[Unit] = {
    val (x, y) = position

    def topLeft  = world.grid(0).take(3).toList ++ world.grid(1).take(3).toList ++ world.grid(2).take(3).toList
    def midLeft  = world.grid(3).take(3).toList ++ world.grid(4).take(3).toList ++ world.grid(5).take(3).toList
    def botLeft  = world.grid(6).take(3).toList ++ world.grid(7).take(3).toList ++ world.grid(8).take(3).toList

    def topMid   = world.grid(0).slice(3, 6).toList ++ world.grid(1).slice(3, 6).toList ++ world.grid(2).slice(3, 6).toList
    def mid      = world.grid(3).slice(3, 6).toList ++ world.grid(4).slice(3, 6).toList ++ world.grid(5).slice(3, 6).toList
    def botMid   = world.grid(6).slice(3, 6).toList ++ world.grid(7).slice(3, 6).toList ++ world.grid(8).slice(3, 6).toList

    def topRight = world.grid(0).slice(6, 9).toList ++ world.grid(1).slice(6, 9).toList ++ world.grid(2).slice(6, 9).toList
    def midRight = world.grid(3).slice(6, 9).toList ++ world.grid(4).slice(6, 9).toList ++ world.grid(5).slice(6, 9).toList
    def botRight = world.grid(6).slice(6, 9).toList ++ world.grid(7).slice(6, 9).toList ++ world.grid(8).slice(6, 9).toList

    val containsN = y match {
      case _ if y < 3 && x < 3 => topLeft.contains(n.toString)
      case _ if y < 3 && x < 6 => topMid.contains(n.toString)
      case _ if y < 3 && x < 9 => topRight.contains(n.toString)
      case _ if y < 6 && x < 3 => midLeft.contains(n.toString)
      case _ if y < 6 && x < 6 => mid.contains(n.toString)
      case _ if y < 6 && x < 9 => midRight.contains(n.toString)
      case _ if y < 9 && x < 3 => botLeft.contains(n.toString)
      case _ if y < 9 && x < 6 => botMid.contains(n.toString)
      case _ if y < 9 && x < 9 => botRight.contains(n.toString)
    }

    if(containsN)
      Left(s"Number $n is already in this square")
    else
      Right(())
  }

  def placeAt(world: World, s: String, position: (Int, Int)): GridOp[World] = {
    val grid = copy(world.grid)

    val (x,y) = position
    grid(y)(x) = s.toString

    Right(world.copy(grid = grid))
  }

  def clamp(n: Int): Int = clamp(n, 0, 8)

  def clamp(n: Int, min: Int, max: Int): Int = {
    n.max(min).min(max)
  }

  def rotate(grid: Array[Array[String]]): Array[Array[String]] = {
    val rotated = Array.fill[String](9,9)(Symbols.blank)

    (0 to 8).foreach(row => {
      (0 to 8).foreach(column => {
        rotated(column)(row) = grid(row)(column)
      })
    })

    rotated
  }
  
}
