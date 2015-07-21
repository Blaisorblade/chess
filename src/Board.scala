// Package
object data {

  //Color enumeration
  trait Color
  case object Black extends Color
  case object White extends Color

  //Piece enumeration
  class PieceT(name: Option[Char])
  case object Pawn extends PieceT(None)
  case object Bishop extends PieceT(Some('B'))
  case object Knight extends PieceT(Some('N'))
  case object Rook extends PieceT(Some('R'))
  case object Queen extends PieceT(Some('Q'))
  case object King extends PieceT(Some('K'))

  //XXX reuse some implementation?
  /*
  trait Vector {
    def x: Int
    def y: Int
  }
   */
  case class Vector(x: Int, y: Int) {
    def +(v: Vector) = Vector(x + v.x, y + v.y)
    def *(i: Int) = Vector(x * i, y * i)
    def reverse = this * -1
  }

  type Displacement = Vector
  type Pos = Vector

  // utils
  def colorToDir(c: Color) = c match {
    case White => 1
    case Black => -1
  }
  def dirsAndRev(dirs: Seq[Displacement]) =
    dirs ++ dirs map (_.reverse)

  val diagDirs = dirsAndRev(Seq(Vector(1, 1), Vector(1, -1)))
  val nonDiagDirs = dirsAndRev(Seq(Vector(0, 1), Vector(1, 0)))
  val allDirs = diagDirs ++ nonDiagDirs

  case class Piece(pieceType: PieceT, color: Color) {
    def possibleDispl: Seq[Displacement] =
      pieceType match {
        case Pawn =>
          //XXX what about initial moves?
          Seq(Vector(0, 1) * colorToDir(color))
        case King =>
          allDirs
        //What about possible repeated movements?
        case Bishop =>
          diagDirs
        case Rook =>
          nonDiagDirs
        case Queen =>
          allDirs
      }
  }

  type Move

  trait BoardState {
    def pieces: Seq[(Piece, Pos)]
    def toNewState(m: Move): BoardState
  }
}
