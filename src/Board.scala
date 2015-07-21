// Package
object data {
  val boardSize = 8

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
  trait Vector2D {
    def x: Int
    def y: Int
  }
   */
  case class Vector2D(x: Int, y: Int) {
    def +(v: Vector2D) = Vector2D(x + v.x, y + v.y)
    def *(i: Int) = Vector2D(x * i, y * i)
    def reverse = this * -1
  }

  type Displacement = Vector2D
  type Pos = Vector2D

  // utils
  def colorToDir(c: Color) = c match {
    case White => 1
    case Black => -1
  }
  def dirsAndRev(dirs: Seq[Displacement]) =
    dirs ++ dirs map (_.reverse)

  val diagDirs = dirsAndRev(Seq(Vector2D(1, 1), Vector2D(1, -1)))
  val nonDiagDirs = dirsAndRev(Seq(Vector2D(0, 1), Vector2D(1, 0)))
  val allDirs = diagDirs ++ nonDiagDirs

  //This is a piece, not located in space.
  //This piece does not have identity, so it does not allow distinguishing two
  //pieces of the same color.
  //Does equality make sense on this type?
  case class Piece(pieceType: PieceT, color: Color) {
    def possibleDispl: Seq[Displacement] =
      pieceType match {
        case Pawn =>
          //XXX what about initial moves?
          Seq(Vector2D(0, 1) * colorToDir(color))
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
  //A piece located on the board. Structural equality of two pieces on the same
  //BoardState implies they're equal.
  case class LocatedPiece(piece: Piece, pos: Pos)

  sealed trait Move
  //A simple move
  case class NormalMove(p: Piece, src: Pos, dst: Pos, isCapture: Boolean) extends Move
  //TODO:
  //- Rooks.
  //- En-passant captures.
  // They also require crosscutting handling elsewhere.

  trait BoardState {
    // Return the sequence of pieces on the board. This is just one possible view.
    def pieces: Set[LocatedPiece]
    //Finds the piece in a given position (if any).
    def piece(p: Pos): Option[Piece]
    //Produces the new state resulting from a given move. The move is supposed to be legal.
    def toNewState(m: Move): BoardState
  }
  class BoardStateI(val pieces: Set[LocatedPiece]) extends BoardState {
    private val piecesArray: Array[Array[Option[Piece]]] = Array.fill(boardSize)(Array.fill(boardSize)(None))
    for (LocatedPiece(piece, Vector2D(x, y)) <- pieces) {
      piecesArray(x)(y) = Some(piece)
    }
    //Finds the piece in a given position (if any).
    def piece(p: Pos): Option[Piece] = p match {
      case Vector2D(x, y) => piecesArray(x)(y)
    }
    def toNewState(m: Move): BoardState = m match {
      case NormalMove(p, src, dst, isCapture) =>
        val dstPiece = piece(dst)
        //We can't restrict statically the move to be valid, so we just assert
        //the condition we need for correctness.
        assert(dstPiece.nonEmpty == isCapture)
        val newPieces = pieces - LocatedPiece(p, src) + LocatedPiece(p, dst) -- (dstPiece map (LocatedPiece(_, dst)))
        new BoardStateI(newPieces)
    }
  }
  //XXX: Checking legality depends on properties of the game history, because
  //of en-passant captures and because of rooks.
  def isLegal(m: Move, b: BoardState) = ???
}
