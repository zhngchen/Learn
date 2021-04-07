# University of Washington, Programming Languages, Homework 6, hw6runner.rb

# This is the only file you turn in, so do not modify the other files as
# part of your solution.

class MyPiece < Piece
  # The constant All_My_Pieces should be declared here
  All_My_Pieces = All_Pieces + [[[[0, 0], [-1, 0], [-2, 0], [1, 0], [2, 0]], # longer (only needs two)
                                  [[0, 0], [0, -1], [0, -2], [0, 1], [0, 2]]],
                                 rotations([[-1, 0], [0, 0], [-1, 1], [0, 1], [1, 1]]), # fat short L
                                 rotations([[0, 0], [0, 1], [1, 1]])] # skinny short L
                                
  # your enhancements here
                                
  # class method to choose the next piece
  def self.next_piece (board)
    MyPiece.new(All_My_Pieces.sample, board)
  end
end

class MyBoard < Board
  # your enhancements here

  def initialize (game)
    super(game)
    @cheat = false
  end

  # gets the information from the current piece about where it is and uses this
  # to store the piece on the board itself.  Then calls remove_filled.
  def store_current
    locations = @current_block.current_rotation
    point_length = @current_block.current_rotation.length
    displacement = @current_block.position
    (0..(point_length - 1)).each{|index| 
      current = locations[index];
      @grid[current[1]+displacement[1]][current[0]+displacement[0]] = 
      @current_pos[index]
    }
    remove_filled
    @delay = [@delay - 2, 80].max
  end
  
  def rotate_180
    rotate_clockwise
    rotate_clockwise
  end

  # gets the next piece
  def next_piece
    if @cheat && @score >= 100
      @score -= 100
      @current_block = MyPiece.new([[[0, 0]]], self)
    else
      @current_block = MyPiece.next_piece(self)
    end        
    @current_pos = nil
    @cheat = false
  end

  def set_cheat
    @cheat = true
  end
end

class MyTetris < Tetris
  # your enhancements here

  # creates a canvas and the board that interacts with it
  def set_board
    @canvas = TetrisCanvas.new
    @board = MyBoard.new(self)
    @canvas.place(@board.block_size * @board.num_rows + 3,
                  @board.block_size * @board.num_columns + 6, 24, 80)
    @board.draw
  end
  
  def key_bindings
    super
    @root.bind('u', proc {@board.rotate_180})
    @root.bind('c', proc {@board.set_cheat})
  end
end


