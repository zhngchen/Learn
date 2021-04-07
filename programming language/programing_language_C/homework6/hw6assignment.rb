# University of Washington, Programming Languages, Homework 6, hw6runner.rb

# This is the only file you turn in, so do not modify the other files as
# part of your solution.

class MyPiece < Piece
  # The constant All_My_Pieces should be declared here

  All_My_Pieces = [[[[0, 0], [1, 0], [0, 1], [1, 1]]],  # square (only needs one)
                  rotations([[0, 0], [-1, 0], [1, 0], [0, -1]]), # T
                  [[[0, 0], [-1, 0], [1, 0], [2, 0]], # long (only needs two)
                  [[0, 0], [0, -1], [0, 1], [0, 2]]],
                  rotations([[0, 0], [0, -1], [0, 1], [1, 1]]), # L
                  rotations([[0, 0], [0, -1], [0, 1], [-1, 1]]), # inverted L
                  rotations([[0, 0], [-1, 0], [0, -1], [1, -1]]), # S
                  rotations([[0, 0], [1, 0], [0, -1], [-1, -1]]), # Z
                  rotations([[0, 0], [1, 0], [0, 1], [1, 1], [-1, 0]]),
                  [[[-1, 0], [-2, 0], [0, 0], [1, 0], [2, 0]],
                   [[0, -1], [0, -2], [0, 0], [0, 1],
                   [0, 2]]],
                  rotations([[0, 0], [0, 1], [1, 0]])]

  # your enhancements here
  def self.next_piece (board)
    MyPiece.new(All_My_Pieces.sample, board)
  end

  My_Cheat_Piece = [[[0, 0]]]
end

class MyBoard < Board
  # your enhancements here
  def initialize (game)
    super(game)
    @current_block = MyPiece.next_piece(self)
    @ischeat = false
  end

  def rotate_180
    if !game_over? and @game.is_running?
      @current_block.move(0, 0, 2)
    end
    draw
  end

  def next_piece
    if @ischeat
      @current_block = MyPiece.new(MyPiece::My_Cheat_Piece, self)
      @ischeat = false
    else
      @current_block = MyPiece.next_piece(self)
    end

    @current_pos = nil
  end
  # 3 => locations.size - 1
  def store_current
  locations = @current_block.current_rotation
  displacement = @current_block.position
  (0..(locations.size-1)).each{|index|  # modify
    current = locations[index];
    @grid[current[1]+displacement[1]][current[0]+displacement[0]] =
    @current_pos[index]
  }
  remove_filled
  @delay = [@delay - 2, 80].max
  end
  # press 'c'(once or repeatedly) only change the @ischeat(that will change the next piece)
  def cheat
    if score > 100  # FIXME  >= (Minor error)
      # only do this once
      if not @ischeat
        @score -= 100
        @game.update_score
      end
      @ischeat = true
    end
  end
end

class MyTetris < Tetris
  # your enhancements here
  # 直接调用super, 会有多余的操作，画出新的Piece
  # cannot call super()
  def set_board
    @canvas = TetrisCanvas.new
    @board = MyBoard.new(self)      # modify, board=> MyBoard
    @canvas.place(@board.block_size * @board.num_rows + 3,
                  @board.block_size * @board.num_columns + 6, 24, 80)
    @board.draw
  end

  def key_bindings
    super()
    @root.bind('u', proc {@board.rotate_180})
    @root.bind('c', proc {@board.cheat})
  end


end

