# University of Washington, Programming Languages, Homework 6, hw6runner.rb

# This is the only file you turn in, so do not modify the other files as
# part of your solution.

class MyPiece < Piece
  # The constant All_My_Pieces should be declared here

  # class array holding all the pieces and their rotations
  All_Pieces = 
  [
  [[[0, 0], [1, 0], [0, 1], [1, 1]]],  # square (only needs one)
  rotations([[0, 0], [-1, 0], [1, 0], [0, -1]]), # T
  [[[0, 0], [-1, 0], [1, 0], [2, 0]], # long (only needs two)
  [[0, 0], [0, -1], [0, 1], [0, 2]]],
  rotations([[0, 0], [0, -1], [0, 1], [1, 1]]), # L
  rotations([[0, 0], [0, -1], [0, 1], [-1, 1]]), # inverted L
  rotations([[0, 0], [-1, 0], [0, -1], [1, -1]]), # S
  rotations([[0, 0], [1, 0], [0, -1], [-1, -1]]), # Z
  rotations([[0, 0], [-1, 0], [1, 0], [-1, 1], [0, 1]]), # new piece 1
  [[[0, 0], [-1, 0], [-2, 0], [1, 0], [2, 0]], # new piece 2 (only needs two)
  [[0, 0], [0, -1], [0, -2], [0, 1], [0, 2]]],
  rotations([[0, 0], [1, 0], [0, 1]]) # new piece 3
  ]

end

class MyBoard < Board

  attr_accessor :cheat

  def initialize(game)
    super(game)
    @cheat = false
  end

  # rotates the current piece 180 degrees
  def rotate_180
    if !game_over? and @game.is_running?
      @current_block.move(0, 0, 2)
    end
    draw
  end

  def next_piece
    if @cheat and @score >= 100
      @current_block = Piece.new([[[0, 0]]], self)
      @score -= 100
    else
      @current_block = Piece.next_piece(self)
    end
    @current_pos = nil
    @cheat = false
  end
end

class MyTetris < Tetris

  def key_bindings  
    @root.bind('n', proc {self.new_game}) 

    @root.bind('p', proc {self.pause})
    
    @root.bind('q', proc {exitProgram})
    
    @root.bind('a', proc {@board.move_left})
    @root.bind('Left', proc {@board.move_left}) 
    
    @root.bind('d', proc {@board.move_right})
    @root.bind('Right', proc {@board.move_right}) 

    @root.bind('s', proc {@board.rotate_clockwise})
    @root.bind('Down', proc {@board.rotate_clockwise})

    @root.bind('w', proc {@board.rotate_counter_clockwise})
    @root.bind('Up', proc {@board.rotate_counter_clockwise}) 
    
    @root.bind('space' , proc {@board.drop_all_the_way}) 
    @root.bind('u', proc {@board.rotate_180})
    @root.bind('c', proc{@board.cheat = true})
  end

  # creates a canvas and the board that interacts with it
  def set_board
    @canvas = TetrisCanvas.new
    @board = MyBoard.new(self)
    @canvas.place(@board.block_size * @board.num_rows + 3,
                  @board.block_size * @board.num_columns + 6, 24, 80)
    @board.draw
  end

end


