# 比如在Intersect下写的， 可以默认eval_prog使用的是已经preprocess的，因为调用时e.preprocess_prog.eval_prog
  def eval_prog env
    e1 = @e1.preprocess_prog.eval_prog(env)
    e2 = @e2.preprocess_prog.eval_prog(env)
    e1.intersect(e2)
  end

# in LineSegment 可以和我的对比一下
  def preprocess_prog
    if real_close_point(@x1,@y1,@x2,@y2)
      Point.new(@x1,@y1)
    elsif real_close(@x1,@x2)
      if @y1 > @y2
        LineSegment.new(@x2,@y2,@x1,@y1)
      else 
        self
      end
    elsif @x1 > @x2
      LineSegment.new(@x2,@y2,@x1,@y1)
    else
      self
    end
  end
# 其他人的，我想应该更好
  def preprocess_prog
    if real_close_point(x1,y1,x2,y2)
      Point.new(x1,y1)
    elsif x1 - x2 > GeometryExpression::Epsilon || (real_close(x1,x2) && y1 - y2 > GeometryExpression::Epsilon)
      LineSegment.new(x2,y2,x1,y1)
    else self
    end
  end
# 小小的细节 .to_f (IN Line, intersectLine)
   else # one-point intersection
      x = (line.b - b).to_f / (m - line.m)
      y = m * x + b
      Point.new(x,y)
    end

# 这样写就更好
  private
  def inbetween(v,end1,end2)
    eps = GeometryExpression::Epsilon
    ((end1 - eps <= v && v <= end2 + eps) || 
     (end2 - eps <= v && v <= end1 + eps))
  end


# 从其他人那里看到的精彩写法
      if real_close(x1, x2) # the segments are one a vertical line
        aXstart, aYstart, aXend, aYend,
        bXstart, bYstart, bXend, bYend = y1 < seg.y1 ? seg1 + seg2 : seg2 + seg1
# in LineSegment
  def intersectWithSegmentAsLineResult seg 
    # the "hard case in the hard case" where self and seg are segments
    # on the same line (which could be vertical or not) and the segments
    # could be disjoint, overlapping, one inside the other or just touching
    if real_close(@x1,@x2)
      # the segments are on a vertical line
      # let segment a start at or below start of segment b
      aXstart,aYstart,aXend,aYend,bXstart,bYstart,bXend,bYend = 
        if @y1 < seg.y1
          [@x1,@y1,@x2,@y2,seg.x1,seg.y1,seg.x2,seg.y2]
        else
          [seg.x1,seg.y1,seg.x2,seg.y2,@x1,@y1,@x2,@y2]
        end
      if real_close(aYend,bYstart)
        Point.new(aXend,aYend) # just touching
      elsif aYend < bYstart
        NoPoints.new # disjoint
      elsif aYend > bYend
        LineSegment.new(bXstart,bYstart,bXend,bYend) # b inside a 
      else 
        LineSegment.new(bXstart,bYstart,aXend,aYend) # overlapping 
      end
    else
      # the segments are on a non-vertical line
      # let segment a start at or to the left of start of segment b
      aXstart,aYstart,aXend,aYend,bXstart,bYstart,bXend,bYend = 
        if @x1 < seg.x1
          [@x1,@y1,@x2,@y2,seg.x1,seg.y1,seg.x2,seg.y2]
        else
          [seg.x1,seg.y1,seg.x2,seg.y2,@x1,@y1,@x2,@y2]
        end
      if real_close(aXend,bXstart)
        Point.new(aXend,aYend) # just touching
      elsif aXend < bXstart
        NoPoints.new # disjoint
      elsif aXend > bXend
        LineSegment.new(bXstart,bYstart,bXend,bYend) # b inside a 
      else 
        LineSegment.new(bXstart,bYstart,aXend,aYend) # overlapping 
      end
    end
  end

# 还有一个很弱智的错误 Point里的intersectwithSe l R, inbetween: y2写成y1了
# 还有一个错误是Line里的intersectLine 算法写错了应是self.m - line.m 