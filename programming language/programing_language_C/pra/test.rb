class A
  def initialize a
    @arr = a
  end
  def get i
    @arr[i]
  end
  def sum
    @arr.inject(0) {|acc,x| acc + x}
  end
end

class B < A
  def initialize a
    super
    @ans = false
  end
  def sum
    if !@ans
      @ans = @arr.inject(0) {|acc,x| acc + x}
    end
    @ans
  end
end

v = [4,19,74]
a = A.new v
b = B.new v
s1 = a.sum
s2 = b.sum
puts s1
puts s2
v[0] = 0
s3 = a.sum
s4 = b.sum
puts s3
puts s4
