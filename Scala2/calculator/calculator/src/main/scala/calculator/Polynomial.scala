package calculator

object Polynomial extends PolynomialInterface:
  def computeDelta(a: Signal[Double], b: Signal[Double],
      c: Signal[Double]): Signal[Double] =
    Signal.Var(b()*b() - 4*a()*c())

  def computeSolutions(a: Signal[Double], b: Signal[Double],
      c: Signal[Double], delta: Signal[Double]): Signal[Set[Double]] =
    def solution(_a: Double, _b:Double, _c: Double, _d: Double): Set[Double] ={
      if (_d<0) Set()
      else if (_d == 0) Set(-_b/(2*_a))
      else Set((-_b+math.sqrt(_d))/(2*_a), ((-_b-math.sqrt(_d))/(2*_a)))
    }
    Signal.Var(solution(a(), b(), c(), delta()))
