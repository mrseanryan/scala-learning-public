package week5

import scala.annotation.tailrec

import Utils._

object TryOutLawsOfFP {
    /**
      * FP follows mathematical laws, so can prove correctness.
      *
      * - can prove assumption of commutation, association
      * - can use induction
      * 
      * Mainly due to immutability.
      */

    /** Laws of concat (++ operation)
     * 
     * - associative
     * (xs ++ ys) ++ zs = xs ++ (ys ++ zs)
     * 
     * - allows Nil as a neutral element on the left, or on the right
     * xs ++ Nil = xs
     * Nil ++ xs = xs
     * 
     * How to prove:
     * - structural induction
     */
    def factorial(n:Int):Int = {
      if (n==0)
      1
      else
      n * factorial(n-1)
    }
  
    /** Natural induction:
    * 
    * To prove P(n) for all the integers n >= b:
    * 
    * - prove for P(b) [base case]
    * - prove for P(n)
    * - prove for P(n+1)
    */
    // Show that for all n>=4:
    // factorial(n) >= power(2, n)
    //
    // True for n=4: [base case, by calculation]
    // factorial(4) = 4 * 3 * 2 [24] > 2^4 [16]
    //
    // Assume true for n.
    //
    // For n+1: [the induction step]
    //
    // factorial(n+1) = (n+1) * n * (n-1) * (n-2) .. 1 [(n+1) * n!] > 2^(n+1) [2 * 2^n]
    // - we know that n! > 2^n (assumed true for n)
    // - and n+1 > 2
    // so is true for n+1

    /* Referential Transparency
    *
    * - FP no side effects
    * - so, a term is equivalent to the term to which it reduces 
    */

    /** Structural Induction: (like Natural induction)
    * 
    * To prove a property P(xs) for all lists xs
    * 
    * - show that P(Nil) holds [the base case]
    * - assume P(xs) holds
    * - show that P(x :: xs) holds [the induction step]
    */
    def concat[T](xs: List[T], ys: List[T]): List[T] = xs match {
      case Nil => ys
      case x :: xs1 => x :: concat(xs1, ys)
    }
    /* To prove - concat is associative
    * (xs ++ ys) ++ zs = xs ++ (ys ++ zs)
    * 
    * From `def concat` -> defining clauses of ++
    * 1- Nil ++ ys = ys                      -- [from case Nil => ys]
    * 2- x :: xs1 ++ ys = x :: (xs1 ++ ys)   -- [from case x :: xs1 => x :: concat(xs1, ys)]
    * 
    * Base case: Nil
    * - (Nil ++ ys) ++ zs = Nil ++ (ys ++ zs)
    * = ys ++ zs -- [from 1]  =   (ys ++ zs)    -- [proved the base case]
    * 
    * Induction step 'xs -> x :: xs' (assuming is true for 'xs')
    * LHS:
    * ( (x :: xs) ++ ys) ++ zs
    * = (x :: (xs ++ ys)) ++ zs -- [from 2] - 'moving braces'
    * = x :: ((xs ++ ys) ++ zs) -- [from 2 again] - 'moving braces'
    * = x :: (xs ++ (ys ++ zs)) -- [applying the hypothesis we are testing]
    * 
    * RHS:
    * (x :: xs) ++ (ys ++ zs)
    * = x :: (xs ++ (ys ++ zs)) -- [from 2] - 'moving braces'
    * = LHS -- [proved the induction 'xs => x :: xs' case]
    */

    /* To prove - reverse (the inneficient version, which is easier to prove)
    * From the code:
    * -1 Nil.reverse = Nil  [1st clause]
    * -2 (x :: xs).reverse = xs.reverse ++ List(x)  [2nd clause]
    * 
    * To prove:
    * xs.reverse.reverse = xs 
    *
    * Base case: Nil
    * Nil.reverse.reverse = Nil
    * => Nil.reverse [from 1]
    * => Nil [from 1] [proves the base case]
    * 
    * Induction step 'xs -> x :: xs' (assuming is true for 'xs')
    * (x :: xs).reverse.reverse = (x :: xs)
    * LHS
    * => (xs.reverse ++ List(x)).reverse  [from 2]
    * RHS = x :: xs
    * => x :: xs.reverse.reverse -- [applying the hypothesis we are testing
    * Both sides simplify to different expressions :-( by induction
    * 
    * Try via generalizing the equation: (auxiliary equation)
    * xs.reverse => ys:
    * (ys ++ List(x)).reverse = x :: ys.reverse
    * Base case: ys => Nil
    * => (Nil ++ List(x)).reverse = x :: Nil.reverse
    * => List(x).reverse = x :: Nil
    * => List(x).reverse = x [proven - single item list]
    * 
    * Then:
    * ( (y :: ys) ++ List(x) ).reverse = x :: (y :: ys).reverse
    * LHS:
    * => (y :: (ys ++ List(x))).reverse // by 2nd clause of ++ (concat)
    * => (y :: (ys :: x)).reverse // List(x) -> x
    * => (x :: (y :: ys)).reverse // swapping order (we know :: is commutative)
    * 
    * or by fold/unfold method
    */

    /* To prove (difficult)
    *
    * Prove the following distribution law for map over concatenation:
    * 
    * For any lists xs, ys and function f:
    * 
    * (xs ++ ys) map f = (xs map f) ++ (ys map f)
    * 
    * - hint: use the clauses of ++ as well as:
    * - Nil map f = Nil
    * - (x :: xs) map f = f(x) :: (xs map f)
    * 
    *  Base case: xs = Nil
    * (Nil ++ ys) map f = (Nil map f) ++ (ys map f)
    * => ys map f = ys map f [proven]
    * 
    * Induction step 'xs -> x :: xs' (assuming is true for 'xs')
    * ((x :: xs) ++ ys) map f = ((x :: xs) map f) ++ (ys map f)
    * RHS
    * => (f(x) :: (xs map f)) ++ (ys map f)
    * => (f(x) :: (xs ++ ys) map f)
    * => (f(x) :: xs) ++ ys) map f) [:: and ++ are associative]
    * => (x :: xs) ++ ys) map f [if f(x) => x, then proven]
    */ 

   def go(): Unit = {
    Utils.banner("Week 5 - laws of FP")
  }
}
