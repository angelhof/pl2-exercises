/*@ predicate untilHere{L}(integer k1, integer k2, integer i, int *x, integer best, integer best_t, integer temp) = 
  @   k2 >= i ? best == best_t :
  @     \at(x[k2], L) == \at(x[k1], L) ? untilHere(k1, k2+1, i, x, best, best_t, temp+1) :
  @       temp > best_t ? untilHere(k2, k2+1, i, x, best, temp, 1) :         
  @                       untilHere(k2, k2+1, i, x, best, best_t, 1);
  @*/

/*@ predicate smaller{L}(integer k1, integer k2, integer i, int *x, integer max, integer best) =
  @   k2 >= i ? best >= max :
  @     \at(x[k2], L) == \at(x[k1], L) ? smaller(k1, k2+1, i, x, max+1, best) :
  @       best >= max;
  @*/

/*@ predicate equal{L}(integer k1, integer k2, integer i, int *x, integer max, integer best) =
  @   k2 >= i ? best == max :
  @     \at(x[k2], L) == \at(x[k1], L) ? equal(k1, k2+1, i, x, max+1, best) :
  @       best == max;
  @*/

/*@ predicate all_equal{L}(integer k1, integer k2, int *x, int N) =
  @   k2 -1 > k1 ? \at(x[k1], L) == \at(x[k2-1], L) && all_equal(k1+1, k2, x, N) :
  @     k2 == N ? 1 > 0 :
  @       \at(x[k2 - 1], L) != \at(x[k1], L);
  @*/



/*@ requires 1 <= N <= 1000000;
  @ requires \valid(x + (0..N-1));
  @ ensures \forall integer k1, k2; 
  @   0 <= k1 <= k2 < N && 
  @   (\forall integer k; k1<= k <= k2 ==> x[k] == x[k1]) ==> 
  @   (
  @     k2 - k1 + 1 <= \result
  @   );
  @ ensures \exists integer k1, k2; 
  @   0 <= k1 <= k2 < N && 
  @   (\forall integer k; k1<= k <= k2 ==> x[k] == x[k1]) && 
  @   (
  @     k2 - k1 + 1 == \result
  @   );
  @*/
int countSameConsecutive(int N, int x[]) {
  int best = 0, i = 0;
  /*@ ghost int i_pre = 0; */
  /*@ ghost int pre_best = 0; */
  /*@ loop invariant 0 <= i <= N;
    @ loop invariant i-i_pre <= best <= i;
    @ loop invariant pre_best <= best;
    @ loop invariant \forall integer k; i_pre <= k < i  ==> x[k] == x[i_pre];
    @ loop invariant i-i_pre > pre_best ==> best == i-i_pre;
    @ loop invariant i-i_pre <= pre_best ==> best == pre_best;
    @ loop invariant 0 < i < N  ==> x[i_pre] != x[i];
    @ loop invariant 0 < i < N ==> x[i] != x[i-1];

    @ loop assigns i, best, i_pre, pre_best;
    @ loop variant N-i;
    @*/
  while (i < N) {
    int j = i+1;
    /*@ loop invariant i+1 <= j <= N;
      @ loop invariant \forall integer k; i <= k < j ==> x[k] == x[i];
      @ loop assigns j;
      @ loop variant N-j;
      @*/
    while (j < N && x[j] == x[i]) ++j;
    /*@ assert j < N ==> \forall integer k; i <= k < j ==> x[j] != x[k]; */ 
    
    /*@ ghost pre_best = best; */
    if (j-i > best) best = j-i;
    /*@ assert \forall integer k; (i <= k < j) ==> best >= k - i; */

    /*@ ghost i_pre = i; */
    i = j;
    
  }
  /*@ assert \forall integer k1, k2; 
    @   0 <= k1 <= k2 < N && 
    @   (\forall integer k; k1<= k <= k2 ==> x[k] == x[k1]) ==> 
    @   (
    @     k2 - k1 + 1 <= best
    @   );
    @*/
  return best;
}

