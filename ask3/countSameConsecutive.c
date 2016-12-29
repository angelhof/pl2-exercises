
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


/*@ requires 1000001 > N > 0;
  @ requires \valid(x + (0..N-1));
  @ ensures \result > 0;
  @*/
int countSameConsecutive(int N, int x[]) {
  int best = 0, i = 0;
  /*@ loop invariant 0 <= i <= N;
    @ loop invariant 0 <= best <= i;
    @ loop assigns i, best;
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
    
    
    if (j-i > best) best = j-i;
    i = j;
  }
  return best;
}


// ensures \forall integer k; 0 <= k <= N ==> smaller(k, k, N, x, 0, \result);
// ensures \exists integer k; 0 <= k <= N && equal(k, k, N, x, 0, \result);

// loop invariant \exists integer k; 0 <= k <= i && equal(k, k, i, x, 0, best);
// loop invariant \forall integer k; 0 <= k <= i ==> smaller(k, k, i, x, 0, best);