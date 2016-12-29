/*@ predicate smaller{L}(int *x, integer maxIdx, integer best) =
  @   \forall integer k1, k2; 
  @     0 <= k1 < k2 <= maxIdx && 
  @     (k1 > 0 ==> \at(x[k1], L) != \at(x[k1-1],L) ) &&
  @     (k2 < maxIdx ==> \at(x[k2], L) != \at(x[k2-1],L) ) &&
  @     (\forall integer k; k1<= k < k2 ==> \at(x[k], L) == \at(x[k2-1],L) ) ==> 
  @     (
  @       k2 - k1 <= best 
  @     );
*/

/*@ predicate one_equals{L}(int *x, integer i, integer best) =
  @    \exists integer k1; 
  @     0 <= k1 < i &&
  @     (\forall integer k; k1 <= k < i ==> \at(x[k],L) == \at(x[i-1],L)) &&
  @     (k1 > 0 ==> \at(x[k1],L) != \at(x[k1-1],L)) &&
  @     i - k1 <= best;
*/

/*@ predicate equals{L}(int *x, integer maxIdx, integer best) =
  @   \forall integer k2;
  @   0 < k2 <= maxIdx &&
  @   (k2 < maxIdx ==> \at(x[k2-1],L) != \at(x[k2],L) ) ==>
  @   equals{L}(x, k2, best);
  @*/

/*@ requires 1 <= N <= 1000000;
  @ requires \valid(x + (0..N-1));
  @ ensures \forall integer k1, k2; 
  @   0 <= k1 < k2 <= N && 
  @   (k1 > 0 ==> x[k1] != x[k1-1]) &&
  @   (k2 < N ==> x[k2] != x[k2-1]) &&
  @   (\forall integer k; k1<= k < k2 ==> x[k] == x[k2-1]) ==> 
  @   (
  @     k2 - k1 <= \result
  @   );
  @ ensures \exists integer k1, k2; 
  @   0 <= k1 <= k2 < N && 
  @   (\forall integer k; k1<= k <= k2 ==> x[k] == x[k1]) && 
  @   (
  @     k2 - k1 + 1 == \result
  @   );
  @ ensures \forall integer k1; 
  @     0 <= k1 < N &&
  @     (k1 > 0 ==> x[k1] != x[k1-1]) ==>
  @     (\exists integer k2; k1 < k2 <= N && 
  @     (\forall integer k; k1 <= k < k2 ==> x[k] == x[k2-1]) &&
  @     k2 - k1 <= \result);
  @ ensures equals(x, N, \result);
  @*/
int countSameConsecutive(int N, int x[]) {
  int best = 0, i = 0;
  /*@ loop invariant 0 <= i <= N;
    @ loop invariant (i > 0 ==> best > 0) || (i == 0 ==> best == 0);
    @ loop invariant i == 0 || 
    @   \exists integer k1; 
    @     0 <= k1 < i &&
    @     (\forall integer k; k1 <= k < i ==> x[k] == x[i-1]) &&
    @     (k1 > 0 ==> x[k1] != x[k1-1]) &&
    @     i - k1 <= best;
    @ loop invariant i == 0 || one_equals(x, i, best);
    @ loop invariant 0 < i < N ==> x[i] != x[i-1];
    @ loop assigns i, best;
    @*/
  while (i < N) {
    int j = i+1;
    /*@ loop invariant i+1 <= j <= N;
      @ loop invariant \forall integer k; i <= k < j ==> x[k] == x[i];
      @ loop assigns j;
      @*/
    while (j < N && x[j] == x[i]) ++j;
    /*@ assert j < N ==> \forall integer k; i <= k < j ==> x[j] != x[k]; */ 
    /*@ assert j < N ==> x[j] != x[j-1]; */

    if (j-i > best) best = j-i;
    /*@ assert \forall integer k; (i <= k < j) ==> best >= k - i; */
    /*@ assert j > i; */

    i = j;
    
  }
  
  return best;
}

