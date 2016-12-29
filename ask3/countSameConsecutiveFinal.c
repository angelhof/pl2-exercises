/**
 *
 * Frama-C Version: Frama-C Silicon-20161101
 * Alt-Ergo Version: 1.30
 *
 * Run the following: 
 * frama-c -wp -wp-rte -wp-prover alt-ergo -wp-timeout 10 countSameConsecutiveFinal.c -then -report
 *
 **/


/*@ predicate all_equal_in_range{L}(int *x, integer minidx, integer maxidx) = 
  @   \forall integer k; minidx <= k < minidx + maxidx ==> \at(x[minidx], L) == \at(x[k], L)
  @   ;
*/

/*@ predicate all_smaller_than_best{L}(int *x, integer i, integer maxidx, integer best) = 
  @   \forall integer k1; 
  @     ( 
  @       i <= k1 < maxidx 
  @       && k1 + best < maxidx 
  @       && (0 < k1 < maxidx ==> \at(x[k1],L) != \at(x[k1-1],L)
  @     ) 
  @     && all_equal_in_range{L}(x, k1, best))
  @     ==> \at(x[k1],L) != \at(x[k1+best],L);
  @*/

/*@ requires 1 <= N <= 1000000;
  @ requires \valid(x + (0..N-1));
  @ ensures \exists integer k1; 0 <= k1 < N && all_equal_in_range(x, k1, \result);
  @ ensures all_smaller_than_best(x, 0, N, \result);
*/
int countSameConsecutive(int N, int x[]) {
  int best = 0, i = 0;
  /*@ loop invariant 0 <= i <= N;
    @ loop invariant i == 0 ==> best == 0;
    @ loop invariant i > 0 ==> 1 <= best <= N;
    @ loop invariant i > 0 ==> \exists integer k1; 0 <= k1 < i && all_equal_in_range(x, k1, best);
    @ loop invariant 0 < i < N ==> x[i] != x[i-1];
    @ loop invariant i > 0 ==> all_smaller_than_best(x, 0, i, best);
    @ loop assigns i, best;
    @ loop variant N - i;
  */
  while (i < N) {
    int j = i+1;
    /*@ loop invariant i < j <= N;
      @ loop invariant all_equal_in_range(x, i, j-i);
      @ loop assigns j;
      @ loop variant N - j;
    */
    while (j < N && x[j] == x[i]) ++j;
    if (j-i > best) best = j-i;
    i = j;    
  }
  return best;
}