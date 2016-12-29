
/*@ predicate all_equal_in_range{L}(int *x, integer minidx, integer maxidx) = 
  @   \forall integer k; minidx <= k < minidx + maxidx ==> \at(x[minidx], L) == \at(x[k], L)
  @   ;
*/

/*@ predicate not_equal_in_range{L}(int *x, integer minidx, integer maxidx) = 
  @   \exists integer k; minidx <= k < maxidx && \at(x[minidx], L) != \at(x[k], L);
*/

/*@ predicate best_is_not_best{L}(int *x, integer maxIdx, integer best) = 
  @   \forall integer k1; 0 <= k1 < maxIdx - best - 1 ==> all_equal_in_range{L}(x, k1, k1 + best) && (\at(x[k1], L) == \at(x[k1 + best + 1], L));
*/

/*@ predicate best_is_best{L}(int *x, integer i, integer N, integer best) = 
  @   \forall integer k1; (0 <= k1 < i ) ==> 
  @              (\forall integer j; (0 < j <= N && j + k1 <= N) && all_equal_in_range{L}(x, k1, j) ==> j <= best); 
  @*/

/*@ predicate all_smaller_than_best{L}(int *x, integer i, integer maxidx, integer best) = 
  @   \forall integer k1; (i<= k1 < maxidx && k1 + best < maxidx && (0 < k1 < maxidx ==> \at(x[k1],L) != \at(x[k1-1],L)) && all_equal_in_range{L}(x, k1, best) )
  @     ==> \at(x[k1],L) != \at(x[k1+best],L);
  @
  @*/

/*@ requires N >= 1;
  @ requires N <= 1000000;
  @ requires \valid(x + (0..N-1));
  @ ensures \exists integer k1; 0 <= k1 < N && all_equal_in_range(x, k1, \result);
  
  @ ensures best_is_best(x, N, N, \result);

  @ ensures \forall integer k1; (0 <= k1 < N && k1 + \result < N && all_equal_in_range(x, k1, \result)) ==> x[k1] != x[k1+\result];
  
  @ ensures \forall integer k1; (0 <= k1 < N ==> !all_equal_in_range(x, k1, \result + 1));

  @ ensures !(\exists integer k1; 0 <= k1 < N && k1 + \result < N && all_equal_in_range(x, k1, \result) && x[k1] == x[k1+\result]);
  
  @ ensures !(\exists integer k1; 0 <= k1 < N && k1 + \result < N && all_equal_in_range(x, k1, \result+1) );
  
  @ ensures all_smaller_than_best(x, 0, N, \result);
*/
int countSameConsecutive(int N, int x[]) {
  int best = 0, i = 0;

  /*@ assert i < N;
  */
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
    /*@ assert (j == N) || (x[j] != x[j-1] && !all_equal_in_range(x, i, j + 1) && x[j] != x[i]); */
    /*@ assert i == 0 && j == N ==> all_equal_in_range(x, 0, N);*/
    if (j-i > best) best = j-i;
    /*@ assert \forall integer k; (i <= k <= j) ==> best >= k - i; */
    /*@ assert best >= j - i >= 1;
    */
    i = j;    
  }
  /*@ assert best <= N;
  */
  return best;
}