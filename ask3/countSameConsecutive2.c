
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


/*@ requires 1 <= N <= 1000000;
  @ requires \valid(x + (0..N-1));
  @ ensures \forall integer k; 0 <= k < N ==> x[k] == \old(x[k]);
  @ ensures \result > 0;
  @ ensures \forall integer k; 0 <= k <= N ==> smaller(k, k, N, x, 0, \result);
  @ ensures \exists integer k; 0 <= k <= N && equal(k, k, N, x, 0, \result);
  @*/
int countSameConsecutive(int N, int x[]) {
  int best = 1, i = 0, temp = 1;
  /*@ loop invariant 0 <= i <= N;
    @ loop invariant 1 <= best <= N;
    @ loop invariant 1 <= temp <= i+1;
    @ loop invariant \exists integer k; 0 <= k <= i && equal(k, k, i, x, 0, best);
    @ loop invariant \forall integer k; 0 <= k <= i ==> smaller(k, k, i, x, 0, best);
    @ loop assigns i, best, temp;
    @ loop variant N-i;
    @*/
  while (i < N-1) {
    if(x[i+1] == x[i]){
      temp++;
    }else{
      if(temp > best) best = temp;
      temp = 1;
    }
    i++;
  }
  
  
  
  return best;
}

