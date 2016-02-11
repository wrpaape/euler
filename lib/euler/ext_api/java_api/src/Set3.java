/************************************************************************************
 *                                  - Set3.java -                                   *
 *                                                                                  *
 * Abstract class 'Set3' houses solutions for problems 11-20.                       *
 ************************************************************************************/
// import java.util.LinkedList;
// import java.util.ListIterator;
import java.util.Set;
import java.util.stream;
import java.util.stream.Collectors;

public abstract class Set3 {
  /**********************************************************************************
   *                                 - problem21 -                                  *
   *                                                                                *
   * Let d(n) be defined as the sum of proper divisors of n (numbers less than n    *
   * which divide evenly into n). If d(a) = b and d(b) = a, where a ≠ b, then a and *
   * b are an amicable pair and each of a and b are called amicable numbers.        *
   *                                                                                *
   * For example, the proper divisors of 220 are 1, 2, 4, 5, 10, 11, 20, 22, 44, 55 *
   * and 110; therefore d(220) = 284. The proper divisors of 284 are 1, 2, 4, 71    *
   * and 142; so d(284) = 220.                                                      *
   *                                                                                *
   * Evaluate the sum of all the amicable numbers under 10000.                      *
   **********************************************************************************/
  public static Integer problem21() { 
		Set<Integer> amicableNums; // stores accumulated amicable numbers 
		Set<Integer> remNums;      // diff set of {1, ..., 9_999} and 'amicableNums' 
	  int nextNum;               // intermediate tested for amicability with 'baseNum'

    Set<Integer> CandidateNums;

    CandidateNums = IntStream
                    .range(1, 10_000)
                    .collect(Collectors.toSet());

    for (Integer baseNum : CandidateNums) {
      System.out.println(baseNum);
    }
    
    // from a base number of 1 to 9_999...
    // IntStream.range(1, 10_000).forEach(baseNum -> {
    //   System.out.println(n);
    // });

    // sum final set of amicable numbers
    // return amicableNums
    //        .parallelStream()
    //        .reduce(0, Integer::sum);

    return 42;
  }

  private int sumProperDivs(int num) {

    return sum;
  }
}
