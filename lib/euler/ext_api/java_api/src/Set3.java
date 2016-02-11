/************************************************************************************
 *                                  - Set3.java -                                   *
 *                                                                                  *
 * Abstract class 'Set3' houses solutions for problems 11-20.                       *
 ************************************************************************************/
import java.util.HashMap;
import java.util.ArrayList;
import java.util.stream.IntStream;

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
    IntStream candidateNums;       // search range of amicable nums: {1, ..., 9_999}
		ArrayList<Integer> amicable;   // list of accumulated amicable numbers 
		HashSet<Integer> remCandNums;  // diff set of 'candidateNums' and established amicable numbers
	  int sumAmicables;              // sum of all amicable numbers < 10_000 (result)
	  // int sumNextDivs;                // sum of proper divisors for 'nextNum' 

    // initialize candidate numbers as a range of Integer objects
    candidateNums = IntStream.range(1, 10_000);

    // initialize 'remCanNums' as a HashSet starting with all candidates
    // remCandNums = candidateNums.boxed()
    //                            .collect(Collectors.toCollection(HashSet::new));

    // HashMap<Integer, ArrayList<Integer>> divsSumMap = new HashMap<Integer, ArrayList<Integer>>();
    HashMap<Integer, ArrayList<Integer>> divsSumMap = new HashMap<>();

    candidateNums.forEach((num) -> {
      Integer sumDivs = sumProperDivs(num);

      List<Integer> amicables = divsSumMap.get(sumDivs);

      if (amicables == null) {
        amicables = new List<Integer>();

        divsSumMap.put(sumDivs, amicables);
      }

      amicables.add(num);
    });

    sumAmicables = 0;
    divsSumMap.values
              .forEach((amicables) -> {
      if (amicables.size > 1) {


        // amicables.reduce(0, Integer::sum);
      }
    });

      // System.out.println("sum:  " + commonSum.toString());
      // System.out.println("divs: " + amicables.toString());
    // sum final set of amicable numbers
    // return amicableNums.parallelStream()
    //                    .reduce(0, Integer::sum);
    return 42;
  }

  static private Integer sumProperDivs(int num) {
    int sumDivs;
    int nextNum;
    int minBigDiv; // current smallest divisor 'b' where a * b = 'nextNum' and b > a

    sumDivs   = 1;   // set init sum to 1 (for first proper divisor, 1)
    minBigDiv = num; // init to 'num' (for first larger proper divisor, 'num')
    nextNum   = 2;   // starting at 2, find second proper divisor of 'num'
    
    while (nextNum < minBigDiv) {
      // if 'num' is evenly divisible by 'nextNum'...
      if (num % nextNum == 0) {
        minBigDiv = num / nextNum; // calculate larger divisor
        // if 'nextNum' is a square root of 'num', add once, otherwise add both divs
        sumDivs += (minBigDiv == nextNum) ? nextNum : (nextNum + minBigDiv);
      }
      nextNum++; // increment counter
    }

    return Integer.valueOf(sumDivs); // return final sum as an instance of Integer
  }
}
