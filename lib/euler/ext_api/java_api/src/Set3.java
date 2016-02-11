/************************************************************************************
 *                                  - Set3.java -                                   *
 *                                                                                  *
 * Abstract class 'Set3' houses solutions for problems 11-20.                       *
 ************************************************************************************/
import java.util.HashMap;
import java.util.LinkedList;
import java.util.stream.Stream;
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
    int sumAmicableNumbers;

    // HashMap<Integer, LinkedList<Integer>> divsSumMap = new HashMap<Integer, LinkedList<Integer>>();
    HashMap<Integer, LinkedList<Integer>> divsSumMap = new HashMap<>();

    IntStream.range(1, 10_000)
             .forEach((num) -> {
      Integer sumDivs = sumProperDivs(num);

      LinkedList<Integer> amicables = divsSumMap.get(sumDivs);

      if (amicables == null) {
        amicables = new LinkedList<Integer>();

        divsSumMap.put(sumDivs, amicables);
      }

      amicables.add(num);
    });

    sumAmicableNumbers = 0;
    for (LinkedList<Integer> amicables : divsSumMap.values()) {
      if (amicables.size() > 1) {

        for (Integer amicableNum : amicables) {
          
        }

        
      }
    }
              .stream()
              .filter((amicables) -> {
                amicables.size() > 1;
              })
              .reduce(Integer.valueOf(0), Integer::sum);
              // .reduce(0, (amicables) -> {

//                      });

      // if (amicables.size() > 1) {
        // amicables.forEach((amicableNum) -> {
        // });
        // for (Integer amicableNum : amicables) {
        //   sumAmicables += amicableNum;
        // }
      // }
    // });

      // System.out.println("sum:  " + commonSum.toString());
      // System.out.println("divs: " + amicables.toString());
    // sum final set of amicable numbers
    // return amicableNums.parallelStream()
    //                    .reduce(0, Integer::sum);
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
