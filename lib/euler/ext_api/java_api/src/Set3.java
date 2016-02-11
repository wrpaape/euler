/************************************************************************************
 *                                  - Set3.java -                                   *
 *                                                                                  *
 * Abstract class 'Set3' houses solutions for problems 11-20.                       *
 ************************************************************************************/
import java.util.HashMap;
import java.util.Map;
import java.util.concurrent.ConcurrentHashMap;
import java.util.Iterator;
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
    int sumAmicableNumbers = 0;
    IntStream validRange   = IntStream.range(2, 10_000);

    Map<Integer, Integer> divsSumMap = new ConcurrentHashMap<>();

    validRange.forEach((baseNum) -> {
      divsSumMap.put(Integer.valueOf(baseNum),
                     sumOfProperDivs(baseNum));
    });

    Iterator<Map.Entry<Integer, Integer>> mapIter = divsSumMap.entrySet()
                                                              .iterator();
    while(mapIter.hasNext()) {
      Map.Entry<Integer, Integer> numSumPair = mapIter.next();
      // System.out.println("checking pair:" + numSumPair.toString());

      Integer baseNum     = numSumPair.getKey();
      Integer candNum     = numSumPair.getValue();
      Integer candSumDivs = divsSumMap.get(candNum);

      if ((candSumDivs != null) && candSumDivs.equals(baseNum) && !candNum.equals(baseNum)) {
        System.out.println("adding amicable pair:" + numSumPair.toString());

        sumAmicableNumbers += (baseNum + candNum);

        mapIter.remove();
        divsSumMap.remove(candNum);
      }
    }

    return sumAmicableNumbers;
  }

  static private Integer sumOfProperDivs(int num) {
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
    // return sumDivs; // return final sum
  }
}
