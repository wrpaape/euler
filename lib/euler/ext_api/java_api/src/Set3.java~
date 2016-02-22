/************************************************************************************
 *                                  - Set3.java -                                   *
 *                                                                                  *
 * Abstract class 'Set3' houses solutions for problems 11-20.                       *
 ************************************************************************************/
import java.util.Map;
import java.util.concurrent.ConcurrentHashMap;
import java.util.Iterator;
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

    Map<Integer, Integer> divsSumMap = new ConcurrentHashMap<>();

    IntStream.range(2, 10_000)
             .forEach((baseNum) -> {

      divsSumMap.put(Integer.valueOf(baseNum),
                     sumOfProperDivs(baseNum));
    });

    Iterator<Map.Entry<Integer, Integer>> mapIter = divsSumMap.entrySet()
                                                              .iterator();
    while(mapIter.hasNext()) {

      Map.Entry<Integer, Integer> numSumPair = mapIter.next();

      Integer baseNum     = numSumPair.getKey();
      Integer candNum     = numSumPair.getValue();
      Integer candSumDivs = divsSumMap.get(candNum);

      if (candSumDivs != null
          && candSumDivs.equals(baseNum)
          && !candNum.equals(baseNum)) {

        sumAmicableNumbers += (baseNum + candNum);

        mapIter.remove();
        divsSumMap.remove(candNum);
      }
    }

    return Integer.valueOf(sumAmicableNumbers);
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
  }


  /**********************************************************************************
   *                                 - problem28 -                                  *
   *                                                                                *
   * Starting with the number 1 and moving to the right in a clockwise direction a  *
   * 5 by 5 spiral is formed as follows:                                            *
   *                                                                                *
   * 21 22 23 24 25                                                                 *
   * 20  7  8  9 10                                                                 *
   * 19  6  1  2 11                                                                 *
   * 18  5  4  3 12                                                                 *
   * 17 16 15 14 13                                                                 *
   *                                                                                *
   * 1 (2) 3 (4) 5 (6)
   * 7 (8 9) 10 (11 12)
   * 13 (14 15 16) 17 (18 19 20)
   * 21 (22 23 24 25) 26 (27 28 29 30)
   * 31 (32 33 34 35 36) 37 () 43 () 49
   *
   * It can be verified that the sum of the numbers on the diagonals is 101.        *
   *                                                                                *
   * What is the sum of the numbers on the diagonals in a 1001 by 1001 spiral       *
   * formed in the same way?                                                        *
   **********************************************************************************/
  public static Integer problem28() { 

    final int STEPS_PER_LENGTH = 2;

    int sumDiags;
    int diag;
    int lengthSide;
    int step;

    sumDiags = -1;
    diag     = 3;

    for (lengthSide = 2; lengthSide < 1_001; lengthSide++) {
      for (step = 0; step < STEPS_PER_LENGTH; step++) {
        sumDiags += diag;
        diag     += lengthSide;
      }
    }

    // System.out.println(diag);
    // System.out.println("sumDiags: " + sumDiags);


    return Integer.valueOf(sumDiags);
  }
}
