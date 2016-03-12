/************************************************************************************
 *                                  - Set3.java -                                   *
 *                                                                                  *
 * Abstract class 'Set3' houses solutions for problems 11-20.                       *
 ************************************************************************************/
package com.euler.java_api;

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

   private static Integer sumOfProperDivs(int num) {
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
      nextNum++; // continue to next 'nextNum'
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
   * It can be verified that the sum of the numbers on the diagonals is 101.        *
   *                                                                                *
   * What is the sum of the numbers on the diagonals in a 1001 by 1001 spiral       *
   * formed in the same way?                                                        *
   **********************************************************************************/
  public static Integer problem28() { 
    int sumDiags;   // accumulating sum of all diagonals
    int diag;       // current diagonal value on spiral
    int lengthSide; // difference between current 'diag' and next 'diag' on spiral
    int side_i;     // control counter for summing 4 'diag's on square of 'lengthSide'

    sumDiags = 0; // init sum to '0'
    diag     = 1; // start spiral counter from '1'

    // for all squares with an even 'lengthSide' less than 1001...
    for (lengthSide = 2; lengthSide < 1_001; lengthSide+= 2) {

      // for all sides of the square...
      for (side_i = 0; side_i < 4; side_i++) {

        sumDiags += diag;       // increment sum
        diag     += lengthSide; // advance spiral counter to next diagonal
      }
    }

    // add final corner diagonal value to sum and return result
    return Integer.valueOf(sumDiags + diag);
  }
}
