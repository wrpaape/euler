/************************************************************************************
 *                                     set_3.js                                     *
 *                                                                                  *
 * Module housing problems 11-20.                                                   *
 ************************************************************************************/
'use strict';

module.exports = {
	/**********************************************************************************
	 *                                 - problem23 -                                  *
	 *                                                                                *
   * A perfect number is a number for which the sum of its proper divisors is       *
   * exactly equal to the number. For example, the sum of the proper divisors of 28 *
   * would be 1 + 2 + 4 + 7 + 14 = 28, which means that 28 is a perfect number.     *
	 *                                                                                *
   * A number n is called deficient if the sum of its proper divisors is less than  *
   * n and it is called abundant if this sum exceeds n.                             *
	 *                                                                                *
   * As 12 is the smallest abundant number, 1 + 2 + 3 + 4 + 6 = 16, the smallest    *
   * number that can be written as the sum of two abundant numbers is 24. By        *
   * mathematical analysis, it can be shown that all integers greater than 28123    *
   * can be written as the sum of two abundant numbers. However, this upper limit   *
   * cannot be reduced any further by analysis even though it is known that the     *
   * greatest number that cannot be expressed as the sum of two abundant numbers is *
   * less than this limit.                                                          *
	 *                                                                                *
   * Find the sum of all the positive integers which cannot be written as the sum   *
   * of two abundant numbers.                                                       *
	 **********************************************************************************/
  problem23: function() {

    const HARD_UPPER_LIMIT = 28124;

    var abundants, // array of accumulated abundant numbers in descending order
        sumBigTwo, // sum of the current two largest abundant numbers
        sumMinTwo, // sum of the two smallest abundant numbers
        resultSum, // sum of result set of numbers adhering to problem condition
        maxBigAbd, // equal to 'n' - 12, used to reduce search pool for prob condition
        minIndex,  // index of smallest abundant number, 12 
        capIndex,  // index of last largest abundant when testing if 'n' ε result set
        bigIndex,  // index of current larger of pair of test abundant numbers
        smlIndex,  // index of current smaller of pair of test abundant numbers
        smlMatch,  // required abundant number to meet prob condition for 'bigIndex'
        bigAbund,  // larger of abundant number pair (abundants[bigIndex])
        smlAbund,  // smaller of abundant number pair (abundants[smlIndex])
        n;         // generic counter



    abundants = [12]; // init abundants with smallest 2 abundant numbers
    sumBigTwo = 0;   // arbitrary low value, will be overriden after first is found

    // generate abundant numbers until can adding two largest > 28123
    for (n = 13; sumBigTwo < HARD_UPPER_LIMIT; n++) {
      if (isAbundant(n)) {
        sumBigTwo = n + abundants[0]; // set 'sumBigTwo' to 'n' + last largest
        abundants.unshift(n);         // unshift 'n' into head as new largest abundant
      }
    }

    // position 'minIndex' to point to last member of abundants (12)
    minIndex  = abundants.length - 1;

    // position 'capIndex' to point to larger of smallest pair of abundant numbers
    capIndex  = minIndex - 1;

    // smallest two abundant numbers are 12 and second to last from 'abundants'
    sumMinTwo = 12 + abundants[capIndex];

    // init resultSum to sum of all numbers smaller than 'sumMinTwo'
    resultSum = 0;
    for (n = 1; n < sumMinTwo; n++) {
      resultSum += n;
    }

    // while n < smallest number expressable as the sum of 2 abundant numbers...
    for (maxBigAbd = n - 12; n < HARD_UPPER_LIMIT; n++, maxBigAbd++) {
      // adjust 'capIndex' while n > (abundants[capIndex] + 12)
      while (maxBigAbd > abundants[capIndex] && capIndex > 0) {
        capIndex--;
      }
      
      bigIndex = capIndex;

      // TODO fix this shit VVVVVVVV
      while (true) {
        smlMatch = n - abundants[bigIndex];

        for (smlIndex = minIndex;
             abundants[smlIndex] < smlMatch;
             smlIndex--) {
          continue;
        }

          // console.log("smlIndex: " + smlIndex);
          // console.log("smlAbund: " + abundants[smlIndex]);
          // console.log("bigIndex: " + smlIndex);
          // console.log("bigAbund: " + abundants[bigIndex]);
          // console.log("n:        " + n);

        if (smlIndex <= bigIndex) {
          resultSum += n;
          break;

        } else if (abundants[smlIndex] == smlMatch) {
          break;
        }

        bigIndex++;
      }
    }

    return resultSum;

    // helper functions
    function isAbundant(n) {
      var smallDiv,
          minBigDiv,
          sumDivs;

      sumDivs   = 1;
      minBigDiv = n;

      for (smallDiv = 2; smallDiv < minBigDiv; smallDiv++) {
        if (n % smallDiv == 0) {
          minBigDiv = n / smallDiv;
          sumDivs  += (smallDiv + minBigDiv);
        }
      }

      // account for extra addition when minBigDiv == smallDiv == sqrt(n)
      if (minBigDiv == (smallDiv - 1)) {
        sumDivs -= minBigDiv;
      }

      return (sumDivs > n); // n is abundant if the sum of n's proper divisors > n
    }
  }
};
