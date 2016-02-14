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
        resultSum, // result accumulator
        n;         // generic counter used to find next abundant number



    abundants = [3, 2]; // init abundants with first 2 abundant numbers

    n = 4; // starting at 4...

    // condition to stop generating abundant numbers
    while ((abundants[0] + abundants[1]) < HARD_UPPER_LIMIT) {
      if (isAbundant(n)) {
        abundants.unshift(n);
      }
      
      n++;
    }

    var resultSum = abundants.reduce(function (acc, el) {
      return acc + el;
    })

    return resultSum;

    function isAbundant(n) {
      var smallDiv,
          minBigDiv,
          sumDivs;

      smallDiv  = 1;
      minBigDiv = n;
      sumDivs   = smallDiv;

      while (smallDiv < minBigDiv) {
        smallDiv++;

        if (n % smallDiv == 0) {
          minBigDiv = n / smallDiv;

          sumDivs += (smallDiv + minBigDiv);
        }
      }

      // account for extra addition when minBigDiv == smallDiv == sqrt(n)
      if (minBigDiv == smallDiv) {
        sumDivs -= minBigDiv;
      }


      return (sumDivs > n); // n is abundant if the sum of n's proper divisors > n
    }
  }
};
