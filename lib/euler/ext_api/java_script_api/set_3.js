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
        resultSum, // result accumulator
        n;         // generic counter



    abundants = [12]; // init abundants with smallest 2 abundant numbers
    sumBigTwo = 12;

    n = 1; // starting at 4...

    // condition to stop generating abundant numbers
    while (sumBigTwo < HARD_UPPER_LIMIT) {
      if (isAbundant(n)) {
        console.log("n: " + n);
        console.log("n: " + n);

        sumBigTwo = n + abundants[0]; // next sumBigTwo = n + last largest abundant
        abundants.unshift(n);

        break;
      }
      n++;
    }


    // resultSum = 0;
    // for (n = HARD_UPPER_LIMIT - 1; n > 0; n--) {
      
    // }

    console.log("abundants:" + abundants);

    return 42;

    // helper functions
    function isAbundant(n) {
      var smallDiv,
          minBigDiv,
          sumDivs;

      smallDiv  = 1;
      minBigDiv = n;
      sumDivs   = smallDiv;

      for (smallDiv = 2; smallDiv < minBigDiv; smallDiv++) {
        if (n % smallDiv == 0) {
          minBigDiv = n / smallDiv;

          sumDivs += (smallDiv + minBigDiv);
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
