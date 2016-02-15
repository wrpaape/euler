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

    const UPPER_LIMIT = 28123;
    const LOWER_LIMIT = 24;

    var abundants, // array of accumulated abundant numbers in descending order
        resultSum, // sum of result set of numbers adhering to problem condition
        maxBigAbd, // equal to 'n' - 12, used to reduce search pool for prob condition
        isTargetNum,  // 
        genCutoff,    // 
        maxIndex,  // index of last largest abundant when testing if 'n' ε result set
        bigIndex,  // index of current larger (or equal) of pair of test abund nums
        smlIndex,  // index of current smaller (or equal) of pair of test abund nums
        smlMatch,  // required abundant number to meet prob condition for 'bigIndex'
        bigAbund,  // larger of abundant number pair (abundants[bigIndex])
        n;         // generic counter


    abundants = [12];             // init abundants with smallest abundant number
    genCutoff = UPPER_LIMIT - 12; // cap on max generated abundant number

    // starting after 12, generate abundant numbers until abundant 'n' + 12 > 28123
    for (n = 13; n <= genCutoff; n++) {
      if (isAbundant(n)) {
        abundants.push(n); // collect abundant numbers in ascending order
      }
    }

    // init 'resultSum' to 0
    resultSum = 0;

    // position 'maxIndex' to point to largest abundant number
    maxIndex = abundants.length - 1;

    // while n > smallest number expressable as the sum of 2 abundant numbers...
    for (n = UPPER_LIMIT - 1, maxBigAbd = n - 12;
         n > LOWER_LIMIT;                         // break when n == 24
         n--, maxBigAbd--) {

      // decrease upper search bound until largest abund + 12 <= n
      while (abundants[maxIndex] > maxBigAbd) {
        maxIndex--;
      }

      isTargetNum = true;

      for (bigIndex = maxIndex, bigAbund = abundants[bigIndex];
           (bigAbund * 2) >= n;                                 // break if bigAbund * 2 < n
           bigIndex--, bigAbund = abundants[bigIndex]) {

        smlMatch = n - bigAbund;

        // increment 'smlIndex' until bigAbund + abundants[smlIndex] >= n
        for (smlIndex = 0; abundants[smlIndex] < smlMatch; smlIndex++);

        // if bigAbund + abundants[smlIndex] == n, 'n' fails the problem condition
        if (abundants[smlIndex] == smlMatch) {
          isTargetNum = false;
          break;
        }
      }

      if (isTargetNum) {
        resultSum += n;
      }
    }


    // add sum of the remaining numbers under 24 (23, 22, ... 1)
    while (n > 1) {
      n--;
      resultSum += n;
    }

    return resultSum;


    // helper function, returns 'true' if 'n' is an abundant number, 'false' if not
    function isAbundant(n) {
      var smallDiv,
          minBigDiv,
          sumDivs;

      sumDivs   = 1;
      minBigDiv = n;

      // find and sum all proper divisors of 'n' (not including 'n')
      for (smallDiv = 2; smallDiv < minBigDiv; smallDiv++) {
        if (n % smallDiv == 0) {
          minBigDiv = n / smallDiv;
          sumDivs  += (smallDiv + minBigDiv);
        }
      }

      // account for duplicate case when minBigDiv == smallDiv == sqrt(n)
      if (minBigDiv == (smallDiv - 1)) {
        sumDivs -= minBigDiv;
      }

      return (sumDivs > n); // 'n' is abundant if sum of all proper divisors > n
    }
  }
};
