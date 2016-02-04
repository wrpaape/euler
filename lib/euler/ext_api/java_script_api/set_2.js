/************************************************************************************
 *                                     set_2.js                                     *
 *                                                                                  *
 * Module housing problems 11-20.                                                   *
 ************************************************************************************/
'use strict';

module.exports = {
	/**********************************************************************************
	 *                                 - problem17 -                                  *
	 *                                                                                *
	 * If the numbers 1 to 5 are written out in words: one, two, three, four, five,		*
	 * then there are 3 + 3 + 5 + 4 + 4 = 19 letters used in total.										*
	 * If all the numbers from 1 to 1000 (one thousand) inclusive were written out in *
	 * words, how many letters would be used?																					*
	 *                                                                                *
	 *                                                                                *
	 * NOTE: Do not count spaces or hyphens. For example, 342 (three hundred and			*
	 * forty-two) contains 23 letters and 115 (one hundred and fifteen) contains 20		*
	 * letters. The use of "and" when writing out numbers is in compliance with				*
	 * British usage.																																  *
	 **********************************************************************************/
  problem17: function() {
    const HUNDRED_CHARS      = 6;  /* hundred */
    const ONE_THOUSAND_CHARS = 11; /* one-thousand */

    // const ONES_MAP  = {
    //   1:  3, /* one */       2:  3, /* two */      3:  5, /* three */
    //   4:  4, /* four */      5:  4, /* five */     6:  3, /* six */
    //   7:  5, /* seven */     8:  5, /* eight */    9:  4  /* nine */
    // };
    // const TENS_MAP  = {
    //   20: 6, /* twenty */    30: 6, /* thirty */
    //   40: 5, /* forty */     50: 5, /* fifty */    60: 5, /* sixty */
    //   70: 6, /* seventy */   80: 6, /* eighty */   90: 6  /* ninety */
    // };
    // const UNIQS_MAP = {
    //   10: 3, /* ten */
    //   11: 6, /* eleven */    12: 6, /* twelve */   13: 8, /* thirteen */
    //   14: 8, /* fourteen */  15: 7, /* fifteen */  16: 7, /* sixteen */
    //   17: 9, /* seventeen */ 18: 8, /* eighteen */ 19: 8  /* nineteen */
    // };

    const ONES_LIST  = {
      3, /* one */       3, /* two */      5, /* three */
      4, /* four */      4, /* five */     3, /* six */
      5, /* seven */     5, /* eight */    4  /* nine */
    };
    const TENS_LIST  = {
      6, /* twenty */    6, /* thirty */
      5, /* forty */     5, /* fifty */    5, /* sixty */
      6, /* seventy */   6, /* eighty */   6  /* ninety */
    };
    const UNIQS_LIST = {
      3, /* ten */
      6, /* eleven */    6, /* twelve */   8, /* thirteen */
      8, /* fourteen */  7, /* fifteen */  7, /* sixteen */
      9, /* seventeen */ 8, /* eighteen */ 8  /* nineteen */
    };

    var sum99,
        digit,
        sumLetters;

    // letterMap = {
    //   1:  3, /* one */      2:  3, /* two */       3:  5, /* three */
    //   4:  4, /* four */     5:  4, /* five */      6:  3, /* six */
    //   7:  5, /* seven */    8:  5, /* eight */     9:  4, /* nine */
    //   10: 3, /* ten */      11: 6, /* eleven */    12: 6, /* twelve */
    //   13: 8, /* thirteen */ 14: 8, /* fourteen */  15: 7, /* fifteen */
    //   16: 7, /* sixteen */  17: 9, /* seventeen */ 18: 8, /* eighteen */
    //   19: 8, /* nineteen */ 20: 6, /* twenty */    30: 6, /* thirty */
    //   40: 5, /* forty */    50: 5, /* fifty */     60: 5, /* sixty */
    //   70: 6, /* seventy */  80: 6, /* eighty */    90: 6, /* ninety */
    //   100: 7, /* n-hundred */ 1000: 8 /* n-thousand */
    // };



    // function sumVals(obj) {
    //   return Object.keys(obj).reduce(function(acc, key) {
    //     return acc + obj[key];
    //   }, 0);
    // }


    /* sum num chars of map digits (1-19, 20, 30, ..., 90) */
    sum99 = [ONES_MAP, TENS_MAP, UNIQS_MAP].reduce(function(acc, obj) {

      return Object.keys(obj).reduce(function(acc, key) {

        return acc + obj[key];

      }, acc);

    }, 0);

    /* sum num chars for digits between those in 'TENS_MAP' */
    sum99 = (TENS_MAP).reduce(function(acc, ))



    // one eleven twenty-one ... one-hundred-and-one
    // two twelve twenty-two
    // three thirteen twenty-three
    // four fourteen 
    // five fifteen twenty-five ...
    // six sixteen twenty-six
    // seven seventeen
    // eight eighteen
    // nine nineteen
    // ten twenty thirty forty fifty sixty seventy eighty ninety one-hundred ... two-hundred ... one-thousand

    return sum99;
  }
};
