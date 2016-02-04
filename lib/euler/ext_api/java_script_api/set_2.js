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

    const HUNDRED_CHARS      = 7;  /* hundred */
    const ONE_THOUSAND_CHARS = 11; /* one-thousand */

    const ONES  = [3, /* one */      3, /* two */       5, /* three */
                   4, /* four */     4, /* five */      3, /* six */
                   5, /* seven */    5, /* eight */     4  /* nine */];

    const TENS  = [6, /* twenty */   6, /* thirty */    5, /* forty */
                   5, /* fifty */    5, /* sixty */     7, /* seventy */
                   6, /* eighty */   6  /* ninety */];

    const UNIQS = [3, /* ten */      6, /* eleven */    6, /* twelve */
                   8, /* thirteen */ 8, /* fourteen */  7, /* fifteen */
                   7, /* sixteen */  9, /* seventeen */ 8, /* eighteen */
                   8  /* nineteen */];

    var sum9,
        sum99,
        sum999;

    // sum num chars of 'ONES' digits (1-9)
    sum9 = ONES.reduce(function(acc, num_chars) {

      return acc + num_chars;
    }, 0) ;

    // add num chars of 'TENS'and 'UNIQS' digits (20, 30, ..., 90)
    sum99 = [TENS, UNIQS].reduce(function(acc, digit_chars) {

      return digit_chars.reduce(function(acc, num_chars) {

        return acc + num_chars;
      }, acc);
    }, sum9);


    // add num chars for digits between those in 'TENS'
    sum99 = TENS.reduce(function(acc, tens_num_chars) {

      // i.e. twenty-one, twenty-two, ... twenty-nine
      return acc + (tens_num_chars * 9) + sum9;
    }, sum99);

    // add num chars for each hundred interval (100 - 999)
    sum999 = ONES.reduce(function(acc, ones_num_chars) {
      var hunds_num_chars = ones_num_chars + HUNDRED_CHARS;

      //i.e one-hundred-and-one ... one-hundred-and-ninety-nine
      //          >                                  <
      return acc + (hunds_num_chars + 3) * 99 + sum99 + hunds_num_chars; 
      //                                               >               <
      //i.e one-hundred
    }, sum99) ;

    // add num chars for 'one-thousand'
    return sum999 + ONE_THOUSAND_CHARS;
  }
};
