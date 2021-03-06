/************************************************************************************
 *                                  - Set2.java -                                   *
 *                                                                                  *
 * Abstract class 'Set2' houses solutions for problems 11-20.                       *
 ************************************************************************************/
package com.euler.java_api;

import java.util.LinkedList;
import java.util.ListIterator;

public abstract class Set2 {
  /**********************************************************************************
   *                                 - problem16 -                                  *
   *                                                                                *
   * 2¹⁵ = 32768 and the sum of its digits is 3 + 2 + 7 + 6 + 8 = 26.               *
   *                                                                                *
   * What is the sum of the digits of the number 2¹⁰⁰⁰?                             *
   **********************************************************************************/
  public static Integer problem16() { 
		LinkedList<Integer> digitsList;       // stores result digits while multiplying
		ListIterator<Integer> digitsIterator; // iterator for traversing 'digitsList'
    int remNumMults;                      // remaining multiplications (* 2)
    int sumDigits;                        // sum of all decimal digits of 2¹⁰⁰⁰
    int digAcc;                           // used to 'carry' overflow to next 'place'

		digitsList = new LinkedList<Integer>();     // initialize 'digitsList'
		digitsList.add(2);                          // set head digit (ones) to '2'

		digitsIterator = digitsList.listIterator(); // initialize iterator
    remNumMults = 999;                          // 999 mults to go
    // build digitsList
    while (remNumMults > 0) {
      digAcc = 0;
      // update digits list
      while (digitsIterator.hasNext()) {
        digAcc += (digitsIterator.next() * 2);

        digitsIterator.set(
            (digAcc > 9) ? digAcc % 10 : digAcc);

        digAcc /= 10;
      }

      // append nonzero overflow
      if (digAcc > 0) {
        digitsIterator.add(digAcc);
      }
      
      digitsIterator = digitsList.listIterator(); // reset iterator
      remNumMults--;                              // decrement remaining multiplications
    }

    sumDigits = 0;
    // sum resultant 'digitsList'
    while (digitsIterator.hasNext()) {
      sumDigits += digitsIterator.next();
    }

    return sumDigits; // return final sum
  }
}
