/************************************************************************************
 *                                  - Set2.java -                                   *
 *                                                                                  *
 * Abstract class 'Set2' houses solutions for problems 11-20.                       *
 ************************************************************************************/
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
  public static void problem16() { 
    // create linked list to store digits, expanding as they accumulate
		LinkedList<Integer> digitsList;
		ListIterator<Integer> digitsIterator;
    int remNumMults;
    int sumDigits;
    int digAcc;

		digitsList = new LinkedList<Integer>();
		digitsList.add(2);

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
    // sum resultant digitsList
    while (digitsIterator.hasNext()) {
      sumDigits += digitsIterator.next();
    }

    System.out.print(sumDigits); // print final sum
  }
}
