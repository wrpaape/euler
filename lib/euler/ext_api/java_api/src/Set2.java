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

		LinkedList<Integer> digitsList = new LinkedList<Integer>();

		System.out.println("adding first el");
		digitsList.add(10);

		System.out.println("init iterator");
		ListIterator<Integer> digitsIterator = digitsList.listIterator();

		
		System.out.println("setting first el");
		digitsIterator.add(0);
		System.out.println("next el");
		digitsIterator.next();
		digitsIterator.set(1);


		System.out.println("resetting iterator");
	  digitsIterator = digitsList.listIterator();

		while (digitsIterator.hasNext()) {
      digitsIterator.add(100);
				System.out.println(digitsIterator.next());
		}

		System.out.println("resetting iterator");
	  digitsIterator = digitsList.listIterator();

		while (digitsIterator.hasNext()) {
      digitsIterator.add(100);
				System.out.println(digitsIterator.next());
		}
  }
}
