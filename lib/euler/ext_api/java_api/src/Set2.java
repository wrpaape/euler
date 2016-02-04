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

    // 00000001 00000000 ... 00000000 (7 '0' bits → 1 '1' bit → 1,000 '0' bits)
    // equals 1,000 '1' bits + 00000001
    //       >              <
    //        equals (1 '1' bit → 999 '0' bits) * 2 - 00000001
    //        2 * 2 * 2 * 2 * 2
    //
		System.out.println("ListIterator Approach: ");
		ListIterator<String> listIterator = linkedList.listIterator();
		while (listIterator.hasNext()) {
				System.out.println(listIterator.next());
		}

	public static void main(String[] args) {
	       LinkedList<String> linkedList = new LinkedList<String>();
	        linkedList.add("eBay");
	        linkedList.add("Paypal");
	        linkedList.add("Google");
	        linkedList.add("Yahoo");
	        linkedList.add("IBM");
	        linkedList.add("Facebook");
 
	        // ListIterator approach
	        System.out.println("ListIterator Approach: ");
	        ListIterator<String> listIterator = linkedList.listIterator();
	        while (listIterator.hasNext()) {
	            System.out.println(listIterator.next());
	        }
  
  }
}
