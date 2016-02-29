/******************************************************************************
 *                                  - Set4.java -                             *
 *                                                                            *
 * Abstract class 'Set4' houses solutions for problems 31-40.                 *
 ******************************************************************************/
import java.util.stream.IntStream;
import java.util.stream.Collectors;
import java.util.Collections;
import java.util.List;
import java.util.LinkedList;
import java.util.ListIterator;
import java.util.concurrent.ConcurrentLinkedQueue;

public abstract class Set4 {
  /****************************************************************************
   *                              - problem32 -                               *
   *                                                                          *
	 * We shall say that an n-digit number is pandigital if it makes use of all *
   * the digits 1 to n exactly once; for example, the 5-digit number, 15234,  *
   * is 1 through 5 pandigital.                                               *
   *                                                                          *
	 * The product 7254 is unusual, as the identity, 39 × 186 = 7254,           *
   * containing multiplicand, multiplier, and product is 1 through 9          *
   * pandigital.                                                              *
   *                                                                          *
	 * Find the sum of all products whose multiplicand/multiplier/product       *
   * identity can be written as a 1 through 9 pandigital.                     *
   *                                                                          *
	 * HINT: Some products can be obtained in more than one way so be sure to   *
   * only include it once in your sum.                                        *
   ****************************************************************************/
	public static Integer problem32() { 
		// return IntStream.range(2, 10)
		// 				.flatMap(Set4::doProducts)
		// 				.distinct()
		// 				.sum();
		doProducts(5);

		return 42;
	}


	private static LinkedList<Integer> doProducts(int splitNum) {

		LinkedList<LinkedList<Integer>> leftCombs = combininations(
			IntStream.range(1, splitNum)
					 .boxed()
					 .collect(Collectors.toCollection(LinkedList::new)));

		leftCombs.forEach(System.out::println);

		// int[] rightDigits = IntStream.range(splitNum, 10)
		//                              .boxed()
		//                              .collect(Collections.toCollection(LinkedList::new));

		return leftCombs.getFirst();
	}

  // private static LinkedList<int[]> combinations(int[] digits) {
	private static <T> LinkedList<LinkedList<T>> combininations(LinkedList<T> list) {
		// List<T> syncList   				   = Collections.synchronizedList(list);
		LinkedList<T> currComb             = new LinkedList<>();
		LinkedList<LinkedList<T>> accCombs = new LinkedList<>();


		doCombine(list, currComb, accCombs);



		return accCombs;
	}


	private static <T> void doCombine(LinkedList<T> remList,
									  LinkedList<T> currComb,
									  LinkedList<LinkedList<T>> accCombs) {

		if (remList.isEmpty()) {
			accCombs.push(currComb);
			return;
		}

		ListIterator<T> listIter = remList.listIterator();

		 do {

			T nextEl = listIter.next();

			listIter.remove();

			// LinkedList<T> nextRem  = (LinkedList<T>) remList.clone();
			// LinkedList<T> nextComb = (LinkedList<T>) nextComb.clone();

			LinkedList<T> nextRem  = new LinkedList<T>(remList);
			LinkedList<T> nextComb = new LinkedList<T>(currComb);

			nextComb.push(nextEl);

			doCombine(nextRem, nextComb, accCombs);

			listIter.add(nextEl);

		} while (listIter.hasNext());
	}
}
