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
					 .collect(Collectors.toCollection(ConcurrentLinkedQueue::new)));

		leftCombs.forEach(System.out::println);

		// int[] rightDigits = IntStream.range(splitNum, 10)
		//                              .boxed()
		//                              .collect(Collections.toCollection(LinkedList::new));

		return leftCombs.getFirst();
	}

  // private static LinkedList<int[]> combinations(int[] digits) {
	private static <T> LinkedList<LinkedList<T>> combininations(ConcurrentLinkedQueue<T> queue) {
		// List<T> syncList   				   = Collections.synchronizedList(queue);
		LinkedList<T> currComb             = new LinkedList<>();
		LinkedList<LinkedList<T>> accCombs = new LinkedList<>();


		doCombine(queue, currComb, accCombs);



		return accCombs;
	}


	private static <T> void doCombine(ConcurrentLinkedQueue<T> remQ,
									  LinkedList<T> currComb,
									  LinkedList<LinkedList<T>> accCombs) {

		// List remQueue = Collections.synchronizedList(remQueue);

		Iterator<T> qIter = remQueue.Iterator();

		while (qIter.hasNext()) {

			LinkedList<T> nextComb = new LinkedList<T>(currComb);
			List list2 = ((List) ((LinkedList) list).clone());

			T nextEl = qIter.next();

			qIter.remove();

			nextComb.push(nextEl);

			accCombs.push(nextComb);

			doCombine(remQ, nextComb, accCombs);

			qIter.add(nextEl);
		}
	}
}
