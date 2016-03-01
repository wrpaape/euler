/******************************************************************************
 *                                  - Set4.java -                             *
 *                                                                            *
 * Abstract class 'Set4' houses solutions for problems 31-40.                 *
 ******************************************************************************/
import java.util.stream.IntStream;
import java.util.stream.Collectors;
import java.util.Collections;
import java.util.List;
import java.util.Arrays;
import java.util.HashSet;
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

		DigitsTree tree144 = new DigitsTree(1, 4);
		DigitsTree tree234 = new DigitsTree(2, 3);

		tree144.start();
		tree234.start();

		return Integer.valueOf(tree144.mergeProducts(tree234)
					  				  .sumProducts());
	}


	private static class DigitsTree {
		private final int lengthFirst;
		private final int lengthSecond;
		private HashSet<Integer> products;

		private DigitsTree(int lengthMultiplicand, int lengthMultiplier) {
			lengthFirst  = lengthMultiplicand;
			lengthSecond = lengthMultiplier;
			products	 = new HashSet<Integer>();
		}

		private void start() {
			new DigitsNode(this).spawnChildren();
		}

		private void processLeaf(DigitsNode leaf) {
			int product = leaf.accNumber * leaf.pivNumber;

			if (product > 9876) {
				return;
			}

			HashSet<Integer> prodDigs = new HashSet<>();
			prodDigs.add(product % 10);

			

			for (int remProd = product / 10; remProd > 0; remProd /= 10) {
				if (!prodDigs.add(remProd % 10)) {
					return;
				}
			} 


			if (prodDigs.containsAll(leaf.pool)) {
				this.products
					.add(Integer.valueOf(product));
			}
		}


		private DigitsTree mergeProducts(DigitsTree other) {
			this.products
				.addAll(other.products);

			return this;
		}


		private int sumProducts() {
			return this.products
					   .stream()
					   .mapToInt(Integer::intValue)
					   .sum();
		}


		private static class DigitsNode {
			private static final LinkedList<Integer> INITIAL_DIGITS_POOL =
				new LinkedList<>(Arrays.asList(9, 8, 7, 6, 5, 4, 3, 2, 1));

			private int remDigits;
			private int accNumber;
			private int offset;
			private int pivNumber;
			private DigitsTree tree;
			private LinkedList<Integer> pool;

			private DigitsNode(DigitsNode parent, Integer digit) {
				remDigits = parent.remDigits - 1;
				accNumber = parent.accNumber + (parent.offset * digit.intValue());
				offset	  = parent.offset * 10;
				pivNumber = parent.pivNumber;
				tree	  = parent.tree;
				pool	  = new LinkedList<Integer>(parent.pool);
			}

			private DigitsNode(DigitsTree digitsTree) {
				remDigits = digitsTree.lengthFirst;
				accNumber = 0;
				offset	  = 1;
				pivNumber = 0;
				tree	  = digitsTree;
				pool	  = new LinkedList<Integer>(INITIAL_DIGITS_POOL);
			}

			private void spawnChildren() {
				if (this.remDigits == 0) {
					if (this.pivNumber == 0) {
						pivot();
					} else {
						this.tree.processLeaf(this);
						return;
					}
				}


				ListIterator<Integer> poolIter = this.pool.listIterator();


				 do {

					Integer digit = poolIter.next();

					poolIter.remove();

					new DigitsNode(this, digit).spawnChildren();

					poolIter.add(digit);

				} while (poolIter.hasNext());
			}

			private void pivot() {
				this.remDigits = this.tree.lengthSecond;
				this.pivNumber = this.accNumber;
				this.accNumber = 0;
				this.offset	   = 1;
			}
		}
	}
}

	// public static Integer problem32() { 
	// 	IntStream.range(2, 10)
	// 					.flatMap(Set4::doProducts)
	// 					.distinct()
	// 					.forEach(System.out::println);
	// 					// .sum();

	// 	return 42;
	// }

	// private static IntStream doProducts(int splitNum) {

	// 	LinkedList<Integer> leftNums  = integerCombinations(1, splitNum);
	// 	LinkedList<Integer> rightNums = integerCombinations(splitNum, 10);
	// 	LinkedList<Integer> products  = new LinkedList<>();

	// 	int leftInt;
	// 	int rightInt;


	// 	for (Integer leftNum : leftNums) {

	// 		leftInt = Integer.valueOf(leftNum);

	// 		for (Integer rightNum : rightNums) {

	// 			rightInt = Integer.valueOf(rightNum);

	// 			products.push(Integer.valueOf(leftInt * rightInt));
	// 		}
	// 	}


	// 	return products.stream()
	// 				   .mapToInt(Integer::intValue);
	// }

	// private static <T> LinkedList<LinkedList<T>> combinations(LinkedList<T> list) {
	// 	LinkedList<T> currComb             = new LinkedList<>();
	// 	LinkedList<LinkedList<T>> accCombs = new LinkedList<>();

	// 	doCombine(list, currComb, accCombs);

	// 	return accCombs;
	// }


	// private static <T> void doCombine(LinkedList<T> remList,
	// 								  LinkedList<T> currComb,
	// 								  LinkedList<LinkedList<T>> accCombs) {
	// 	if (remList.isEmpty()) {
	// 		accCombs.push(currComb);
	// 		return;
	// 	}

	// 	ListIterator<T> listIter = remList.listIterator();

	// 	 do {

	// 		T nextEl = listIter.next();

	// 		listIter.remove();

	// 		LinkedList<T> nextRem  = new LinkedList<T>(remList);
	// 		LinkedList<T> nextComb = new LinkedList<T>(currComb);

	// 		nextComb.push(nextEl);

	// 		doCombine(nextRem, nextComb, accCombs);

	// 		listIter.add(nextEl);

	// 	} while (listIter.hasNext());
	// }


	// private static Integer integerFromDigits(LinkedList<Integer> digits) {

	// 	int number = 0;
	// 	int digOffset = 1;

	// 	for (Integer digit : digits) {
	// 		number 	  += (digit.intValue() * digOffset);
	// 		digOffset *= 10;
	// 	}

	// 	return Integer.valueOf(number);
	// }


	// private static LinkedList<Integer> integerCombinations(int from, int until) {

	// 	LinkedList<Integer> digits = IntStream.range(from, until)
	// 										  .boxed()
	// 										  .collect(Collectors.toCollection(LinkedList::new));

	// 	return combinations(digits).stream()
	// 							   .map(Set4::integerFromDigits)
	// 							   .collect(Collectors.toCollection(LinkedList::new));
	// }
// }
