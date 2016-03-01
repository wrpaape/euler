/******************************************************************************
 *                                  - Set4.java -                             *
 *                                                                            *
 * Abstract class 'Set4' houses solutions for problems 31-40.                 *
 ******************************************************************************/
import java.util.Arrays;
import java.util.HashSet;
import java.util.LinkedList;
import java.util.ListIterator;

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
		/*
		 * Let 'a', 'b', and 'c' represent the multipicand, multiplier, and
		 * product, respectively, of the multiplication statement:
		 *
		 * 	   'a × b = c'.
		 *
		 * One condition for a 1 through 9 pandigital identity is that 9
		 * digits must compose the entire statement, i.e.
		 *
		 *     [number of digits of 'a']
		 *   + [number of digits of 'b']
		 *   + [number of digits of 'c'] = 9
		 *
		 * Ignoring the commutative interchangability of 'a' and 'b', it can be
		 * shown that two unique cases where this condition is satisfied exist:
		 *
		 *   number of digits of 'a' = 1
		 *   number of digits of 'b' = 4
		 *   number of digits of 'c' = 4
		 *
		 * or
		 *
		 *   number of digits of 'a' = 2
		 *   number of digits of 'b' = 3
		 *   number of digits of 'c' = 4
		 *
		 * Accordingly, our search pool of potential 1 through 9 pandigital
		 * identities can be reduced significantly by ignoring all other cases.
		 */

		// initialize search conditions for mulitplication identity generators
		DigitsTree tree144 = new DigitsTree(1, 4);
		DigitsTree tree234 = new DigitsTree(2, 3);

		// begin generating 1 through 9 pandigital identities
		tree144.start();
		tree234.start();

		// merge and sum the unique products from the valid identities
		return Integer.valueOf(tree144.mergeProducts(tree234)
					  				  .sumProducts());
	}


	private static class DigitsTree {
		private static final int MAX_PRODUCT = 9876;

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

			if (product > MAX_PRODUCT) {
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

			private final DigitsTree tree;
			private LinkedList<Integer> pool;
			private int remDigits;
			private int offset;
			private int accNumber;
			private int pivNumber;

			private DigitsNode(DigitsNode parent, Integer digit) {
				tree	  = parent.tree;
				pool	  = new LinkedList<Integer>(parent.pool);
				remDigits = parent.remDigits - 1;
				offset	  = parent.offset * 10;
				accNumber = parent.accNumber + (parent.offset * digit.intValue());
				pivNumber = parent.pivNumber;
			}

			private DigitsNode(DigitsTree digitsTree) {
				tree	  = digitsTree;
				pool	  = new LinkedList<Integer>(INITIAL_DIGITS_POOL);
				remDigits = digitsTree.lengthFirst;
				offset	  = 1;
				accNumber = 0;
				pivNumber = 0;
			}

			private void spawnChildren() {
				// if enough digits have been picked to generate a number...
				if (this.remDigits == 0) {
					// if the pivot number hasn't been generated...
					if (this.pivNumber == 0) {
						// store the generated number 'accNumber' to 'pivNumber'
						this.pivNumber = this.accNumber;
						// re-init generator fields to prep for second number
						this.remDigits = this.tree.lengthSecond;
						this.offset	   = 1;
						this.accNumber = 0;

					} else {
						/*
						 * Branch is done, i.e. 'accNumber' and 'pivNumber'
						 * are completed numbers s.t. their digits are composed
						 * of unique digits from [1, 2, ..., 9] that are not
						 * included in the remaining digit pool.
						 */
						this.tree.processLeaf(this); // process leaf node
						return;						 // return to caller
					}
				}

				// spawn children for each remaining digit in 'pool'
				ListIterator<Integer> poolIter = this.pool.listIterator();

				 do {
					// select next 'digit' from 'pool' and remove it
					Integer digit = poolIter.next();
					poolIter.remove();

					// spawn a child node and continue reduction of branch
					new DigitsNode(this, digit).spawnChildren();

					// add 'digit' back to its initial position in 'pool'
					poolIter.add(digit);

				} while (poolIter.hasNext());
			}
		}
	}
}
