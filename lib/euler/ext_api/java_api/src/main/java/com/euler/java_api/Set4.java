/******************************************************************************
 *                                  - Set4.java -                             *
 *                                                                            *
 * Abstract class 'Set4' houses solutions for problems 31-40.                 *
 ******************************************************************************/
package com.euler.java_api;

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


	/************************************************************************
	 *							- problem38 -								*
	 *																		*
	 * Take the number 192 and multiply it by each of 1, 2, and 3:			*
	 *																		*
	 * 192 × 1 = 192														*
	 * 192 × 2 = 384														*
	 * 192 × 3 = 576														*
	 *																		*
	 * By concatenating each product we get the 1 to 9 pandigital,			*
	 * 192384576. We will call 192384576 the concatenated product of 192	*
	 * and (1, 2, 3)														*
	 *																		*
	 * The same can be achieved by starting with 9 and multiplying by 1, 2,	*
	 * 3, 4, and 5, giving the pandigital, 918273645, which is the			*
	 * concatenated product of 9 and (1, 2, 3, 4, 5).						*
	 *																		*
	 * What is the largest 1 to 9 pandigital 9-digit number that can be		*
	 * formed as the concatenated product of an integer with				*
	 * (1, 2, ... , n) where n > 1?											*
	 ************************************************************************/
	public static String problem38() {
		boolean[] dupSet;
		boolean matchingMax;
		int[] digBuff = new int[9];
		int[] maxDigs = new int[9];
		int root;
		int magUp;
		int baseMag;
		int magDown;
		int n;
		int digit;
		int digsI;
		int remProd;
		int prodMag;
		int remMag;
		int nextMag;

		for (root = 9876, magUp = 10_000,  baseMag = 1_000,   magDown = 100;
			 root > 0;
			 root /= 10,  magUp = baseMag, baseMag = magDown, magDown /= 10) {

			for (n = root; n > baseMag; n--) {

				//strip leading digit of 'n'
				digit = n / baseMag;

				// if 'digit' is less than the most sig digit of running max
				// concatenated 1 through 9 pandigital product, 'maxDigs',
				// the remaining 'n' in this loop are doomed to be smaller as well,
				// continue to next root
				if (digit < maxDigs[0]) {
					break;
				}

				matchingMax = digit == maxDigs[0];

				// initialize state for concatenating remaining digits of 'n'
				// (and digits of remaining products of 'n')

				// init lookup array for dups
				dupSet 		  = new boolean[10]; // indices 0-9 default to 'false'
				dupSet[0]	  = true;			 // exclude '0' by default
				dupSet[digit] = true;			 // mark 'digit'

				// set first digit of buffer, update index 'digsI'
				digBuff[0] = digit;
				digsI	   = 1;

				// init accumulators for generating products of 'n' and stripping
				// their digits...
				prodMag  = baseMag;
				nextMag  = magUp;
				nextProd = 2 * n;

				// init accumulators for stripping remaining digits of 'n'
				remProd = n % prodMag;
				remMag  = magDown;


				// unconditional loop for concatenating remaining products because
				// there are several short circuit opportunities mid loop
				while (true) {

					// if product has been completely stripped of its digits...
					if (remProd == 0) {
						// check if next product jumps an order of magnitude
						// (i.e. from 5 * 1 = 5 to 5 * 2 = 10) and adjust
						// mags accordingly
						if (nextProd >= nextMag) {
							prodMag  = nextMag;
							nextMag *= 10;
						}

						// reset product to next multiple of 'n' and continue
						remMag    = prodMag;
						remProd   = nextProd;
						nextProd += n;
					}

					// strip next most sig digit from remaining product of 'n'
					digit = remProd / remMag;

					// if digit is a dup or 0, bail
					if (dupSet[digit]) {
						break;
					}

					// if running concatenation is not greater than 'maxDigs' up to
					// 'digsI - 1'...
					if (matchingMax) {

						// if next most sig digit is less than next 'maxDigs' digit,
						// max cannot be exceeded, bail
						if (digit < maxDigs[digsI]) {
							break;

						// if next digit exceeds next 'maxDigs' digit, running
						// concatenation is safely larger than current max, switch
						// flag and continue
						} else if (digit > maxDigs[digsI]) {
							matchingMax = false;

						// if 'digBuff' matches 'maxDigs' up to the second to last
						// digit, at best running concatenation is an even 1 through
						// 9 pandigital that MATCHES 'maxDigs' completely
						// (can't exceed), bail
						} else if (digsI == 7) {
							break;
						}
					}

					// otherwise copy digit to buffer
					digBuff[digsI] = digit;

					// if buffer has been completely filled, concatenation is
					// complete, bail...
					if (digsI == 8) {

						// if last remaining product is single digit then digBuff MUST
						// be 1 through 9 pandigital AND greater than the current
						// running max, update then bail
						if (remProd < 10) {
							System.arraycopy(digBuff, 0,
											 maxDigs, 0, 9);
						}

						// otherwise more digits remain in last concatenated
						// product of 'n', solution condition failed, bail
						break;
					}


					// remaining product of 'n' has passed the gauntlet, update
					// accumulators to prepare for next digit
					dupSet[digit] = true;
					digsI++;
					remProd %= remMag;
					remMag  /= 10;
				}
			}
		}

		// build string representation of 'maxDigs' digit array and return
		StringBuilder maxString = new StringBuilder();

		for (int dig : maxDigs) {
			maxString.append(Integer.toString(dig));
		}

		return maxString.toString();
	}


	/*
	 * Stores conditions for generating 1 through 9 pandigital product
	 * identities and an accumulator for collecting and summing
	 * valid products.
	 */
	private static class DigitsTree {
		// theoretical max value of product in 1-9 pandigital identity
		private static final int MAX_PRODUCT = 9876;

		private final int lengthFirst;	   // number of digits of 1st number
		private final int lengthSecond;	   // number of digits of 2nd number

		// unique generated products with a valid 1-9 pandigital identity
		private HashSet<Integer> products;

		private DigitsTree(int lengthMultiplicand, int lengthMultiplier) {
			lengthFirst  = lengthMultiplicand;
			lengthSecond = lengthMultiplier;
			products	 = new HashSet<Integer>();
		}

		/*
		 * Spawns the first node with the branch root conditions
		 * then begins generation of the first and second
		 * numbers.
		 */
		private void start() {
			new DigitsNode(this).spawnChildren();
		}

		/*
		 * The final nodes of all completed branches call
		 * this method to check if a 1-9 pandigital identity
		 * was generated.  If so, its product is stored in
		 * the 'products' set.
		 */
		private void processLeaf(DigitsNode leaf) {
			int product = leaf.accNumber * leaf.pivNumber;

			// if 'product' > 9876...
			if (product > MAX_PRODUCT) {
				return; // pandigital condition impossible, return
			}

			// otherwise 'product' must be 4 digits (can't be <= 3)

			HashSet<Integer> prodDigs = new HashSet<>();
			prodDigs.add(product % 10);

			// extract digits from product, return if a duplicate is found
			for (int remProd = product / 10; remProd > 0; remProd /= 10) {
				if (!prodDigs.add(remProd % 10)) {
					return;
				}
			}

			// if digits coincides with those remaining in 'pool'...
			if (prodDigs.containsAll(leaf.pool)) {
				// a valid identity was found, add product to set
				this.products
					.add(Integer.valueOf(product));
			}
		}


		/*
		 * Adds entire set of 'products' from 'other' to 'this' tree's
		 * 'products', returning 'this'.
		 */
		private DigitsTree mergeProducts(DigitsTree other) {
			this.products
				.addAll(other.products);

			return this;
		}


		/*
		 * Sums set of 1 through 9 pandigital 'products' of 'this'.
		 */
		private int sumProducts() {
			return this.products
					   .stream()
					   .mapToInt(Integer::intValue)
					   .sum();
		}


		/*
		 * Houses state of nodes belonging to DigitsTree to allow
		 * deterministic generation of unique pandigital pairs of numbers.
		 */
		private static class DigitsNode {
			// list of digits available for root node of tree
			private static final LinkedList<Integer> INITIAL_DIGITS_POOL =
				new LinkedList<>(Arrays.asList(1, 2, 3, 4, 5, 6, 7, 8, 9));

			private final DigitsTree tree;    // tree supervisor
			private LinkedList<Integer> pool; // list of remaining digits
			private int remDigits; // number of digits remaining for current number
			private int offset;	   // place value for next digit in current number
			private int accNumber; // accumulator for current number
			private int pivNumber; // storage for completed number

			// child node constructor
			private DigitsNode(DigitsNode parent, Integer digit) {

				remDigits = parent.remDigits - 1; // dec number of remaining digits
				offset	  = parent.offset * 10;	  // update place value for next digit

				// add digit times 'parent''s place value to number accumulator
				accNumber = parent.accNumber + (parent.offset * digit.intValue());

				// store pivot number and direct reference to tree
				pivNumber = parent.pivNumber;
				tree	  = parent.tree;

				// store mutated copy of 'parent''s 'pool' ('digit' missing)
				pool	  = new LinkedList<Integer>(parent.pool);
			}

			// root node constructor
			private DigitsNode(DigitsTree digitsTree) {
				// set reference to digitsTree for descendant pivot and leaf nodes
				tree	  = digitsTree;

				// child nodes will select digits to build the first number
				remDigits = digitsTree.lengthFirst;

				// 1st digit will occupy one's place of 'accNumber'
				offset	  = 1;

				// number accumulators initialized to zero
				accNumber = 0;
				pivNumber = 0;

				// copy "constant" list of initial digits to allow mutation
				pool	  = new LinkedList<Integer>(INITIAL_DIGITS_POOL);
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

					// spawn a child node and continue branching
					new DigitsNode(this, digit).spawnChildren();

					// add 'digit' back to its initial position in 'pool'
					poolIter.add(digit);

				} while (poolIter.hasNext());
			}
		}
	}
}
