/******************************************************************************
 *                                  - Sets.java -                             *
 *                                                                            *
 * Abstract class 'Sets' houses common functionality shared amongst problem	  *
 * 'SetX' modules. 															  *
 ******************************************************************************/
import java.util.LinkedList;
import java.util.ListIterator;
import java.util.Set;

public abstract class Sets {
	/**************************************************************************
	 *							- permutations -							  *
	 *																		  *
	 * Takes an input LinkedList of elements, 'list', and returns a			  *
	 * LinkedList of all orderings of 'list', 'accPerms'.					  *
	 **************************************************************************/
	public static <T> LinkedList<LinkedList<T>> permutations(LinkedList<T> list) {
		LinkedList<LinkedList<T>> accPerms = new LinkedList<>();

		// start reduction with an empty branch and main accumulators
		doPermute(list, new LinkedList<T>(), accPerms);

		return accPerms; // return main accumulator
	}

	/**************************************************************************
	 *								- doPermute -							  *
	 *																		  *
	 * Recursive function implemented by 'permutations' to generate all		  *
	 * orderings of a list of elements. elements are selected one-by-one from *
	 * 'remList' and added to the branch permutation accumulator, 'currPerm'. *
	 * The remaining list is reduced along with the next accumulator until	  *
	 * no elements remain in 'remList'.  The completed permutation is added	  *
	 * to the main accumulator, 'accPerms', and a return is made one level up *
	 * the call stack.														  *
	 **************************************************************************/
	private static <T> void doPermute(LinkedList<T> remList,
									  LinkedList<T> currPerm,
									  LinkedList<LinkedList<T>> accPerms) {
		// if no elements remain...
		if (remList.isEmpty()) {
			// permutation complete, push 'currPerm' into main accumulator
			accPerms.push(currPerm);
			return;
		}

		// otherwise spawn new permutation branches
		ListIterator<T> listIter = remList.listIterator();

		 do {
			// remove 'nextEl' from 'remList'
			T nextEl = listIter.next();
			listIter.remove();

			// copy the current shortened 'remList'
			LinkedList<T> nextRem  = new LinkedList<T>(remList);

			// copy the branch accumulator and push 'nextEl' into the new list
			LinkedList<T> nextPerm = new LinkedList<T>(currPerm);
			nextPerm.push(nextEl);

			// reduce new branch
			doPermute(nextRem, nextPerm, accPerms);

			// add 'nextEl' back to is old place in 'remList'
			listIter.add(nextEl);

		} while (listIter.hasNext());
	}


	public static class DigitsSet implements Set {
		private static final int NUM_DIGITS = 10;

		private boolean[] taken;
		private boolean[] base;
		private int size;

		public DigitSet(int... excludeFromBase) {
			base  = new boolean[NUM_DIGITS];
			taken = new boolean[NUM_DIGITS];
			size  = 0;

			for (int digit : excludeFromBase) {
				base[digit]  = true;
				taken[digit] = true;
				size++;
			}
		}

		public boolean add(int digit) {
			if (this.taken[digit]) {
				return false;
			}

			this.taken[digit] = true;
			size++;
			return true;
		}

		public boolean remove(int digit) {
			boolean wasTaken  = this.taken[digit];
			this.taken[digit] = false;
			return wasTaken;
		}

		public void clear() {
			System.arraycopy(this.base,  0,
							 this.taken, 0,
							 NUM_DIGITS);
			this.size = 0;
		}


	}

}
