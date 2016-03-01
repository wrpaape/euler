/******************************************************************************
 *                                  - Sets.java -                             *
 *                                                                            *
 * Abstract class 'Sets' houses common functionality shared amongst problem	  *
 * 'SetX' modules. 															  *
 ******************************************************************************/
import java.util.LinkedList;
import java.util.ListIterator;

public abstract class Sets {

	private static <T> LinkedList<LinkedList<T>> combinations(LinkedList<T> list) {
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

			LinkedList<T> nextRem  = new LinkedList<T>(remList);
			LinkedList<T> nextComb = new LinkedList<T>(currComb);

			nextComb.push(nextEl);

			doCombine(nextRem, nextComb, accCombs);

			listIter.add(nextEl);

		} while (listIter.hasNext());
	}
}
