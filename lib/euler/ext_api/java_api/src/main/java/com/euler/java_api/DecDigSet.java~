package com.euler.java_api;

public class DecDigSet {
	private static final int NUM_DIGITS = 10;
	private static final int STRING_CAP = 32;

	private boolean[] taken;
	private boolean[] base;
	private int size;

	public DecDigSet(int... excludeFromBase) {
		base  = new boolean[NUM_DIGITS];
		taken = new boolean[NUM_DIGITS];
		size  = 0;

		for (int digit : excludeFromBase) {
			base[digit]  = true;
			taken[digit] = true;
		}
	}

	public boolean add(int digit) {
		if (this.taken[digit]) {
			return false;
		}

		this.taken[digit] = true;
		this.size++;
		return true;
	}

	public boolean remove(int digit) {
		if (this.taken[digit]) {
			this.taken[digit] = false;
			this.size--;
			return true;
		}

		return false;
	}

	public void clear() {
		System.arraycopy(this.base,  0,
						 this.taken, 0,
						 NUM_DIGITS);
		this.size = 0;
	}

	public int size() {
		return this.size;
	}

	public String toString() {

		if (this.size == 0) {
			return "[]";
		}

		StringBuilder digString = new StringBuilder(STRING_CAP);

		digString.append("[");

		int digit = 0;
		int count = 0;

		while (true) {

			while (this.base[digit] || !this.taken[digit]) {
				digit++;
			}

			digString.append(Integer.toString(digit));
			count++;

			if (count == this.size) {
				return digString.append("]")
								.toString();
			}

			digString.append(", ");
			digit++;
		}

	}
}
