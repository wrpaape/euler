/************************************************************************
 *			    set_4.c					*
 *									*
 * Module housing solutions to problems 31-40                           *
 ************************************************************************/
/************************************************************************
 *			PREPROCESSOR DIRECTIVES				*
 ************************************************************************/
#include "sets.h"
#include "set_4.h"
/************************************************************************
 *			TOP LEVEL FUNCTIONS				*
 ************************************************************************/
/************************************************************************
 *				- problem_33 -				*
 *									*
 * The fraction ⁴⁹/₉₈ is a curious fraction, as an inexperienced	*
 * mathematician in attempting to simplify it may incorrectly believe	*
 * that ⁴⁹/₉₈ = ⁴/₈, which is correct, is obtained by cancelling the	*
 * 9s.									*
 *									*
 * We shall consider fractions like, ³⁰/₅₀ = ³/₅, to be trivial		*
 * examples.								*
 *									*
 * There are exactly four non-trivial examples of this type of		*
 * fraction, less than one in value, and containing two digits in the	*
 * numerator and denominator.						*
 *									*
 * If the product of these four fractions is given in its lowest	*
 * common terms, find the value of the denominator.			*
 ************************************************************************/
void problem_33(char *result_buffer)
{
	int den;
	int num;
	int gcd;
	int div;
	int base_den;
	int base_num;
	int num_acc;
	int den_acc;
	int sml_div;
	int big_div;
	int fracs_found;
	int **digits_map;
	struct DivNode **divisors_map;

	digits_map = init_digits_map();
	divisors_map = init_divisors_map();

	fracs_found = 0;
	num_acc = 1;
	den_acc = 1;

	for (den = 11; den < 100; ++den) {

		for (num = 10; num < den; ++num) {

			gcd = greatest_common_divisor(num, den, divisors_map);

			if (gcd % 10 == 0)
				continue;

			base_den = den / gcd;

			if (base_den > 9)
				continue;

			base_num = num / gcd;

			div = 2;



			if (is_curious(num,	 den,
				       base_num, base_den, digits_map)) {

				num_acc *= num;
				den_acc *= den;
				++fracs_found;

				if (fracs_found == 4)
					goto FOUND_ALL_FRACTIONS;
			}
		}
	}

FOUND_ALL_FRACTIONS:
	sml_div = 1;

	while (1) {
		while (num_acc % sml_div != 0)
			++sml_div;

		big_div = num_acc / sml_div;

		if (den_acc % big_div == 0) {
			 /*
			  * divide denominator by greatest common divisor
			  * and copy result to buffer
			  */

			sprintf(result_buffer, "%d", den_acc / big_div);
			return;

		} else if (sml_div < big_div) {
			/* increment divisor and continue */
			++sml_div;

		} else {
			 /*
			  * fraction is irreducible, return initial
			  * value of denominator accumulator
			  */
			sprintf(result_buffer, "%d", den_acc);
			return;
		}
	}

}

/************************************************************************
 *				HELPERS					*
 ************************************************************************/
bool is_curious(int num, int den, int base_num, int base_den, int **digits_map)
{
	if (digits_map[num][0] == digits_map[den][1]) {
		return mults_match(base_num, digits_map[num][1],
				   base_den, digits_map[den][0]);
	}

	if (digits_map[num][1] == digits_map[den][0]) {
		return mults_match(base_num, digits_map[num][0],
				   base_den, digits_map[den][1]);
	}

	return false;
}


bool mults_match(int base_num, int match_num, int base_den, int match_den)
{
	if ((base_num == match_num) && (base_den == match_den))
		return true;

	for (int mult = 2, den = base_den * mult;
	     den < 10;
	     ++mult, den = base_den * mult) {

		if ((den == match_den) && ((base_num * mult) == match_num))
			return true;
	}

	return false;
}


int greatest_common_divisor(int num, int den, struct DivNode **divisors_map)
{
	struct DivNode *div_den = divisors_map[den]->next;

	/* if denominator is a prime number, return 1 */
	if (div_den == NULL)
		return 1;

	struct DivNode *div_num = divisors_map[num];

	while(1) {
		if (div_num->div > div_den->div) {
			div_num = div_num->next;

			if (div_num == NULL)
				return 1;

		} else if (div_den->div > div_num->div) {
			div_den = div_den->next;

			if (div_den == NULL)
				return 1;

		} else {
			return div_den->div;
		}
	}
}


int **init_digits_map(void)
{
	int n;
	int **digits_map;

	digits_map = handle_malloc(sizeof(int *) * 100);

	for (n = 10; n < 100; ++n) {
		digits_map[n] = handle_malloc(sizeof(int) * 2);

		digits_map[n][0] = n / 10;
		digits_map[n][1] = n % 10;
	}

	return digits_map;
}


struct DivNode **init_divisors_map(void)
{
	int n;
	int big_divs[10];
	int sml_div;
	int big_div_i;
	struct DivNode *node;
	struct DivNode **divisors_map;

	divisors_map = handle_calloc(100, sizeof(struct DivNode *));

	big_div_i = -1;

	for (n = 10; n < 100; ++n) {
		big_divs[0] = n;
		big_div_i = 0;
		sml_div = 2;

		do {
			if (n % sml_div == 0) {
				node = handle_malloc(sizeof(struct DivNode));

				node -> div = sml_div;
				node -> next = divisors_map[n];

				divisors_map[n] = node;

				++big_div_i;
				big_divs[big_div_i] = n / sml_div;
			}

			++sml_div;

		} while (sml_div < big_divs[big_div_i]);

		do {
			node = handle_malloc(sizeof(struct DivNode));

			node -> div = big_divs[big_div_i];
			node -> next = divisors_map[n];

			divisors_map[n] = node;

			--big_div_i;

		} while(big_div_i > -1);
	}

	return divisors_map;
}
