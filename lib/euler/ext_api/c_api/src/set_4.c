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
	int red_den;
	int num_acc;
	int den_acc;
	int sml_div;
	int big_div;
	int fracs_found;
	int **digs_map;
	struct MultNode **mult_map;

	digs_map = init_digs_map();
	mult_map = init_mult_map();

	fracs_found = 0;
	num_acc = 1;
	den_acc = 1;

	for (den = 11; den < 100; ++den) {

		for (num = 10; num < den; ++num) {

			gcd = greatest_common_divisor(num, den, mult_map);

			if (gcd % 10 == 0)
				continue;

			red_den = den / gcd;

			if (red_den > 9)
				continue;

			if (is_curious(num,	  den,
				       num / gcd, red_den, digs_map)) {

				printf("num: %d\n", num);
				printf("den: %d\n", den);
				printf("fracs_found: %d\n", fracs_found);
				fflush(stdout);

				num_acc *= num;
				den_acc *= den;
				++fracs_found;

				if (fracs_found == 4)
					goto FOUND_ALL_FRACTIONS;
			}
		}
	}

FOUND_ALL_FRACTIONS:
			puts("DONE");
			printf("num: %d\n", num);
			printf("den: %d\n", den);
	sml_div = 2;

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
bool is_curious(int num, int den, int red_num, int red_den, int **digs_map)
{
	if (digs_map[num][0] == digs_map[den][1]) {
		return ((red_num == digs_map[num][1]) &&
			(red_den == digs_map[den][0]));
	}

	if (digs_map[num][1] == digs_map[den][0]) {
		return ((red_num == digs_map[num][0]) &&
			(red_den == digs_map[den][1]));
	}

	return false;
}

int greatest_common_divisor(int num, int den, struct MultNode **mult_map)
{
	struct MultNode *mult_den = mult_map[den]->next;

	/* if denominator is a prime number, return 1 */
	if (mult_den == NULL)
		return 1;

	struct MultNode *mult_num = mult_map[num];

	while(1) {
		if (mult_num->mult > mult_den->mult) {
			mult_num = mult_num->next;

			if (mult_num == NULL)
				return 1;

		} else if (mult_den->mult > mult_num->mult) {
			mult_den = mult_den->next;

			if (mult_den == NULL)
				return 1;

		} else {
			return mult_den->mult;
		}
	}
}


int **init_digs_map(void)
{
	int n;
	int **digs_map;

	digs_map = handle_malloc(sizeof(int *) * 100);

	for (n = 10; n < 100; ++n) {
		digs_map[n] = handle_malloc(sizeof(int) * 2);

		digs_map[n][0] = n / 10;
		digs_map[n][1] = n % 10;
	}

	return digs_map;
}


struct MultNode **init_mult_map(void)
{
	int n;
	int big_divs[10];
	int sml_div;
	int big_div_i;
	struct MultNode *next_mult;
	struct MultNode **mult_map;

	mult_map = handle_calloc(100, sizeof(struct MultNode *));

	big_div_i = -1;

	for (n = 10; n < 100; ++n) {
		big_divs[0] = n;
		big_div_i = 0;
		sml_div = 2;

		do {
			if (n % sml_div == 0) {
				next_mult = handle_malloc(sizeof(struct MultNode));

				next_mult -> mult = sml_div;
				next_mult -> next = mult_map[n];

				mult_map[n] = next_mult;

				++big_div_i;
				big_divs[big_div_i] = n / sml_div;
			}

			++sml_div;

		} while (sml_div < big_divs[big_div_i]);

		do {
			next_mult = handle_malloc(sizeof(struct MultNode));

			next_mult -> mult = big_divs[big_div_i];
			next_mult -> next = mult_map[n];

			mult_map[n] = next_mult;

			--big_div_i;

		} while(big_div_i > -1);
	}

	return mult_map;
}
