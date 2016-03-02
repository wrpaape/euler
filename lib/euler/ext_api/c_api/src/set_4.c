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
	struct MultNode *mult;
	struct MultNode **mult_map = init_mult_map();

	for (int n = 0; n < 100; ++n) {
		printf("n: %d\n", n);
		for (mult = mult_map[n]; mult != NULL; mult = mult -> next) {
			printf("  mult: %d\n", mult -> mult);
		}
	}

  sprintf(result_buffer, "%d", 42); /* copy score total to buffer */
}

/************************************************************************
 *				HELPERS					*
 ************************************************************************/
struct MultNode **init_mult_map(void)
{
	int n;
	int big_divs[10];
	int small_div;
	int big_div_i;
	struct MultNode *next_mult;
	struct MultNode **mult_map = handle_calloc(100, sizeof(struct MultNode *));

	for (n = 10; n < 100; ++n) {
		big_div_i = 0;
		big_divs[big_div_i] = n;
		small_div = 2;

		do {
			if (n % small_div == 0) {
				next_mult = handle_malloc(sizeof(struct MultNode));

				next_mult -> mult = small_div;
				next_mult -> next = mult_map[n];

				mult_map[n] = next_mult;

				++big_div_i;
				big_divs[big_div_i] = n / small_div;
			}

			++small_div;

		} while (small_div < big_divs[big_div_i]);


		do {
			next_mult = handle_malloc(sizeof(struct MultNode));

			next_mult -> mult = big_divs[big_div_i];
			next_mult -> next = mult_map[n];

			mult_map[n] = next_mult;

			--big_div_i;

		} while (big_div_i > -1);
	}

	return mult_map;
}
