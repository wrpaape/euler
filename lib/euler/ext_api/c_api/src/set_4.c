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
	int fracs_found;
	int num;
	int dem;
	int num_acc;
	int dem_acc;
	int com_digs[2];
	int **digs_map;
	struct MultNode **mult_map;
	struct Fraction frac;

	digs_map = init_digs_map();
	mult_map = init_mult_map();

	fracs_found = 0;
	num_acc = 1;
	dem_acc = 1;

	frac.num = 27;
	frac.dem = 81;

	reduce_fraction(&frac, mult_map);

	printf("frac.num %d\n", frac.num);
	printf("frac.dem %d\n", frac.dem);

	/* for (dem = 11; dem < 100; ++dem) { */
	/* 	for (num = 10; num < dem; ++num) { */

	/* 	} */
	/* } */

	/* struct MultNode *mult; */
	/* for (int n = 10; n < 100; ++n) { */
	/* 	printf("n: %d\n", n); */
	/* 	for (mult = mult_map[n]; mult != NULL; mult = mult -> next) { */
	/* 		printf("  mult: %d\n", mult -> mult); */
	/* 	} */

	/* 	printf("  0: %d\n", digs_map[n][0]); */
	/* 	printf("  1: %d\n", digs_map[n][1]); */
	/* } */

	sprintf(result_buffer, "%d", 42); /* copy score total to buffer */
}

/************************************************************************
 *				HELPERS					*
 ************************************************************************/
void reduce_fraction(struct Fraction *frac, struct MultNode **mult_map)
{
	struct MultNode *mult_num;
	struct MultNode *mult_dem;

	mult_num = mult_map[frac->num];
	mult_dem = mult_map[frac->dem];

	do {
		if (mult_dem->mult > mult_num->mult) {
			mult_dem = mult_dem->next;

		} else if (mult_num->mult > mult_dem->mult) {
			mult_num = mult_num->next;
		} else {
			frac->dem /= mult_num->mult;
			frac->num /= mult_num->mult;
			return;
		}

	} while((mult_dem != NULL) && (mult_num != NULL));
}


int **init_digs_map(void)
{
	int n;
	int **digs_map;

	digs_map = handle_malloc(sizeof(int *) * 100);

	for (n = 1; n < 100; ++n) {
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
	int small_div;
	int big_div_i;
	struct MultNode *next_mult;
	struct MultNode **mult_map;

	mult_map = handle_calloc(100, sizeof(struct MultNode *));

	big_div_i = -1;

	for (n = 10; n < 100; ++n) {
		big_divs[0] = n;
		big_div_i = 0;
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

		} while(big_div_i > -1);
	}

	return mult_map;
}
