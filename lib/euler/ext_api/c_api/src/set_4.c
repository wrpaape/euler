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
	int ini_num;
	int ini_den;
	int red_num;
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

	for (ini_den = 11; ini_den < 100; ++ini_den) {


		red_den = ini_den;

		for (ini_num = 10; ini_num < ini_den; ++ini_num) {

			red_num = ini_num;

			if (attempt_reduce(&red_num, &red_den, mult_map) &&
			    is_non_trivial(ini_num, ini_den,
					   red_num, red_den, digs_map)) {

				printf("ini_num: %d\n", ini_num);
				printf("ini_den: %d\n", ini_den);
				fflush(stdout);

				num_acc *= ini_num;
				den_acc *= ini_den;
				++fracs_found;

				if (fracs_found == 4)
					goto FOUND_ALL_FRACTIONS;
				else
					red_den = ini_den;
			}
		}
	}

FOUND_ALL_FRACTIONS:
	sml_div = 2;

	while (1) {
		while (num_acc % sml_div != 0)
			++sml_div;

		big_div = num_acc / sml_div;

		if (den_acc % big_div == 0)
			break;

		if (sml_div < big_div) {
			++sml_div;
			continue;
		}

		big_div = 1;
		break;
	}

	 /* divide denominator by greatest common divisor and copy result to buffer */
	sprintf(result_buffer, "%d", den_acc / big_div);
}

/************************************************************************
 *				HELPERS					*
 ************************************************************************/
bool is_non_trivial(int ini_num, int ini_den,
		    int red_num, int red_den, int **digs_map)
{
	return true;
}

bool attempt_reduce(int *ini_num, int *ini_den, struct MultNode **mult_map)
{
	struct MultNode *mult_num;
	struct MultNode *mult_den;

	mult_num = mult_map[*ini_num];
	mult_den = mult_map[*ini_den];

	do {
		if (mult_den->mult > mult_num->mult) {
			mult_den = mult_den->next;

		} else if (mult_num->mult > mult_den->mult) {
			mult_num = mult_num->next;
		} else {
			(*ini_num) /= mult_num->mult;
			(*ini_den) /= mult_num->mult;
			return true;
		}

	} while((mult_den != NULL) && (mult_num != NULL));

	return false;
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
