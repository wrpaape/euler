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

#define PRIME_BKTS(base) (((size_t)			\
			   ((((double) base) * 90.0) /	\
			    log((double) base))) + 1u)
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
	int den;      /* denominator */
	int num;      /* numerator */
	int gcd;      /* greatest common divisor between 'num' & 'den' */
	int base_den; /* denominator of reduced form of 'num / den' */

	/* accumulators for 'curious fractions' */
	int num_acc;	 /* product of curious numerators */
	int den_acc;	 /* product of curious denominators */
	int fracs_found; /* counter for curious fractions generated */

	/*
	 * control for reducing final accumulators,
	 * where 'sml_div' * 'big_div' = 'num_acc'
	 */
	int sml_div; /* smaller divisor */
	int big_div; /* larger divisor */


	/*
	 * reference arrays for reducing redundant division and modulo
	 * operations where properties of a given 2-digit number 'n'
	 * are cached at index 'n' of each array
	 */
	int **digits_map;	       /*
					* array of digits, i.e
					* digits[15] = {1, 5}
					*/
	struct DivNode **divisors_map; /*
					* array of lists of even divisors
					* for 'n' in descending order,
					* excluding '1'
					*/

	digits_map   = init_digits_map();
	divisors_map = init_divisors_map();

	fracs_found = 0;
	num_acc = 1;
	den_acc = 1;

	for (den = 11; den < 100; ++den) {

		for (num = 10; num < den; ++num) {

			gcd = greatest_common_divisor(num, den, divisors_map);

			if (gcd == 1 || gcd % 10 == 0)
				continue;

			base_den = den / gcd;

			if (base_den > 9)
				continue;

			if (is_curious(num,	  den,
				       num / gcd, base_den, digits_map)) {

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
 *				- problem_34 -				*
 *									*
 * 145 is a curious number, as 1! + 4! + 5! = 1 + 24 + 120 = 145.	*
 *									*
 * Find the sum of all numbers which are equal to the sum of the	*
 * factorial of their digits.						*
 *									*
 * Note: as 1! = 1 and 2! = 2 are not sums they are not included.	*
 ************************************************************************/
void problem_34(char *result_buffer)
{
	int n;
	int digit_i;
	int num_digits;
	int sum_dig_facts;
	int sum_curious_n;
	int fact;
	int next_fact;
	int fact_deltas[10];
	int *digit_deltas[7];

	for (n = 0, fact = 1; n < 9; ++n, fact = next_fact) {
		next_fact = fact * (n + 1);
		fact_deltas[n] = next_fact - fact;
	}

	fact_deltas[9] = 1 - fact;

	for (digit_i = 0; digit_i < 7; ++digit_i)
		digit_deltas[digit_i] = fact_deltas;


	sum_curious_n = 0;
	n = 10;
	num_digits = 2;
	sum_dig_facts = 2;
	++digit_deltas[1];

	while (1) {
		++n;
		digit_i = 0;

		while (1) {
			sum_dig_facts += *digit_deltas[digit_i];

			if (*digit_deltas[digit_i] < 0) {
				digit_deltas[digit_i] = fact_deltas;
				++digit_i;

				if (digit_i == num_digits) {
					++num_digits;

					if (num_digits > 7)
						goto DONE;

					++sum_dig_facts;
				}

			} else {
				++digit_deltas[digit_i];
				break;
			}
		}

		if (sum_dig_facts == n)
			sum_curious_n += n;
	}

DONE:
	sprintf(result_buffer, "%d", sum_curious_n);
}


/************************************************************************
 *				- problem_35 -				*
 *									*
 * The number, 197, is called a circular prime because all rotations of	*
 * the digits: 197, 971, and 719, are themselves prime.			*
 *									*
 * There are thirteen such primes below 100: 2, 3, 5, 7, 11, 13, 17,	*
 * 31, 37, 71, 73, 79, and 97.						*
 *									*
 * How many circular primes are there below one million?		*
 ************************************************************************/
void problem_35(char *result_buffer)
{
	size_t hash;
	int prime_val;
	int circ_count;
	int next_base;
	struct IntNode *prime;
	int dig_buff[6];
	size_t *count_map;
	size_t num_bkts;
	size_t num_digs;
	size_t *bkt_count;

	for (prime = atkin_sieve(999);
	     prime->val < 100;
	     prime = prime->nxt);

	void (*sort_nets[4])(int *) = {
		sort_net_3, sort_net_4, sort_net_5, sort_net_6
	};

	void (**sort_digits)(int *) = &sort_nets[0];


	num_digs   = 3u;
	next_base  = 1000;
	num_bkts   = PRIME_BKTS(100);
	count_map  = handle_calloc(num_bkts, sizeof(size_t));
	circ_count = 0;

	do {
		prime_val = prime->val;

		if (prime_val > next_base) {
			free(count_map);
			num_bkts = PRIME_BKTS(next_base);
			count_map = handle_calloc(num_bkts, sizeof(size_t));
			++sort_digits;
			++num_digs;
			next_base *= 10;
		}

		hash = hash_digits(prime_val, dig_buff, *sort_digits);
		/* printf("%c\n", dig_buff[0] + '0'); */
		/* printf("%c\n", dig_buff[1] + '0'); */
		/* printf("%c\n", dig_buff[2] + '0'); */
		/* printf("pval: %d\n", prime_val); */
		/* printf("hash: %d\n", hash); */
		/* printf("index: %lu\n", hash % num_bkts); */
		/* printf("num_bkts: %lu\n", num_bkts); */
		/* exit(0); */
		if (hash == 341839278u) {
			for (int i = 0; i < num_digs; ++i)
				printf("%c\n", dig_buff[i] + '0');

			printf("hash: %zu\n", hash);
			printf("prime_val%d\n", prime_val);
			printf("*bkt_count: %zu\n", *bkt_count);
		}

		bkt_count = &count_map[hash % num_bkts];
		++(*bkt_count);

		if ((*bkt_count) == num_digs) {
			++circ_count;
			printf("hash: %lu\n", hash);
			printf("prime_val%d\n", prime_val);
			printf("*bkt_count: %zu\n", *bkt_count);
		}

		prime = prime->nxt;

	} while (prime != NULL);


	sprintf(result_buffer, "%d", circ_count);
}


size_t hash_digits(int n, int *dig_buff, void (*sort_digits)(int *))
{
	int i = 0;
	/* size_t hash = 0; */
	size_t hash = 2166136261u;

	while (1) {
		dig_buff[i] = n % 10;
		n    /= 10;
		if (n == 0)
			break;
		++i;
	}

	sort_digits(dig_buff);

	do {
		hash = (hash * 16777619) ^ dig_buff[i];
		/* hash ^= (hash << 5) + (hash >> 2) + dig_buff[i]; */
		/* hash = (33 * hash) ^ dig_buff[i]; */
		--i;
	} while (i > -1);

	return hash;
}
/************************************************************************
 *				HELPERS					*
 ************************************************************************/
bool is_curious(int num, int den, int base_num, int base_den, int **digits_map)
{
	if (digits_map[num][0] == digits_map[den][1]) {
		return mult_match(base_num, digits_map[num][1],
				  base_den, digits_map[den][0]);
	}

	if (digits_map[num][1] == digits_map[den][0]) {
		return mult_match(base_num, digits_map[num][0],
				  base_den, digits_map[den][1]);
	}

	return false;
}


inline bool mult_match(int base_num, int match_num, int base_den, int match_den)
{
	if ((base_num == match_num) && (base_den == match_den))
		return true;

	for (int den = base_den * 2, num = base_num * 2;
	     den < 10;
	     den += base_den,        num += base_num) {

		if ((den == match_den) && (num == match_num))
			return true;
	}

	return false;
}


/*
 * returns the greatest common divisor bewtween 'num' and 'den' by traversing
 * their lists of divisors
 */
int greatest_common_divisor(int num, int den, struct DivNode **divisors_map)
{
	/* skip identity DivNode for denominator */
	struct DivNode *div_den = divisors_map[den]->next;

	/* if denominator is a prime number, return 1 */
	if (div_den == NULL)
		return 1;

	struct DivNode *div_num = divisors_map[num];

	while (1) {
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

	for (n = 10; n < 100; ++n) {
		big_divs[0] = n;
		big_div_i = 0;
		sml_div = 2;

		do {
			if (n % sml_div == 0) {
				node = handle_malloc(sizeof(struct DivNode));

				node -> div  = sml_div;
				node -> next = divisors_map[n];

				divisors_map[n] = node;

				++big_div_i;
				big_divs[big_div_i] = n / sml_div;
			}

			++sml_div;

		} while (sml_div < big_divs[big_div_i]);

		do {
			node = handle_malloc(sizeof(struct DivNode));

			node -> div  = big_divs[big_div_i];
			node -> next = divisors_map[n];

			divisors_map[n] = node;

			--big_div_i;

		} while (big_div_i > -1);
	}

	return divisors_map;
}
