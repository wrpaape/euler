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

	struct PrimeBucket {
		size_t count;
		size_t primes[10];
	};

	struct IntNode *prime;
	int prime_val;
	int digit;
	int dig_buff[6];
	int num_digs;
	int hash_cycle;
	int next_base;
	size_t dig_i;
	size_t circ_count;
	/* size_t *count_map; */
	struct PrimeBucket *count_map;
	struct PrimeBucket *bucket;
	size_t hash;
	size_t num_bkts;
	size_t num_perms;
	size_t *bkt_count;
	void (**sort_digits)(int *);

	void (*sort_nets[4])(int *) = {
		sort_net_3, sort_net_4, sort_net_5, sort_net_6
	};


	for (prime = atkin_sieve(999);
	     prime->val < 100;
	     prime = prime->nxt);

	num_digs    = 3;
	num_bkts    = num_prime_buckets(100);
	/* count_map   = handle_calloc(num_bkts, sizeof(size_t)); */
	count_map   = handle_calloc(num_bkts,
				    sizeof(struct PrimeBucket));
	sort_digits = &sort_nets[0];
	next_base   = 1000;
	circ_count  = 13u;
	num_perms   = 6;

		/* for (int i = 0; i < num_bkts; ++i) { */
		/* 	printf("%lu", count_map[i]); */
		/* 	fflush(stdout); */
		/* 	usleep(1000); */
		/* } */
		/* exit(0); */

	do {
		prime_val = (size_t) prime->val;

		int val = prime_val;

		if (prime_val > next_base) {
			free(count_map);
			num_bkts  = num_prime_buckets(next_base);
			/* count_map = handle_calloc(num_bkts, sizeof(size_t)); */
			count_map = handle_calloc(num_bkts,
						  sizeof(struct PrimeBucket));
			++sort_digits;
			++num_digs;
			num_perms *= num_digs;
			next_base *= 10u;
		}


		for (dig_i = 0; dig_i < num_digs; ++dig_i, prime_val /= 10) {

			digit = prime_val % 10;

			if ((digit & 1) == 0)
				goto NEXT_PRIME;

			dig_buff[dig_i] = digit;
		}

		(*sort_digits)(dig_buff);

		hash = 0;
		hash = hash_digits( dig_buff,
				    num_digs,
				    hash);
		/* for (hash_cycle = 0; hash_cycle < 5; ++hash_cycle) { */
			/* hash = jenkins_hash((unsigned char *) dig_buff, */
			/* 		    num_digs * sizeof(int), */
			/* 		    hash); */
			/* hash = hash_digits( dig_buff, */
			/* 		    num_digs, */
			/* 		    hash); */
		/* } */


		/* bkt_count = &count_map[hash & (num_bkts - 1)]; */
		bucket = &count_map[hash & (num_bkts - 1)];


		bucket->primes[bucket->count] = val;

		++(bucket->count);

		if (bucket->count >= num_digs) {
			circ_count += num_perms;
			for (int i = 0; i < 10; ++i) {
				printf("primes[%d]: %lu\n", i, bucket->primes[i]);
			}
		}



		/* ++(*bkt_count); */

		/* if ((*bkt_count) > num_digs) { */
		/* 	circ_count += num_digs; */
		/* 	printf("*****\n****\n%d\n", val); */
		/* 	printf("hash: %lu\n", hash); */
		/* 	printf("*bkt_count: %zu\n", *bkt_count); */
		/* 	printf("index: %zu\n", hash & (num_bkts - 1)); */
		/* 	printf("num_bkts: %zu\n", num_bkts); */
		/* } */

NEXT_PRIME:
		prime = prime->nxt;

	} while (prime != NULL);


	sprintf(result_buffer, "%zu", circ_count);
}

/************************************************************************
 *				HELPERS					*
 ************************************************************************/


size_t hash_digits(int *dig_buff, const int num_digs, size_t hash)
{
	for (int dig_i = 0; dig_i < num_digs; ++dig_i) {
		hash += dig_buff[dig_i];
		hash += (hash << 10);
		hash ^= (hash >> 6);
	}

	hash += (hash << 3);
	hash ^= (hash >> 11);
	hash += (hash << 15);

	return hash;
}


size_t num_prime_buckets(size_t base)
{
	uint64_t num_primes = (uint64_t) ((((double) base) * 9.0) /
					  log((double) base));

	return (size_t) next_power_of_2(num_primes);
}


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
