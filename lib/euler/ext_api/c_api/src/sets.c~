/************************************************************************************
 *                                     sets.c                                       *
 *                                                                                  *
 * Defines constants, declares helper functions, and includes libraries shared      *
 * amongst set modules.                                                             *
 ************************************************************************************/
/************************************************************************************
 *                             PREPROCESSOR DIRECTIVES                              *
 ************************************************************************************/
#include "sets.h"
/************************************************************************************
 *                               INTIAL DECLARATIONS                                *
 ************************************************************************************/
static bool (*FLIP_MAP[60])(const int, struct SquareTerms *terms) = {
	NULL, flp1, NULL, NULL, NULL, NULL, NULL, flp2, NULL, NULL,
	NULL, flp3, NULL, flp1, NULL, NULL, NULL, flp1, NULL, flp2,
	NULL, NULL, NULL, flp3, NULL, NULL, NULL, NULL, NULL, flp1,
	NULL, flp2, NULL, NULL, NULL, NULL, NULL, flp1, NULL, NULL,
	NULL, flp1, NULL, flp2, NULL, NULL, NULL, flp3, NULL, flp1,
	NULL, NULL, NULL, flp1, NULL, NULL, NULL, NULL, NULL, flp3
};
/************************************************************************************
 *                            INLINE FUNCTION PROTOTYPES                            *
 ************************************************************************************/
extern inline void *handle_malloc(const size_t total_bytes);
extern inline void *handle_calloc(const size_t count, const size_t indiv_bytes);
extern inline FILE *handle_fopen(const char *filename, const char *mode);
extern inline void handle_pthread_create(pthread_t *thread,
                                         const pthread_attr_t *attr,
                                         void *(*start_routine)(void *),
                                         void *arg);
extern inline void handle_pthread_join(pthread_t thread, void **return_value);
extern inline int nth_pow(int base, int n);
extern inline long long int nth_powll(long long int base, int n);
extern inline unsigned long long int nth_powull(unsigned long long int base,
						int n);
extern inline uint64_t next_power_of_2(uint64_t i);
extern inline unsigned long long int two_exp_mod(unsigned long long int n,
						 unsigned long long int div);
/************************************************************************************
 *                               TOP LEVEL FUNCTIONS                                *
 ************************************************************************************/
bool bpsw_prime_test(const unsigned long long int n)
{
	if (not_base_2_strong_probable_prime(n))
		return false;

	unsigned long long int big_d = 5;

	int neg_jacobi = ((n & 3) == 3) ? -1 : 1;

	while (1) {
		if (jacobi_symbol(big_d, n, 1) == -1)
			return is_strong_lucas_pseudoprime(big_d, n);

		big_d += 2;

		if (jacobi_symbol(big_d, n, neg_jacobi) == -1)
			return is_strong_lucas_pseudoprime(-big_d, n);

		big_d += 2;
	}

}


bool is_strong_lucas_pseudoprime(const long long int big_d,
				 const unsigned long long int n)
{
	const unsigned long long int n_plus_one = n + 1ll;
	const long long int q = (1ll - big_d) / 4ll; /* 75% (naive) of time a power of 2 */
	unsigned long long int d = n_plus_one;
	unsigned int s = 0u;

	long long int q_term;
	long long int (*pow_fun)(const long long int, const int);

	if (q == 1ll) {
		pow_fun = pow_pos_one;

	} else if (q == -1ll) {
		pow_fun = pow_neg_one;

	} else if ((q & (q - 1ll)) == 0) {
		pow_fun = (q < 0) ? pow_neg_pow_two : pow_pos_pow_two;
		q_term  = __builtin_ctzll(q);

	} else {
		pow_fun = nth_powll;
		q_term  = q;
	}


	/* while d is even... */
	while ((d & 1) == 0) {
		++s;
		d = n_plus_one >> s;
	}


	long long int u_prev;
	long long int u_k = 1ll;
	long long int v_k = 1ll;
	int k = 1;
	int prev_k = 0;
	long long int q_raised_k = 1ll;


	for (int shift = ((sizeof(d) * CHAR_BIT) - 2) - __builtin_clzll(d);
	     shift > -1; --shift) {

		q_raised_k *= pow_fun(q_term, k - prev_k);

		prev_k = k;
		k *= 2;

		u_k *= v_k;
		v_k = (v_k * v_k) - (q_raised_k * 2ll);

		if ((d >> shift) & 1) {
			u_prev = u_k;
			u_k = ((u_prev           + v_k) / 2ll);
			v_k = (((big_d * u_prev) + v_k) / 2ll);
			++k;
		}
	}


	if ((u_k % n) == 0ll)
		return true;

	if (s == 0u)
		return false;

	if ((v_k % n) == 0ll)
		return true;


	while (1) {
		q_raised_k *= q_raised_k;

		printf("q_raised_k: %lld\n", q_raised_k);
		printf("q: %lld\n", q);
		printf("k: %d\n", k);
		printf("LLONG_MIN: %lld\n", LLONG_MIN);
		printf("LLONG_MAX:  %lld\n", LLONG_MAX);
		printf("v_k:       %lld\n", v_k);

		v_k = (v_k * v_k) - (q_raised_k * 2ll);

		if ((v_k % n) == 0ll)
			return true;

		--s;

		if (s == 0llu)
			return false;
	}
}

int jacobi_symbol(unsigned long long int top,
		  unsigned long long int bot,
		  int jacobi)
{
	int num_facts_2;
	int temp;
	unsigned int top_mod_8;

	while (1) {
		bot %= top;

		if (bot == 0)
			return (top == 1) ? jacobi : 0;

		num_facts_2 = 0;

		while ((bot & 1) == 0) {
			++num_facts_2;
			bot /= 2;
		}

		/* if an odd number of '2's were factored from 'bot'... */
		if (num_facts_2 & 1) {

		 top_mod_8 = (unsigned int) (top & 7);

			/* if 'top mod 8' = 3 or 5... */
			if ((top_mod_8 == 3u) || (top_mod_8 == 5u))
				jacobi = -jacobi; /* flip sign */
		}


		if (((top & 3) == 3) && ((bot & 3) == 3))
			jacobi = -jacobi;

		temp = bot;
		bot  = top;
		top  = temp;
	}
}


bool not_base_2_strong_probable_prime(const unsigned long long int n)
{
	/*
	 * find first odd strong pseudoprime 'd' and exponent 's' s.t.
	 * n = d * 2^s + 1
	 */
	const unsigned long long int n_minus_one = n - 1llu;
	unsigned long long int d = n_minus_one;
	unsigned long long int s = 0lu;

	/* while d is even... */
	while ((d & 1lu) == 0lu) {
		++s;
		d = n_minus_one >> s;
	}

	/* for base 'a' = 2, if a^d % n = 1... */
	unsigned long long int a_raised_d_mod_n = two_exp_mod(d, n);

	 if (a_raised_d_mod_n == 1llu)
		 return false; /* 'n' is a base 2 strong provable prime, bail */

	 if (s == 0)
		 return true;

	 if (a_raised_d_mod_n == n_minus_one)
		 return false; /* 'n' is a base 2 strong provable prime, bail */

	 /* otherwise test second condition: */
	 for (unsigned long long int r = 1llu; r < s; ++r) {

		 /* if 2^(d * 2^r) % n = n - 1 for 0 ≤ r < s... */
		 if (two_exp_mod(d << r, n) == n_minus_one)
			 return false; /* 'n' is a base 2 strong provable prime */
	 }

	 return true; /* 'n' is not a base 2 strong provable prime */
}


int greatest_common_divisor(int x, int y)
{
	if (y == 0)
		return x;

	int gcd = y;
	int next_rem = x % y;
	int prev_rem;

	while (next_rem != 0) {

		prev_rem = next_rem;

		next_rem = gcd % next_rem;

		gcd = prev_rem;
	}

	return gcd;
}



struct IntNode *prime_sieve(const int sup)
{
	struct IntNode *primes = handle_malloc(sizeof(struct IntNode));
	struct IntNode *num;
	struct IntNode *prv;
	struct IntNode *prime;
	int prime_val;

	num = primes;
	num->val = 2;

	/* initialize sieve to all odd numbers between '2' and input supremum 'sup' */
	for (int odd = 3; odd < sup; odd += 2) {
		num->nxt = handle_malloc(sizeof(struct IntNode));
		num = num->nxt;
		num->val = odd;
	}

	num->nxt = NULL;

	for (prime = primes->nxt; prime != NULL; prime = prime->nxt) {

		prime_val = prime->val;
		prv = prime;
		num = prv->nxt;

		while (num != NULL) {

			if (num->val % prime_val == 0) {
				prv->nxt = num->nxt;
				free(num);
				num = prv->nxt;
			} else {
				prv = num;
				num = num->nxt;
			}
		}
	}

	return primes;
}


struct IntNode *atkin_sieve(const int sup)
{
	pthread_t sieve_threads[4];
	struct SieveArg args[4];
	struct IntNode *primes;
	struct IntNode *prime;
	struct IntNode *prev;
	struct IntNode *cand;
	int q_i;
	int start;
	int prime_val;
	int sq_val;
	int mult_val;
	int mult_sq;
	const int DELTA = (sup - 13) / 4;

	/* initialize result list 'primes' with first 5 prime numbers */
	primes = handle_malloc(sizeof(struct IntNode) * 5);
	prime  = primes;
	prime->val = 2; prime->nxt = prime + 1; ++prime;
	prime->val = 3; prime->nxt = prime + 1; ++prime;
	prime->val = 5; prime->nxt = prime + 1; ++prime;
	prime->val = 7; prime->nxt = prime + 1; ++prime;
	prime->val = 11;

	/* split range into 4 intervals and sieve in parallel */
	for (q_i = 0, start = 13; q_i < 3; ++q_i) {
		args[q_i].start = start;
		start	       += DELTA;
		args[q_i].until = start;

		handle_pthread_create(&sieve_threads[q_i],
				      NULL,
				      sieve_range,
				      (void *) &args[q_i]);
	}

	/* ensure final sieve interval ends at input supremum 'sup' */
	args[3].start = start;
	args[3].until = sup;

	handle_pthread_create(&sieve_threads[3],
			      NULL,
			      sieve_range,
			      (void *) &args[3]);

	/* await threads, join candidates and append to 'primes' */
	handle_pthread_join(sieve_threads[0], NULL);
	prime->nxt = args[0].head;
	cand	   = args[0].last;

	for (q_i = 1; q_i < 4; ++q_i) {
		handle_pthread_join(sieve_threads[q_i], NULL);
		cand->nxt = args[q_i].head;
		cand	  = args[q_i].last;
	}

	cand->nxt = NULL;
	const int SQ_CUTOFF = cand->val;

	/* starting at node 7... */
	--prime;
	prime_val = 7;
	sq_val	  = 49;

	while (1) {
		prev    = prime;
		cand    = prime->nxt;
		mult_sq = sq_val;

		while (1) {
			if (cand->val < mult_sq) {
				prev = cand;
				cand = cand->nxt;
				continue;
			}

			if (cand->val == mult_sq) {
				prev->nxt = cand->nxt;
				free(cand);
				cand = prev->nxt;

			} else {
				prev = cand;
				cand = cand->nxt;
			}

			mult_sq += sq_val;

			if (mult_sq > SQ_CUTOFF)
				break;
		}


		prime	  = prime->nxt;
		prime_val = prime->val;
		sq_val	  = prime_val * prime_val;

		if (sq_val > SQ_CUTOFF)
			return primes;
	}
}
/************************************************************************
 *				HELPERS					*
 ************************************************************************/
void *sieve_range(void *arg)
{
	bool (*flip_fun)(const int, struct SquareTerms *);
	struct IntNode *cand;

	struct SieveArg *params = (struct SieveArg *) arg;
	struct IntNode **prv    = &params->head;
	const int until		= params->until;
	/*
	 * case where x will be largest corresponds to the third flipping
	 * quadratic:
	 *
	 * 3x² - y² = n  where x > y
	 *
	 * when y = one less than x = (x - 1)
	 *
	 * substituting:
	 *
	 * 3x² - (x - 1)² = n
	 * 2x² + 2x - (1 + n) = 0
	 *
	 * from quadratic equation:
	 * x_max = (-1 + sqrt(3 + 2n)) / 2
	 *
	 * add one to ensure that range of x includes x_max:
	 */
	const int LENGTH_TERMS =
		((((int) sqrtf((float) ((until * 2) + 3))) - 1) / 2) + 1;

	int x_sq_3[LENGTH_TERMS];
	int x_sq_4[LENGTH_TERMS];

	for (int x = 2, x_sq; x < LENGTH_TERMS; ++x) {
		x_sq = x * x;
		x_sq_3[x] = 3 * x_sq;
		x_sq_4[x] = 4 * x_sq;
	}

	struct SquareTerms SQ_TERMS = {
		.X_SQ_3 = x_sq_3,
		.X_SQ_4 = x_sq_4
	};

	for (int n = params->start; n < until; n+=2) {

		flip_fun = FLIP_MAP[n % 60];

		if (flip_fun && flip_fun(n, &SQ_TERMS)) {
			cand = handle_malloc(sizeof(struct IntNode));
			cand->val = n;
			*prv = cand;
			prv  = &cand->nxt;
		}
	}

	params->last = cand;

	pthread_exit(NULL);
}

inline bool flp1(const int n, struct SquareTerms *SQ_TERMS)
{
	bool is_prime	 = false;
	const int *TERMS = SQ_TERMS->X_SQ_4;
	int y_sq	 = n - 4;
	int x		 = 2;
	int y;

	while (1) {
		y = (int) sqrtf((float) y_sq);

		if ((y * y) == y_sq)
			is_prime = !is_prime;

		y_sq = n - TERMS[x];

		if (y_sq < 1)
			return is_prime;

		++x;
	}
}

inline bool flp2(const int n, struct SquareTerms *SQ_TERMS)
{
	bool is_prime	 = false;
	const int *TERMS = SQ_TERMS->X_SQ_3;
	int y_sq	 = n - 3;
	int x		 = 2;
	int y;

	while (1) {
		y = (int) sqrtf((float) y_sq);

		if ((y * y) == y_sq)
			is_prime = !is_prime;

		y_sq = n - TERMS[x];

		if (y_sq < 1)
			return is_prime;

		++x;
	}
}

inline bool flp3(const int n, struct SquareTerms *SQ_TERMS)
{
	bool is_prime	 = false;
	const int *TERMS = SQ_TERMS->X_SQ_3;
	int x		 = (((int) sqrtf((float) ((n * 2) + 3))) - 1) / 2;
	int y_sq	 = TERMS[x] - n;
	int y;

	while (1) {
		y = (int) sqrtf((float) y_sq);

		if ((y * y) == y_sq)
			is_prime = !is_prime;

		--x;

		y_sq = TERMS[x] - n;

		if (y_sq < 1)
			return is_prime;
	}
}

/* for calculating q^(k - k_prev) */
inline long long int pow_pos_one(const long long int _, const int ___)
{
	return 1ll;
}
inline long long int pow_neg_one(const long long int _, const int pow)
{
	return (pow & 1) ? -1ll : 1ll;
}
inline long long int pow_pos_pow_two(const long long int init_shift,
				     const int pow)
{
	return 1ll << (init_shift * pow);
}
inline long long int pow_neg_pow_two(const long long int init_shift,
				     const int pow)
{
	return ((pow & 1) ? -1ll : 1ll) << (init_shift * pow);
}
