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
extern inline bool is_perfect_square(int n);
/************************************************************************************
 *                               TOP LEVEL FUNCTIONS                                *
 ************************************************************************************/
struct IntNode *prime_sieve(const int upto)
{
	struct IntNode *primes = handle_malloc(sizeof(struct IntNode));
	struct IntNode *num;
	struct IntNode *prv;
	struct IntNode *prime;
	int prime_val;

	num = primes;
	num->val = 2;

	for (int n = 3; n < upto; n+=2) {
		num->nxt = handle_malloc(sizeof(struct IntNode));
		num = num->nxt;
		num->val = n;
	}

	if (upto & 1) {
		num->nxt = handle_malloc(sizeof(struct IntNode));
		num = num->nxt;
		num->val = upto;
	}

	num->nxt = NULL;

	prime = primes->nxt;

	while (prime != NULL) {
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

		prime = prime->nxt;
	}

	return primes;
}


struct IntNode *atkin_sieve(const int upto)
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
	const int DELTA = (upto - 7) / 4;

	/* initials result list 'primes' */
	primes = handle_malloc(sizeof(struct IntNode) * 3);
	prime  = primes;
	prime->val = 2; prime->nxt = prime + 1; ++prime;
	prime->val = 3; prime->nxt = prime + 1; ++prime;
	prime->val = 5;

	/* split range into 4 intervals and sieve in parallel */
	for (q_i = 0, start = 7; q_i < 3; ++q_i) {
		args[q_i].start = start;
		start	       += DELTA;
		args[q_i].until = start;

		handle_pthread_create(&sieve_threads[q_i],
				      NULL,
				      sieve_range,
				      (void *) &args[q_i]);
	}

	/* ensure entire range is sieved */
	args[3].start = start;
	args[3].until = upto + 1;

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

	while (1) {
		prime	  = prime->nxt;
		prime_val = prime->val;
		sq_val	  = prime_val * prime_val;

		if (sq_val > SQ_CUTOFF)
			return primes;

		mult_sq = sq_val;
		prev    = prime;
		cand    = prime->nxt;

		do {
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

		} while (mult_sq <= SQ_CUTOFF);
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
	 * 3x² - (x - 1)² = n
	 * 2x² + 2x - (1 + n) = 0
	 *
	 * from quadratic equation:
	 * x_max = (-1 + sqrt(3 + 2n)) / 2
	 *
	 * add one to ensure that range of x includes x_max:
	 */
	const int NUM_TERMS =
		((((int) sqrtf((float) ((until * 2) + 3))) - 1) / 2) + 1;

	int x_sq_3[NUM_TERMS];
	int x_sq_4[NUM_TERMS];

	for (int x = 0, x_sq; x < NUM_TERMS; ++x) {
		x_sq = x * x;
		x_sq_3[x] = 3 * x_sq;
		x_sq_4[x] = 4 * x_sq;
	}

	struct SquareTerms SQ_TERMS = {
		.X_SQ_3 = x_sq_3,
		.X_SQ_4 = x_sq_4,
		.x_min  = 2
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
	const int *TERMS = SQ_TERMS->X_SQ_4;
	bool is_prime	 = false;
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
	const int *TERMS = SQ_TERMS->X_SQ_3;
	bool is_prime	 = false;
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
	const int *TERMS = SQ_TERMS->X_SQ_3;
	bool is_prime = false;
	int x, y_sq, y;

	/* find 'x_min' that produces y² >= 1 */
	for (x = SQ_TERMS->x_min; TERMS[x] <= n; ++x);
	/* update 'x_min' for next 'flp3' case */
	SQ_TERMS->x_min = x;

	while (1) {
		y_sq = n - TERMS[x];
		y    = (int) sqrtf((float) y_sq);

		if (y < x) {
			if ((y * y) == y_sq)
				is_prime = !is_prime;

			++x;
		} else {
			return is_prime;
		}
	}
}
