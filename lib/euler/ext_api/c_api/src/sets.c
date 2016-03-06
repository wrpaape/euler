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
const int SQUARE_MASK	   = SQ_MASK;
const uint32_t START[1024] = STRT;
const bool BAD255[512]     = BD255;

static bool (*FLIP_MAP[60])(const int) = {
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
	struct IntNode *primes;
	struct IntNode *prime;
	struct IntNode *candidates;
	struct IntNode *cand;
	struct SieveArg args[4];
	pthread_t sieve_threads[4];
	int q_i;
	int start;
	const int delta = (upto - 7) / 4;

	/* split range into 4 intervals and sieve in parallel */
	for (q_i = 0, start = 7; q_i < 3; ++q_i) {
		args[q_i].start = start;
		start	       += delta;
		args[q_i].until = start;

		handle_pthread_create(&sieve_threads[q_i],
				      NULL,
				      sieve_range,
				      (void *) &args[q_i]);
	}
		args[3].start = start;
		args[3].until = upto + 1;

		handle_pthread_create(&sieve_threads[3],
				      NULL,
				      sieve_range,
				      (void *) &args[3]);


	/* await threads, join prime 'candidates' */
	handle_pthread_join(sieve_threads[0], NULL);
	candidates = args[0].head;
	cand	   = args[0].last;

	for (q_i = 1; q_i < 4; ++q_i) {
		handle_pthread_join(sieve_threads[q_i], NULL);
		cand->nxt = args[q_i].head;
		cand	  = args[q_i].last;
	}

	cand->nxt = NULL;

	/* for (cand = candidates; cand != NULL; cand = cand->nxt) */
	/* 	printf("cand->val%d\n", cand->val); */


	primes = handle_malloc(sizeof(struct IntNode) * 3);
	prime  = primes;
	prime->val = 2; prime->nxt = prime + 1; ++prime;
	prime->val = 3; prime->nxt = prime + 1; ++prime;
	prime->val = 5; prime->nxt = NULL;

	/* for (prime = primes; prime != NULL; prime = prime->nxt) */
	/* 	printf("prime->val%d\n", prime->val); */

	return primes;
}
/************************************************************************
 *				HELPERS					*
 ************************************************************************/
void *sieve_range(void *arg)
{
	bool (*flip_fun)(const int);
	struct IntNode *cand;

	struct SieveArg *params = (struct SieveArg *) arg;
	struct IntNode **prv    = &params->head;
	const int until		= params->until;

	for (int n = params->start; n < until; n+=2) {

		flip_fun = FLIP_MAP[n % 60];

		if (flip_fun && flip_fun(n)) {
			cand = handle_malloc(sizeof(struct IntNode));
			cand->val = n;
			*prv = cand;
			prv  = &cand->nxt;
		}
	}

	params->last = cand;

	pthread_exit(NULL);
}

inline bool flp1(const int n)
{
	int x, y, x_cmp, y_sq;
	bool is_prime = false;

	y = 0;

	while (1) {
		x_cmp =  n - (y * y);



	}


	return is_prime;
}

inline bool flp2(const int n)
{
	bool is_prime = false;
	return is_prime;
}

inline bool flp3(const int n)
{
	bool is_prime = false;
	return is_prime;
}
