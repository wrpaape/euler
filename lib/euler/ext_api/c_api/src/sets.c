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
/************************************************************************************
 *                               TOP LEVEL FUNCTIONS                                *
 ************************************************************************************/
struct IntNode *atkin_sieve(int upto)
{
	if ((upto & 1) == 0)
		--upto;

	struct IntNode *primes;
	struct IntNode *prime;
	struct IntNode *candidates;
	struct IntNode *cand;
	struct SieveArg *args[4];
	pthread_t sieve_threads[4];
	int q_i;
	int from;
	const int delta = (upto - 15) / 4;


	/* split range into 4 intervals and sieve in parallel */
	for (q_i = 0, from = 7; q_i < 4; ++q_i, from += (delta + 2)) {

		args[q_i].from = from;
		args[q_i].upto = from + delta;

		handle_pthread_create(&sieve_threads[q_i],
				      NULL,
				      sieve_range,
				      (void *) &args[q_i]);

	}
	/* await threads, join prime 'candidates' */
	handle_pthread_join(sieve_threads[0], NULL);
	candidates = args[0].head;
	cand	   = args[0].last;

	for (q_i = 1; q_i < 4; ++q_i) {
		handle_pthread_join(sieve_threads[q_i], NULL);
		cand->nxt = args[q_i].head;
		cand	  = args[q_i].last;
	};

	cand->nxt = NULL;



	primes = handle_malloc(sizeof(struct IntNode) * 3);
	prime  = primes;
	prime->val = 2; prime->nxt = prime + 1; ++prime;
	prime->val = 3; prime->nxt = prime + 1; ++prime;
	prime->val = 5; prime->nxt = NULL;

	for (prime = primes; prime != NULL; prime = prime->nxt)
		printf("prime->val%d\n", prime->val);

	return primes;
}


void *sieve_range(void *arg)
{
	struct IntNode *primes;
	struct IntNode *prime;
	struct IntNode *candidates;
	struct IntNode *cand;

	struct SieveArg *

	enum RemainderCase {
		IGN, ONE, TWO, THR
	};

	const enum RemainderCase rem_map[60] = {
		IGN, ONE, IGN, IGN, IGN, IGN, IGN, TWO, IGN, IGN,
		IGN, THR, IGN, ONE, IGN, IGN, IGN, ONE, IGN, TWO,
		IGN, IGN, IGN, THR, IGN, IGN, IGN, IGN, IGN, ONE,
		IGN, TWO, IGN, IGN, IGN, IGN, IGN, ONE, IGN, IGN,
		IGN, ONE, IGN, TWO, IGN, IGN, IGN, THR, IGN, ONE,
		IGN, IGN, IGN, ONE, IGN, IGN, IGN, IGN, IGN, THR
	};




	candidates = handle_malloc(sizeof(struct IntNode) *
				   ((upto - 5) / 2));

	for (n = 7, cand = candidates; n < upto; n+=2, ++cand) {
		cand->val = n;
		cand->nxt = cand + 1;
	}
	cand->val = upto;
	cand->nxt = NULL;


	for (cand = candidates; cand != NULL; cand = cand->nxt) {
		n = cand->val;

		prime->nxt = handle_malloc(sizeof(struct IntNode));
		prime = prime->nxt;
		prime->val = n;

		switch (rem_map[n % 60]) {
			case ONE:
				break;
			case TWO:
				break;
			case THR:
				break;
			default:
				/* do nothing */
				break;
		}
	}

	free(candidates);


	pthread_exit(NULL); /* must return something for pthread routine */
}


struct IntNode *prime_sieve(int upto)
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
