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
