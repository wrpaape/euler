/************************************************************************************
 *                                     sets.h                                       *
 *                                                                                  *
 * Defines constants, declares helper functions, and includes libraries shared      *
 * amongst set modules.                                                             *
 ************************************************************************************/
/************************************************************************************
 *                             PREPROCESSOR DIRECTIVES                              *
 ************************************************************************************/
#ifndef _sets_h_included_
#define _sets_h_included_
/* <GUARD MACRO> */

#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>
#include <string.h>
#include <memory.h>
#include <stdbool.h>
#include <math.h>
#include <errno.h>
#include <limits.h>
#include <pthread.h>
#include <setjmp.h>

#define FORMAT_ERROR(MSG) "\n\e[31m\e[5mERROR\e[25m\n  " #MSG "\e[0m\n"
/************************************************************************************
 *                               INTIAL DECLARATIONS                                *
 ************************************************************************************/
struct IntNode {
	int val;
	struct IntNode *nxt;
};

struct SquareTerms {
	const int *X_SQ_3;
	const int *X_SQ_4;
};

struct SieveArg {
	int start;
	int until;
	struct IntNode *head;
	struct IntNode *last;
};
/************************************************************************************
 *                               FUNCTION PROTOTYPES                                *
 ************************************************************************************/
struct IntNode *prime_sieve(const int sup);
struct IntNode *atkin_sieve(const int sup);
bool bpsw_prime_test(const uint64_t n);
int greatest_common_divisor(int x, int y);
void *sieve_range(void *arg);
static inline bool flp1(const int n, struct SquareTerms *SQ_TERMS);
static inline bool flp2(const int n, struct SquareTerms *SQ_TERMS);
static inline bool flp3(const int n, struct SquareTerms *SQ_TERMS);
static inline int priv_nth_pow(int lil, int big, int n);
static inline uint64_t priv_nth_pow_u64(uint64_t lil, uint64_t big, int n);
static bool not_base_2_strong_probable_prime(const uint64_t n);
static int jacobi_symbol(int64_t top, int64_t bot, int jacobi);
bool is_strong_lucas_pseudoprime(const uint64_t d, const uint64_t n);
/************************************************************************************
 *                           INLINE FUNCTION DEFINITIONS                            *
 ************************************************************************************/
inline void *handle_malloc(const size_t total_bytes)
{
  void *ptr = malloc(total_bytes);
  if (ptr == NULL) {
    fprintf(stderr,
	    FORMAT_ERROR(failed to allocate "%lu" bytes total),
        total_bytes);
    exit(1);
  }

  return ptr;
}

inline void *handle_calloc(const size_t count, const size_t indiv_bytes)
{
  void *ptr = calloc(count, indiv_bytes);

  if (ptr == NULL) {
    fprintf(stderr,
	    FORMAT_ERROR(failed to allocate "%lu" count of "%lu" bytes),
        count, indiv_bytes);
    exit(1);
  }

  return ptr;
}

inline FILE *handle_fopen(const char *filename, const char *mode)
{
  FILE *ptr = fopen(filename, "r");

  if (ptr == NULL) {
    fprintf(stderr,
	    FORMAT_ERROR(failed to open %s in mode "%s"\n\n  reason: %s),
        filename, mode, strerror(errno));
    exit(1);
  }

  return ptr;
}

inline void handle_pthread_create(pthread_t *thread_ptr,
                                  const pthread_attr_t *attr,
                                  void *(*start_routine)(void *),
                                  void *arg)
{
  int return_code = pthread_create(thread_ptr, attr, start_routine, arg);

  if (return_code != 0) {
    fprintf(stderr, FORMAT_ERROR(pthread_create failure\n\n  return code: "%d"),
        return_code);
    exit(1);
  }
}

inline void handle_pthread_join(pthread_t thread, void **return_value)
{
  int return_code = pthread_join(thread, return_value);

  if (return_code != 0) {
    fprintf(stderr, FORMAT_ERROR(pthread_join failure\n\n  return code: "%d"),
        return_code);
    exit(1);
  }
}

inline int nth_pow(int base, int n)
{
	return priv_nth_pow(1, base, n);
}

inline int priv_nth_pow(int lil, int big, int n)
{
	if (n == 0) return lil;

	if (n == 1) return big * lil;

	if (n & 1)  return priv_nth_pow(big * lil, big * big, (n - 1) / 2);

	else	    return priv_nth_pow(lil,       big * big, n / 2);
}

inline uint64_t nth_pow_u64(uint64_t base, int n)
{
	return priv_nth_pow64(1, base, n);
}

inline uint64_t priv_nth_pow_u64(uint64_t lil, uint64_t big, int n)
{
	if (n == 0) return lil;

	if (n == 1) return big * lil;

	if (n & 1)  return priv_nth_pow(big * lil, big * big, (n - 1) / 2);

	else	    return priv_nth_pow(lil,       big * big, n / 2);
}


/* http://graphics.stanford.edu/~seander/bithacks.html#IntegerLog */
#define _LOG2A(s) ((s &0xffffffff00000000) ? (32 + _LOG2B(s >> 32)) : (_LOG2B(s)))
#define _LOG2B(s) ((s &0xffff0000)         ? (16 + _LOG2C(s >> 16)) : (_LOG2C(s)))
#define _LOG2C(s) ((s &0xff00)             ? (8  + _LOG2D(s >> 8))  : (_LOG2D(s)))
#define _LOG2D(s) ((s &0xf0)               ? (4  + _LOG2E(s >> 4))  : (_LOG2E(s)))
#define _LOG2E(s) ((s &0xc)                ? (2  + _LOG2F(s >> 2))  : (_LOG2F(s)))
#define _LOG2F(s) ((s &0x2)                ? (1)                    : (0))

#define LOG2_UINT64 _LOG2A
#define LOG2_UINT32 _LOG2B
#define LOG2_UINT16 _LOG2C
#define LOG2_UINT8  _LOG2D

inline uint64_t next_power_of_2(uint64_t i)
{
#if defined(__GNUC__)
	return 1UL <<(1 +(63 - __builtin_clzl(i - 1)));
#else
	i = i - 1;
	i = LOG2_UINT64(i);
	return 1UL <<(1 + i);
#endif
}

/* </GUARD MACRO> */
#endif
