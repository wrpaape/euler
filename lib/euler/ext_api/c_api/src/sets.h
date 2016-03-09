/************************************************************************************
 *                                     sets.h                                       *
 *                                                                                  *
 * Defines constants, declares helper functions, and includes libraries shared      *
 * amongst set modules.                                                             *
 ************************************************************************************/
/************************************************************************************
 *                             PREPROCESSOR DIRECTIVES                              *
 ************************************************************************************/
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
struct IntNode *prime_sieve(const int upto);
struct IntNode *atkin_sieve(const int upto);
void *sieve_range(void *arg);
static inline bool flp1(const int n, struct SquareTerms *SQ_TERMS);
static inline bool flp2(const int n, struct SquareTerms *SQ_TERMS);
static inline bool flp3(const int n, struct SquareTerms *SQ_TERMS);
/************************************************************************************
 *                           INLINE FUNCTION DEFINITIONS                            *
 ************************************************************************************/
inline void *handle_malloc(const size_t total_bytes)
{
  void *ptr = malloc(total_bytes);
  if (ptr == NULL) {
    fprintf(stderr, FORMAT_ERROR(failed to allocate "%lu" bytes total),
        total_bytes);
    exit(1);
  }

  return ptr;
}

inline void *handle_calloc(const size_t count, const size_t indiv_bytes)
{
  void *ptr = calloc(count, indiv_bytes);

  if (ptr == NULL) {
    fprintf(stderr, FORMAT_ERROR(failed to allocate "%lu" count of "%lu" bytes),
        count, indiv_bytes);
    exit(1);
  }

  return ptr;
}

inline FILE *handle_fopen(const char *filename, const char *mode)
{
  FILE *ptr = fopen(filename, "r");

  if (ptr == NULL) {
    fprintf(stderr, FORMAT_ERROR(failed to open %s in mode "%s"\n\n  reason: %s),
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
  int result = base;

  do {
    result *= base;
    --n;
  } while (n > 1);

  return result;
}

#define min(x, y) (x < y ? x : y)
#define max(x, y) (x < y ? y : x)
#define swp(x, y) { const int a = min(arr[x], arr[y]); \
                    const int b = max(arr[x], arr[y]); \
                    arr[x] = a; arr[y] = b; }

inline void sort_net_3(int *arr)
{
	swp(1, 2); swp(0, 2); swp(0, 1);
}

inline void sort_net_4(int *arr)
{
	swp(0, 1); swp(2, 3); swp(0, 2); swp(1, 3); swp(1, 2);
}

inline void sort_net_5(int *arr)
{
	swp(0, 1); swp(3, 4); swp(2, 3); swp(1, 4);
	swp(0, 3); swp(0, 2); swp(1, 3); swp(1, 2);
}

inline void sort_net_6(int *arr)
{
	swp(1, 2); swp(4, 5); swp(0, 2); swp(3, 5); swp(0, 1); swp(3, 4);
	swp(2, 5); swp(0, 3); swp(1, 4); swp(2, 4); swp(1, 3); swp(2, 3);
}

#undef min
#undef max
#undef swp

/* http://graphics.stanford.edu/~seander/bithacks.html#IntegerLog */
#define __LOG2A(s) ((s &0xffffffff00000000) ? (32 +__LOG2B(s >>32)): (__LOG2B(s)))
#define __LOG2B(s) ((s &0xffff0000)         ? (16 +__LOG2C(s >>16)): (__LOG2C(s)))
#define __LOG2C(s) ((s &0xff00)             ? (8  +__LOG2D(s >>8)) : (__LOG2D(s)))
#define __LOG2D(s) ((s &0xf0)               ? (4  +__LOG2E(s >>4)) : (__LOG2E(s)))
#define __LOG2E(s) ((s &0xc)                ? (2  +__LOG2F(s >>2)) : (__LOG2F(s)))
#define __LOG2F(s) ((s &0x2)                ? (1)                  : (0))

#define LOG2_UINT64 __LOG2A
#define LOG2_UINT32 __LOG2B
#define LOG2_UINT16 __LOG2C
#define LOG2_UINT8  __LOG2D

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


/* The mixing step */
#define mix(a,b,c)				\
{						\
	a -= b;  a -= c;  a ^= (c >> 13);	\
	b -= c;  b -= a;  b ^= (a <<  8);	\
	c -= a;  c -= b;  c ^= (b >> 13);	\
	a -= b;  a -= c;  a ^= (c >> 12);	\
	b -= c;  b -= a;  b ^= (a << 16);	\
	c -= a;  c -= b;  c ^= (b >>  5);	\
	a -= b;  a -= c;  a ^= (c >>  3);	\
	b -= c;  b -= a;  b ^= (a << 10);	\
	c -= a;  c -= b;  c ^= (b >> 15);	\
}

inline size_t jenkins_hash(register unsigned char *k, /* the key */
			   const size_t LENGTH,	      /* length of the key in bytes */
			   size_t init_val)	      /* prev hash or arb val */
{
	register size_t a, b, c; /* the internal state */
	size_t len;		 /* how many key bytes still need mixing */

	/* Set up the internal state */
	a = b = 0x9e3779b9; /* the golden ratio; an arbitrary value */
	c = init_val;       /* variable initialization of internal state */

	/*---------------------------------------- handle most of the key */
	for (len = LENGTH; len >= 12; k += 12, len -= 12) {

		a += (k[0]
		      + ((size_t) k[1] <<  8)
		      + ((size_t) k[2] << 16)
		      + ((size_t) k[3] << 24));

		b += (k[4]
		      + ((size_t) k[5] <<  8)
		      + ((size_t) k[6] << 16)
		      + ((size_t) k[7] << 24));

		c += (k[8]
		      + ((size_t) k[9]  <<  8)
		      + ((size_t) k[10] << 16)
		      + ((size_t) k[11] << 24));

		mix(a, b, c);
	}

	/*------------------------------------- handle the last 11 bytes */
	c += LENGTH;

	switch(len) { /* all the case statements fall through */

		case 11: c += ((size_t) k[10] << 24);
		case 10: c += ((size_t) k[9]  << 16);
		case  9: c += ((size_t) k[8]  <<  8);
		/* the first byte of c is reserved for the length */
		case  8: b += ((size_t) k[7]  << 24);
		case  7: b += ((size_t) k[6]  << 16);
		case  6: b += ((size_t) k[5]  <<  8);
		case  5: b += k[4];
		case  4: a += ((size_t) k[3]  << 24);
		case  3: a += ((size_t) k[2]  << 16);
		case  2: a += ((size_t) k[1]  <<  8);
		case  1: a += k[0];
		/* case 0: nothing left  to add */
	}

	mix(a, b, c);

	/*-------------------------------------------- report the result */
	return c;
}

#undef mix
