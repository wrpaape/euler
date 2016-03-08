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
