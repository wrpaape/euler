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

#define UINT_MAX_PRIME 4294967291u
#define FORMAT_ERROR(MSG) "\n\e[31m\e[5mERROR\e[25m\n  " #MSG "\e[0m\n"
/************************************************************************************
 *                               INTIAL DECLARATIONS                                *
 ************************************************************************************/
struct IntNode {
	int val;
	struct IntNode *nxt;
};

struct SieveArg {
	int from;
	int upto;
	int init;
	struct IntNode *head;
	struct IntNode *last;
};
/************************************************************************************
 *                               FUNCTION PROTOTYPES                                *
 ************************************************************************************/
struct IntNode *prime_sieve(int upto);
struct IntNode *atkin_sieve(int upto);
void *sieve_range(void *arg);
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
