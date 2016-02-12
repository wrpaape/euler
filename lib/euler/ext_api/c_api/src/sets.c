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
 *                               TOP LEVEL FUNCTIONS                                *
 ************************************************************************************/
static inline void
handle_malloc(void *ptr, const size_t total_bytes)
{
  ptr = malloc(total_bytes);
  if (ptr == NULL) {
    fprintf(stderr, FORMAT_ERROR(failed to allocate "%lu" bytes total),
        total_bytes);
    exit(1);
  }
}

static inline void
handle_calloc(void *ptr, const size_t count, const size_t indiv_bytes)
{
  ptr = calloc(count, indiv_bytes);
  if (ptr == NULL) {
    fprintf(stderr, FORMAT_ERROR(failed to allocate "%lu" count of "%lu" bytes),
        count, indiv_bytes);
    exit(1);
  }
}

static inline void
handle_fopen(FILE *ptr, const char *filename, const char *mode)
{
  ptr = fopen(filename, "r");
  if (ptr == NULL) {
    fprintf(stderr, FORMAT_ERROR(failed to open "%s in mode %s\n\n  reason: %s"),
        filename, mode, strerror(errno));
    exit(1);
  }
}
