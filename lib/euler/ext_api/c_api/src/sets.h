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
#include <string.h>
#include <memory.h>
#include <stdbool.h>
#include <math.h>
#include <errno.h>
#include <limits.h>

#define FORMAT_ERROR(MSG) "\n\e[31m\e[5mERROR\e[25m\n  " #MSG "\e[0m\n"
/************************************************************************************
 *                               FUNCTION PROTOTYPES                                *
 ************************************************************************************/
/************************************************************************************
 *                           INLINE FUNCTION DEFINITIONS                            *
 ************************************************************************************/
static inline void handle_malloc(void *ptr, const size_t total_bytes)
{
  ptr = malloc(total_bytes);
  if (ptr == NULL) {
    fprintf(stderr, FORMAT_ERROR(failed to allocate "%lu" bytes total),
        total_bytes);
    exit(1);
  }
}

static inline void handle_calloc(void *ptr, const size_t count, const size_t indiv_bytes)
{
  ptr = calloc(count, indiv_bytes);
  if (ptr == NULL) {
    fprintf(stderr, FORMAT_ERROR(failed to allocate "%lu" count of "%lu" bytes),
        count, indiv_bytes);
    exit(1);
  }
}

static inline void handle_fopen(FILE *ptr, const char *filename, const char *mode)
{
  ptr = fopen(filename, "r");
  if (ptr == NULL) {
    fprintf(stderr, FORMAT_ERROR(failed to open "%s in mode %s\n\n  reason: %s"),
        filename, mode, strerror(errno));
    exit(1);
  }
}
