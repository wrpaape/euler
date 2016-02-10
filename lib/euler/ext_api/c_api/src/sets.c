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
void mem_error(const size_t num_bytes)
{
  fprintf(stderr, FORMAT_ERROR(failed to allocate "%lu" bytes of memory), num_bytes);
  exit(1);
}


void file_error(const char *filename)
{
  fprintf(stderr, FORMAT_ERROR(failed to open "%s\n\n  reason: %s"), filename, strerror(errno));
  exit(1);
}

