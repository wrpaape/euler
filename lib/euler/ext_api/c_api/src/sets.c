/************************************************************************************
 *                                     errors.c                                     *
 *                                                                                  *
 * Defines constants, declares helper functions, and includes libraries for         *
 * handling and reporting errors.                                                   *
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

