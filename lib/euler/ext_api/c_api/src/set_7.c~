/************************************************************************************
 *                                     set_7.c                                      *
 *                                                                                  *
 * Module housing solutions to problems 61-70                                       *
 ************************************************************************************/
/************************************************************************************
 *                             PREPROCESSOR DIRECTIVES                              *
 ************************************************************************************/
#include "set_7.h"
/************************************************************************************
 *                               TOP LEVEL FUNCTIONS                                *
 ************************************************************************************/
void problem_67(char *result_buffer)
{
  int **tri_mat;

  tri_mat = load_triangle();

  int i, j, num_cols;

  num_cols = 3;
  for (i = 0; i < NUM_TRI_ROWS; ++i) {
    for (j = 0; j < num_cols; ++j) {
      printf("tri_mat[%i][%i]: %i\n", i, j, tri_mat[i][j]);
    }
      ++num_cols;
  }

}
/************************************************************************************
 *                                HELPER FUNCTIONS                                  *
 ************************************************************************************/
int **load_triangle(void)
{
  FILE *tri_file;
  int **tri_mat;
  int *tri_row;
  size_t row_bytes; /* num bytes required for row pointers of 'tri_mat' */
  size_t col_bytes;
  size_t num_cols;
  size_t row_i;
  size_t col_i;
  size_t ob_col_i;
  
  /* open the triangle data txt file */
  tri_file = fopen(TRI_FILENAME, "r");
  if (tri_file == NULL) {
    file_error(TRI_FILENAME);
  }

  /* allocate memory for row pointers of 'tri_mat' */
  row_bytes = sizeof(int *) * NUM_TRI_ROWS;
  tri_mat   = (int **) malloc(row_bytes);
  if (tri_mat == NULL) {
    mem_error(row_bytes);
  }

  for (row_i = 0, num_cols = 3; row_i < NUM_TRI_ROWS; ++row_i, ++num_cols) {
    col_bytes = sizeof(int) * num_cols;
    tri_row   = (int *) malloc(col_bytes);
    if (tri_row == NULL) {
      mem_error(col_bytes);
    }

   ob_col_i = num_cols - 1;
    /* scan file top to bottom */
    for (col_i = 1; col_i < ob_col_i; ++col_i) {
      fscanf(tri_file, "%i", &tri_row[col_i]);
    }

    /* set first and last cells to negative "out of bounds" sentinel values */
    tri_row[0]        = -1;
    tri_row[ob_col_i] = -1;

    tri_mat[row_i] = tri_row;
  }

  return tri_mat;
}


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
