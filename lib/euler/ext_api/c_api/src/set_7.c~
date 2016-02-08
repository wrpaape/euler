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
  int row_i;
  int **tri_mat;
  struct BranchNode *branches;

  tri_mat  = load_triangle();
  branches = init_branches(tri_mat[NUM_TRI_ROWS - 1]);

  while (branches -> next_ptr != NULL) {
    printf("col_i: %i, sum: %i\n", branches -> col_i, branches -> sum);
    branches = branches -> next_ptr;
  }
}
/************************************************************************************
 *                                HELPER FUNCTIONS                                  *
 ************************************************************************************/
int **load_triangle(void)
{
  FILE *tri_file;
  size_t row_bytes; /* num bytes required for row pointers of 'tri_mat' */
  size_t col_bytes;
  int **tri_mat;
  int *tri_row;
  int num_cols;
  int row_i;
  int col_i;
  int ob_col_i;
  
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

struct BranchNode *init_branches(int *base_row)
{
  struct BranchNode *next_node;
  struct BranchNode *prev_node;

  const size_t node_bytes = sizeof(struct BranchNode);
  next_node               = NULL;

  /* working backwards from the last IN BOUNDS index of the 'base_row' of 'tri_mat' */
  for (int col_i = NUM_TRI_ROWS; col_i > 0; --col_i) {
    /* allocate memory for new branch node */
    prev_node = (struct BranchNode *) malloc(node_bytes);
    if (prev_node == NULL) {
      mem_error(node_bytes);
    }

    /* initialize new node in list */
    prev_node -> col_i    = col_i;
    prev_node -> sum      = base_row[col_i];
    prev_node -> next_ptr = next_node;

    next_node = prev_node; /* set new node as list head */
  }

  return next_node; /* return head of list */
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
