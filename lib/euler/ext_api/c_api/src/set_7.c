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
 *                               INTIAL DECLARATIONS                                *
 ************************************************************************************/
const size_t BRANCH_NODE_BYTES = sizeof(struct BranchNode);
/************************************************************************************
 *                               TOP LEVEL FUNCTIONS                                *
 ************************************************************************************/
void problem_67(char *result_buffer)
{
  int row_i;
  int col_i;
  int num_cols;
  int **tri_mat;
  int *tri_row;
  struct BranchNode *head_node;
  struct BranchNode *prev_node;
  struct BranchNode *next_node;
  struct BranchNode *fork_node;

  tri_mat   = load_triangle();
  head_node = init_branches(tri_mat[NUM_TRI_ROWS - 1]);

  /* starting from the second-to-last-row and working up... */
  for (row_i = NUM_TRI_ROWS - 2, num_cols = row_i + 1; row_i > -1; --row_i, --num_cols) {
    tri_row = tri_mat[row_i];

    /* fork branch nodes, from second node to second-to-last node */
    prev_node = head_node;
    next_node = prev_node -> next_ptr;

    while (next_node != NULL) {
      fork_node = (struct BranchNode *) malloc(BRANCH_NODE_BYTES);
      if (fork_node == NULL) {
        mem_error(BRANCH_NODE_BYTES);
      }
      memcpy(fork_node, next_node, BRANCH_NODE_BYTES);

      /* --(fork_node -> col_i); /1* decrement column index of new node *1/ */

      /* insert 'fork_node' between 'prev_node' and 'next_node' */
      prev_node -> next_ptr = fork_node;
      fork_node -> next_ptr = next_node;

      prev_node = next_node;             /* set 'prev_node' to last node */
      next_node = next_node -> next_ptr; /* set 'next_node' to next in list */
    }

    /* terminate list at last 'fork_node' */
    free(fork_node -> next_ptr);
    fork_node -> next_ptr = NULL;

    /* compare overlapping nodes: the branch with the lesser sum is discarded */
    prev_node = head_node;
    next_node = prev_node -> next_ptr;

    while (next_node != NULL) {
      if ((prev_node -> sum) > (next_node -> sum)) {

      }

      /* insert 'fork_node' between 'prev_node' and 'next_node' */
      prev_node -> next_ptr = fork_node;
      fork_node -> next_ptr = next_node;

      prev_node = next_node;             /* set 'prev_node' to last node */
      next_node = next_node -> next_ptr; /* set 'next_node' to next in list */
    }


    /* next_node = head_node; */
    /* while(next_node != NULL) { */
    /*   printf("sum: %d, col_i: %d\n", next_node -> sum, next_node -> col_i); */
    /*   next_node = next_node -> next_ptr; */
    /* } */
    /* return; */
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
  /* int ob_col_i; */
  
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

  /* for (row_i = 0, num_cols = 3; row_i < NUM_TRI_ROWS; ++row_i, ++num_cols) { */
  for (row_i = 0, num_cols = 1; row_i < NUM_TRI_ROWS; ++row_i, ++num_cols) {
    col_bytes = sizeof(int) * num_cols;
    tri_row   = (int *) malloc(col_bytes);
    if (tri_row == NULL) {
      mem_error(col_bytes);
    }

   /* ob_col_i = num_cols - 1; */
    /* for (col_i = 1; col_i < ob_col_i; ++col_i) { */
    /* scan file top to bottom */
    for (col_i = 0; col_i < num_cols; ++col_i) {
      fscanf(tri_file, "%2d", &tri_row[col_i]);
    }

    /* set first and last cells to negative "out of bounds" sentinel values */
    /* tri_row[0]        = -1; */
    /* tri_row[ob_col_i] = -1; */

    tri_mat[row_i] = tri_row;
  }

  return tri_mat;
}

struct BranchNode *init_branches(int *base_row)
{
  int col_i;
  struct BranchNode *next_node;
  struct BranchNode *prev_node;

  next_node = NULL;

  /* working backwards from the last index of the 'base_row' of 'tri_mat' */
  for (col_i = NUM_TRI_ROWS - 1; col_i > -1; --col_i) {
    /* allocate memory for new branch node */
    prev_node = (struct BranchNode *) malloc(BRANCH_NODE_BYTES);
    if (prev_node == NULL) {
      mem_error(BRANCH_NODE_BYTES);
    }

    /* initialize new node in list */
    /* prev_node -> col_i    = col_i; */
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
