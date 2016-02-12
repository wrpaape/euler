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
const size_t B_NODE_PTR_BYTES  = sizeof(struct BranchNode *);
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
  struct BranchNode **prev_ptr;
  struct BranchNode **head_ptr;
  struct BranchNode *prev_node;
  struct BranchNode *next_node;
  struct BranchNode *fork_node;

  tri_mat  = load_triangle(); /* load in triangle numbers from txt file */

  handle_malloc(head_ptr, sizeof(struct BranchNode *));
  /* head_ptr = (struct BranchNode **) malloc(B_NODE_PTR_BYTES); */
  /* if (head_ptr == NULL) { */
  /*   mem_error(B_NODE_PTR_BYTES); */
  /* } */

  *head_ptr = init_branches(tri_mat[NUM_TRI_ROWS - 1]); /* set init sums = last row */

  /* starting from the second-to-last-row and working upward... */
  for (row_i = NUM_TRI_ROWS - 2, num_cols = row_i + 1; row_i > -1; --row_i, --num_cols) {
    tri_row   = tri_mat[row_i];
    next_node = *head_ptr;
    for (col_i = 1; col_i < num_cols; ++col_i) {
      prev_node = next_node;              /* set 'prev_node' to last 'next_node' */
      next_node = prev_node -> next_node; /* set 'next_node' to next in list */

      handle_malloc(fork_node, sizeof(struct BranchNode));
      /* fork_node = (struct BranchNode *) malloc(BRANCH_NODE_BYTES); */
      /* if (fork_node == NULL) { */
      /*   mem_error(BRANCH_NODE_BYTES); */
      /* } */
      memcpy(fork_node, next_node, BRANCH_NODE_BYTES);

      /* insert 'fork_node' between 'prev_node' and 'next_node' */
      fork_node -> next_node = next_node;
      prev_node -> next_node = fork_node;
    }


    /* merge forked branches, deleting those nodes with lesser sums */
    prev_ptr = head_ptr;
    for (col_i = 0; col_i < num_cols; ++col_i) {
      prev_node = (*prev_ptr);
      next_node = prev_node -> next_node;

      if ((prev_node -> sum) > (next_node -> sum)) {
        prev_node -> sum      += tri_row[col_i];            /* increment branch sum */
        prev_node -> next_node = next_node -> next_node;    /* bridge pointer gap */
        prev_ptr               = &(prev_node -> next_node); /* set next 'prev_ptr' */
        free(next_node);                                    /* delete lesser node */
      } else {
        next_node -> sum      += tri_row[col_i];            /* increment branch sum */
        *prev_ptr              = next_node;                 /* bridge pointer gap */
        prev_ptr               = &(next_node -> next_node); /* set next 'prev_ptr' */
        free(prev_node);                                    /* delete lesser node */
      }
    }
  }
  /* the last remaining branch node should hold the greatest possible traversal sum */
  sprintf(result_buffer, "%d", (*head_ptr) -> sum);
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
  
  /* open the triangle data txt file for reading */
  handle_fopen(tri_file, TRI_FILENAME, "r");
  /* tri_file = fopen(TRI_FILENAME, "r"); */
  /* if (tri_file == NULL) { */
  /*   file_error(TRI_FILENAME); */
  /* } */

  /* allocate memory for row pointers of 'tri_mat' */
  handle_malloc(tri_mat, sizeof(int *) * NUM_TRI_ROWS);
  /* row_bytes = sizeof(int *) * NUM_TRI_ROWS; */
  /* tri_mat   = (int **) malloc(row_bytes); */
  /* if (tri_mat == NULL) { */
  /*   mem_error(row_bytes); */
  /* } */

  /* for (row_i = 0, num_cols = 3; row_i < NUM_TRI_ROWS; ++row_i, ++num_cols) { */
  for (row_i = 0, num_cols = 1; row_i < NUM_TRI_ROWS; ++row_i, ++num_cols) {
    handle_malloc(tri_row, sizeof(int) * num_cols);
    /* col_bytes = sizeof(int) * num_cols; */
    /* tri_row   = (int *) malloc(col_bytes); */
    /* if (tri_row == NULL) { */
    /*   mem_error(col_bytes); */
    /* } */

    /* scan file top to bottom */
    for (col_i = 0; col_i < num_cols; ++col_i) {
      fscanf(tri_file, "%2d", &tri_row[col_i]);
    }

    tri_mat[row_i] = tri_row;
  }

  fclose(tri_file);

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
    handle_malloc(prev_node, sizeof(struct BranchNode));
    /* prev_node = (struct BranchNode *) malloc(BRANCH_NODE_BYTES); */
    /* if (prev_node == NULL) { */
    /*   mem_error(BRANCH_NODE_BYTES); */
    /* } */

    /* initialize new node in list */
    prev_node -> sum       = base_row[col_i];
    prev_node -> next_node = next_node;

    next_node = prev_node; /* set new node as list head */
  }

  return next_node;
}
