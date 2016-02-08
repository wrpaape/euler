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
  struct BranchNode **prev_ptr;
  struct BranchNode **head_ptr;
  struct BranchNode *prev_node;
  struct BranchNode *next_node;
  struct BranchNode *fork_node;

  tri_mat  = load_triangle();
  head_ptr = init_branches(tri_mat[NUM_TRI_ROWS - 1]);

  /* starting from the second-to-last-row and working up... */
  for (row_i = NUM_TRI_ROWS - 2, num_cols = row_i + 1; row_i > -1; --row_i, --num_cols) {
    tri_row = tri_mat[row_i];

    /* fork branch nodes, from second node to second-to-last node */
    prev_node = *head_ptr;
    next_node = prev_node -> next_node;

    while ((next_node -> next_node) != NULL) {
      fork_node = (struct BranchNode *) malloc(BRANCH_NODE_BYTES);
      if (fork_node == NULL) {
        mem_error(BRANCH_NODE_BYTES);
      }
      memcpy(fork_node, next_node, BRANCH_NODE_BYTES);

      /* insert 'fork_node' between 'prev_node' and 'next_node' */
      fork_node -> next_node = next_node;
      prev_node -> next_node = fork_node;

      prev_node = next_node;              /* set next 'prev_node' to last node */
      next_node = prev_node -> next_node; /* set next 'next_node' to next in list */
    }

    /* terminate list at last 'fork_node' */
    /* free(fork_node -> next_node); */
    /* fork_node -> next_node = NULL; */
      puts("last nodes:");
      printf("  prev_node -> sum: %d\n", prev_node -> sum);
      printf("  fork_node -> sum: %d\n", fork_node -> sum);
      printf("  next_node -> sum: %d\n", next_node -> sum);
      fflush(stdout);

    puts("forked branches");
    fflush(stdout);

    /* compare overlapping nodes: the branch with the lesser sum is discarded */
    prev_ptr = head_ptr;

    for (col_i = 0; col_i < num_cols; ++col_i) {
      prev_node = (*prev_ptr) -> next_node;
      next_node = prev_node   -> next_node;


      printf("tri_row[%d]: %d\n", col_i, tri_row[col_i]);
      printf("  prev_node -> sum: %d\n", prev_node -> sum);
      printf("  next_node -> sum: %d\n", next_node -> sum);
      fflush(stdout);
      if ((prev_node -> sum) > (next_node -> sum)) {
        prev_node -> sum += tri_row[col_i];            /* increment branch sum */
        prev_node -> next_node = next_node -> next_node; /* bridge pointer gap */
        prev_ptr = &prev_node;                         /* set next 'prev_ptr' to better branch node */
        free(next_node);                               /* delete lesser node */
      } else {
        next_node -> sum += tri_row[col_i];            /* increment branch sum */
        *prev_ptr = next_node;                         /* bridge pointer gap */
        prev_ptr  = &next_node;                        /* set next 'prev_ptr' to better branch node */
        free(prev_node);                               /* delete lesser node */
      }
    }

    next_node = *head_ptr;
    while(next_node != NULL) {
      printf("sum: %d\n", next_node -> sum);
      next_node = next_node -> next_node;
    }
    return;
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

    /* scan file top to bottom */
    for (col_i = 0; col_i < num_cols; ++col_i) {
      fscanf(tri_file, "%2d", &tri_row[col_i]);
    }

    tri_mat[row_i] = tri_row;
  }

  return tri_mat;
}

struct BranchNode **init_branches(int *base_row)
{
  int col_i;
  struct BranchNode **head_ptr;
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
    prev_node -> sum       = base_row[col_i];
    prev_node -> next_node = next_node;


    next_node = prev_node; /* set new node as list head */
  }

  head_ptr = &next_node;

  return head_ptr; /* return head node */
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
