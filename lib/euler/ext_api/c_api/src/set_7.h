/************************************************************************************
 *                                    set_7.h                                       *
 *                                                                                  *
 * Houses preprocessor directives, constant declarations, and function prototypes   *
 * needed in module 'set_7.c'.                                                      *
 ************************************************************************************/
/************************************************************************************
 *                             PREPROCESSOR DIRECTIVES                              *
 ************************************************************************************/
#include <stdlib.h>
#include "errors.h"
#include "sets.h"

#define NUM_TRI_ROWS 100
#define TRI_FILENAME "set_7-prob_67-data.txt"
/************************************************************************************
 *                               INTIAL DECLARATIONS                                *
 ************************************************************************************/
struct BranchNode {
  int sum;
  struct BranchNode *next_node;
};
/************************************************************************************
 *                               FUNCTION PROTOTYPES                                *
 ************************************************************************************/
void problem_67(char *result_buffer);
int **load_triangle(void);
struct BranchNode *init_branches(int *base_row);
void mem_error(const size_t num_bytes);
void file_error(const char *filename);
