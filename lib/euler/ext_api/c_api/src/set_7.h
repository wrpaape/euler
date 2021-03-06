/************************************************************************************
 *                                    set_7.h                                       *
 *                                                                                  *
 * Houses preprocessor directives, constant declarations, and function prototypes   *
 * needed in module 'set_7.c'.                                                      *
 ************************************************************************************/
/************************************************************************************
 *                             PREPROCESSOR DIRECTIVES                              *
 ************************************************************************************/
#define NUM_TRI_ROWS 100
#define TRI_FILENAME "data/set_7-prob_67-data.txt"
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
