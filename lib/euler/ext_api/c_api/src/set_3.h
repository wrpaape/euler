/************************************************************************************
 *                                    set_3.h                                       *
 *                                                                                  *
 * Houses preprocessor directives, constant declarations, and function prototypes   *
 * needed in module 'set_3.c'.                                                      *
 ************************************************************************************/
/************************************************************************************
 *                             PREPROCESSOR DIRECTIVES                              *
 ************************************************************************************/
#define SAFE_LEN_NAMES 6000
#define SIZE_NAME_BUF 16
#define NAMES_FILENAME "data/set_3-prob_22-data.txt"
/************************************************************************************
 *                               INTIAL DECLARATIONS                                *
 ************************************************************************************/
struct NameNode {
  char name[SIZE_NAME_BUF];
  int score;
  struct NameNode *next_ptr;
};

struct SortParams {
  struct NameNode **interval;
  int span;
};

struct DigitNode {
  char value;
  struct DigitNode *next_ptr;
}
/************************************************************************************
 *                               FUNCTION PROTOTYPES                                *
 ************************************************************************************/
void problem_22(char *result_buffer);
void problem_23(char *result_buffer);
struct NameNode **load_buckets(void);
void *sort_buckets(void *arg_ptr);
void do_permute(int num_rem_digs,
                struct DigitNode *digs_head_ptr,
                char *dig_buff_ptr,
                long *perm_count_ptr);
