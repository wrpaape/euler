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

struct SortArgs {
  struct NameNode **bucket_interval;
  int bucket_span;
};
/************************************************************************************
 *                               FUNCTION PROTOTYPES                                *
 ************************************************************************************/
void problem_22(char *result_buffer);
struct NameNode **load_buckets(void);
void sort_buckets(struct SortArgs *args);
