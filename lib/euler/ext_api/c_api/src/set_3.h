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
  long long score;
  struct NameNode *next_ptr;
};

struct NameBucket {
  struct NameNode *head_ptr;
};

/* struct LoadInfo { */
/*   struct NameBucket *name_tups; */
/*   size_t num_names; */
/* }; */
/************************************************************************************
 *                               FUNCTION PROTOTYPES                                *
 ************************************************************************************/
void problem_22(char *result_buffer);
struct NameBucket *load_buckets(void);
