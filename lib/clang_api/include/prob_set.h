/************************************************************************************
 *                                    prob_set.h                                    *
 *                                                                                  *
 * Defines types 'ProbType' and 'SetType' that compose the problem set structure.   *
 ************************************************************************************/
/************************************************************************************
 *                             PREPROCESSOR DIRECTIVES                              *
 ************************************************************************************/
#include "set_2.h"
/************************************************************************************
 *                               INITIAL DECLARATIONS                               *
 ************************************************************************************/
typedef struct ProbNode {
  unsigned int num;    /* problem number identifier */
  struct ProbNode *next_ptr;  /* points to next problem in set */
  char *(*func)(void); /* pointer to problem function */
} ProbType;

typedef struct SetNode {
  unsigned int num;           /* problem set number identifier */
  struct SetNode *next_ptr;   /* points to next problem set */
  struct ProbNode *prob_ptr;  /* points to first problem in set */
} SetType;

static const unsigned int SET_SIZE  = sizeof(SetType);
static const unsigned int PROB_SIZE = sizeof(ProbType);
/************************************************************************************
 *                               FUNCTION PROTOTYPES                                *
 ************************************************************************************/
extern SetType *init_prob_set(void);
extern ProbType *init_prob(const unsigned int prob_num, ProbType *next_prob_ptr, char *(*prob_func)(void));
extern SetType *init_set(const unsigned int set_num, SetType *next_set_ptr, ProbType *head_prob_ptr);



