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
static const unsigned int *PROB_SET[][] = {
  {2, {12}}
};
/* typedef struct ProbNode { */
/*   unsigned int num;    /1* problem number identifier *1/ */
/*   struct ProbNode *next_ptr;  /1* points to next problem in set *1/ */
/*   char *(*func)(void); /1* pointer to problem function *1/ */
/* } ProbType; */

/* typedef struct SetNode { */
/*   unsigned int num;           /1* problem set number identifier *1/ */
/*   struct SetNode *next_ptr;   /1* points to next problem set *1/ */
/*   struct ProbNode *prob_ptr;  /1* points to first problem in set *1/ */
/* } SetType; */

/* static const unsigned int SET_SIZE  = sizeof(SetType); */
/* static const unsigned int PROB_SIZE = sizeof(ProbType); */

/************************************************************************************
 *                               FUNCTION PROTOTYPES                                *
 ************************************************************************************/
/* extern SetType *init_prob_set(void); */
/* extern ProbType *init_prob(const unsigned int prob_num, ProbType *next_prob_ptr, char *(*prob_func)(void)); */
/* extern SetType *init_set(const unsigned int set_num, SetType *next_set_ptr, ProbType *head_prob_ptr); */
