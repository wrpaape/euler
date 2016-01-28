/************************************************************************************
 *                                    prob_set.c                                    *
 *                                                                                  *
 * Initializes problem set body.                                                    *
 ************************************************************************************/
/************************************************************************************
 *                             PREPROCESSOR DIRECTIVES                              *
 ************************************************************************************/
#include "prob_set.h"
/************************************************************************************
 *                               INITIAL DECLARATIONS                               *
 ************************************************************************************/
/************************************************************************************
 *                               FUNCTION PROTOTYPES                                *
 ************************************************************************************/
SetType *init_prob_set(void)
{

  ProbType *head_prob_ptr = init_prob(12, NULL, &problem_12);

  SetType *head_set_ptr = init_set(2, NULL, head_prob_ptr);


  return head_set_ptr;
}


ProbType *init_prob(const unsigned int prob_num, ProbType *next_prob_ptr, char *(*prob_func)(void))
{
  ProbType *prob_ptr;

  prob_ptr = malloc(PROB_SIZE);
  if (prob_ptr != NULL) {
    prob_ptr -> num = prob_num;
    prob_ptr -> next_ptr = next_prob_ptr;
    prob_ptr -> func = prob_func;
  }

  return prob_ptr;
}

SetType *init_set(const unsigned int set_num, SetType *next_set_ptr, ProbType *head_prob_ptr)
{
  SetType *set_ptr;

  set_ptr = malloc(SET_SIZE);

  if (set_ptr != NULL) {
    set_ptr -> num = set_num;
    set_ptr -> next_ptr  = next_set_ptr;
    set_ptr -> prob_ptr = head_prob_ptr;
  }

  return set_ptr;
}
