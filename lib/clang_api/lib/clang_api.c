/************************************************************************************
 *                                   clang_api.c                                    *
 *                                                                                  *
 * Module responsible for communication between Elixir Mix project 'euler' and      *
 * problems solved in c.                                                            *
 ************************************************************************************/
/************************************************************************************
 *                             PREPROCESSOR DIRECTIVES                              *
 ************************************************************************************/
#include "clang_api.h"
/************************************************************************************
 *                               INITIAL DECLARATIONS                               *
 ************************************************************************************/
/************************************************************************************
 *                               FUNCTION PROTOTYPES                                *
 ************************************************************************************/
/************************************************************************************
 *                                  MAIN FUNCTION                                   *
 ************************************************************************************/
int main(int argc, char *argv[])
{
  unsigned int prob_num;
  unsigned int set_num;
  unsigned int succ_convs;

  while (argc > 1) {
    --argc;
    ++argv;

    succ_convs = sscanf(*argv, "%u", &prob_num);

    if (succ_convs == 1) {
      set_num = prob_num / (PROBLEMS_PER_SET + 1) + 1;

      dispatch(set_num, prob_num);
      /* process(prob_num); */
    }
  }
  
  return 0;
}
/************************************************************************************
 *                               TOP LEVEL FUNCTIONS                                *
 ************************************************************************************/
void process(const unsigned int prob_num)
{
  /* SetType *set_ptr; */
  unsigned int set_num;

  /* set_ptr = PROB_SET; */
  set_num = prob_num / (PROBLEMS_PER_SET + 1) + 1;
  
  while (*set_ptr != NULL) {
    if (set_ptr -> num == set_num) {
      solve(set_ptr -> prob_ptr, prob_num);
      return;
    }

    set_ptr = set_ptr -> next_ptr;
  }

  printf("\nERROR: problem set %u housing problem number %u not found!", set_num, prob_num);
}

/************************************************************************************
 *                                HELPER FUNCTIONS                                  *
 ************************************************************************************/
void solve(ProbType *prob_ptr, const unsigned int prob_num)
{
  while (*prob_ptr != NULL) {
    if (prob_ptr -> num == prob_num) {
      puts((prob_ptr -> func)());
      return;
    }
    prob_ptr = prob_ptr -> next_ptr;
  }

  printf("\nERROR: problem number %u not found in problem set %u!", prob_num, set_num);
}

