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
 *                                  MAIN FUNCTION                                   *
 ************************************************************************************/
int main(int argc, char *argv[])
{
  unsigned int prob_num;
  unsigned int succ_convs;
  unsigned int set_num;

  while (argc > 1) {
    --argc;
    ++argv;

    succ_convs = sscanf(*argv, "%u", &prob_num);

    if (succ_convs != 1) {
      parse_error(*argv);
      continue;
    }

    set_num = prob_num / (PROBLEMS_PER_SET + 1) + 1;

    dispatch(set_num, prob_num);
  }
  
  return 0;
}
/************************************************************************************
 *                               TOP LEVEL FUNCTIONS                                *
 ************************************************************************************/
void dispatch(const unsigned int set_num, const unsigned int prob_num)
{
  switch (set_num) {
    /*  problem set 2: "set_2.c"
       ▼▼▼▼▼▼▼▼▼▼▼▼▼▼▼▼▼▼▼▼▼▼▼▼▼▼ */
    case 2:
      switch (prob_num) {
        case 12:
          problem_12();
          break;

        default:
          missing_prob_error(prob_num, 12);
      }
      break;
    /* ▲▲▲▲▲▲▲▲▲▲▲▲▲▲▲▲▲▲▲▲▲▲▲▲▲▲ */

    default:
      missing_set_error(set_num, prob_num);
  }
}

/************************************************************************************
 *                                HELPER FUNCTIONS                                  *
 ************************************************************************************/
void parse_error(const char *arg)
{
  fprintf(stderr, FORMAT_ERROR("failed to parse problem number from argument \"%s\""), arg);
}

void missing_set_error(const unsigned int set_num, const unsigned int prob_num)
{
  fprintf(stderr, FORMAT_ERROR("problem set %u housing problem number %u not found"), set_num, prob_num);
}

void missing_prob_error(const unsigned int prob_num, const unsigned int set_num)
{
  fprintf(stderr, FORMAT_ERROR("problem number %u not found in problem set %u"), prob_num, set_num);
}
