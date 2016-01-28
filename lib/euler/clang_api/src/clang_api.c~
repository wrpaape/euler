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
  if (argc != 3) {
    arg_error();
  }

  unsigned int prob_num;
  unsigned int set_num;

  parse_num(argv[1], &set_num);
  parse_num(argv[2], &prob_num);

  dispatch(set_num, prob_num);
  
  return 0;
}
/************************************************************************************
 *                               TOP LEVEL FUNCTIONS                                *
 ************************************************************************************/
void parse_num(char *arg, unsigned int *num_ptr)
{
  unsigned int succ_convs;

  succ_convs = sscanf(arg, "%u", num_ptr);

  if (succ_convs != 1) {
    parse_error(arg);
  }
}


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
void arg_error(void)
{
  fprintf(stderr, FORMAT_ERROR(incorrect number of arguments));
  exit(1);
}

void parse_error(const char *arg)
{
  fprintf(stderr, FORMAT_ERROR(failed to parse problem number from argument "%s"), arg);
  exit(1);
}

void missing_set_error(const unsigned int set_num, const unsigned int prob_num)
{
  fprintf(stderr, FORMAT_ERROR(problem set %u housing problem number %u not found), set_num, prob_num);
  exit(1);
}

void missing_prob_error(const unsigned int prob_num, const unsigned int set_num)
{
  fprintf(stderr, FORMAT_ERROR(problem number %u not found in problem set %u), prob_num, set_num);
  exit(1);
}
