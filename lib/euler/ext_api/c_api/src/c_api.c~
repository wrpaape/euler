/************************************************************************************
 *                                     c_api.c                                      *
 *                                                                                  *
 * Module responsible for communication between Elixir Mix project 'euler' and      *
 * problems solved in C.                                                            *
 ************************************************************************************/
/************************************************************************************
 *                             PREPROCESSOR DIRECTIVES                              *
 ************************************************************************************/
#include "c_api.h"
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
          time_func(problem_12);
          break;

        case 13:
          time_func(problem_13);
          break;

        case 14:
          time_func(problem_14);
          break;

        case 15:
          time_func(problem_15);
          break;

        default:
          missing_prob_error(prob_num, 2);
      }
      break;
    /* ▲▲▲▲▲▲▲▲▲▲▲▲▲▲▲▲▲▲▲▲▲▲▲▲▲▲

        problem set 7: "set_7.c"
       ▼▼▼▼▼▼▼▼▼▼▼▼▼▼▼▼▼▼▼▼▼▼▼▼▼▼ */
    case 7:
      switch (prob_num) {
        case 67:
          time_func(problem_67);
          break;

        default:
          missing_prob_error(prob_num, 7);
      }
      break;
    /* ▲▲▲▲▲▲▲▲▲▲▲▲▲▲▲▲▲▲▲▲▲▲▲▲▲▲ */

    default:
      missing_set_error(set_num, prob_num);
  }
}

void time_func(void (*func_ptr)(void))
{
  clock_t time_start;
  clock_t time_elapsed;

  time_start = clock();
  func_ptr();
  time_elapsed = clock() - time_start;

  printf("\n%lu", time_elapsed);
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
