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
#include <signal.h>
/************************************************************************************
 *                               INTIAL DECLARATIONS                                *
 ************************************************************************************/
static void (*SET_MAP[7][10])(char *) = {
	/* set 1 (empty) */
	[0 ... 9] = NULL,
	/* set 2 */
	{NULL,	     problem_12, problem_13, problem_14, problem_15,
	 NULL,	     NULL,       NULL,	     problem_19, NULL},
	/* set 3 */
	{NULL,	     problem_22, NULL,	     problem_24, NULL,
	 NULL,	     NULL,	 NULL,	     problem_29, problem_30},
	/* set 4 */
	{NULL,	     NULL,	 problem_33, problem_34, problem_35,
	 problem_36, NULL,	 NULL,	     NULL,	 NULL},
	/* set 5 (empty) */
	[0 ...9] = NULL,
	/* set 6 (empty) */
	[0 ...9] = NULL,
	/* set 7 */
	{NULL,	     NULL,	 NULL,	     NULL,	 NULL,
	 NULL,	     problem_67, NULL,	     NULL,	 NULL}
};
/************************************************************************************
 *                                  MAIN FUNCTION                                   *
 ************************************************************************************/
int main(int argc, char *argv[])
{
	if (argc != 3)
		arg_error();

	int prob_num;
	int set_num;

	char *set_str = argv[1];

	for (set_num = 0; *set_str != '\0'; ++set_str) {
		set_num += *set_str +
	}



	signal(SIGSEGV, SEGVFunction);

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

        case 19:
          time_func(problem_19);
          break;

        default:
          missing_prob_error(prob_num, 2);
      }
      break;
    /* ▲▲▲▲▲▲▲▲▲▲▲▲▲▲▲▲▲▲▲▲▲▲▲▲▲▲

        problem set 3: "set_3.c"
       ▼▼▼▼▼▼▼▼▼▼▼▼▼▼▼▼▼▼▼▼▼▼▼▼▼▼ */
    case 3:
      switch (prob_num) {
        case 22:
          time_func(problem_22);
          break;

        case 24:
          time_func(problem_24);
          break;

        case 29:
          time_func(problem_29);
          break;

        case 30:
          time_func(problem_30);
          break;

        default:
          missing_prob_error(prob_num, 3);
      }
      break;
    /* ▲▲▲▲▲▲▲▲▲▲▲▲▲▲▲▲▲▲▲▲▲▲▲▲▲▲

        problem set 4: "set_4.c"
       ▼▼▼▼▼▼▼▼▼▼▼▼▼▼▼▼▼▼▼▼▼▼▼▼▼▼ */
    case 4:
      switch (prob_num) {
        case 33:
          time_func(problem_33);
          break;

        case 34:
          time_func(problem_34);
          break;

        case 35:
          time_func(problem_35);
          break;

        default:
          missing_prob_error(prob_num, 4);
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

void time_func(void (*func_ptr)(char *))
{
  clock_t time_start;
  clock_t time_stop;
  clock_t time_elapsed;
  char result_buffer[SIZE_RES_BUF];

  time_start = clock();
  func_ptr(result_buffer);
  time_stop  = clock();

  time_elapsed = time_stop - time_start;

  printf("%s\n%lu", result_buffer, time_elapsed);
}

/************************************************************************************
 *                                HELPER FUNCTIONS                                  *
 ************************************************************************************/
void arg_error(void)
{
  fprintf(stderr,
      FORMAT_ERROR(incorrect number of arguments));
  exit(1);
}

void parse_error(const char *arg)
{
  fprintf(stderr,
      FORMAT_ERROR(failed to parse problem number from argument "%s"),
      arg);
  exit(1);
}

void missing_set_error(const unsigned int set_num, const unsigned int prob_num)
{
  fprintf(stderr,
      FORMAT_ERROR(problem set "%u" housing problem number %u not found),
      set_num, prob_num);
  exit(1);
}

void missing_prob_error(const unsigned int prob_num, const unsigned int set_num)
{
  fprintf(stderr,
      FORMAT_ERROR(problem number "%u" not found in problem set "%u"),
      prob_num, set_num);
  exit(1);
}
