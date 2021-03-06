/************************************************************************************
 *                                     c_api.c                                      *
 *                                                                                  *
 * Module responsible for communication between Elixir Mix project 'euler' and      *
 * problems solved in C.                                                            *
 ************************************************************************************/
/************************************************************************************
 *                             PREPROCESSOR DIRECTIVES                              *
 ************************************************************************************/
#include <time.h>
#include <signal.h>
#include "c_api.h"
#include "sets.h"
#include "set_2.h"
#include "set_3.h"
#include "set_4.h"
#include "set_5.h"
#include "set_7.h"

#define SIZE_RES_BUF 100
/************************************************************************************
 *                               INTIAL DECLARATIONS                                *
 ************************************************************************************/
/************************************************************************************
 *                                  MAIN FUNCTION                                   *
 ************************************************************************************/
int main(int argc, char *argv[])
{
	if (argc != 3)
		arg_error();

	int prob_num;
	int set_num;

	parse_num(argv[1], &set_num);
	parse_num(argv[2], &prob_num);

	dispatch(set_num, prob_num);

	return 0;
}
/************************************************************************************
 *                               TOP LEVEL FUNCTIONS                                *
 ************************************************************************************/
void parse_num(char *arg, int *num_ptr)
{
	int succ_convs = sscanf(arg, "%d", num_ptr);

	if (succ_convs != 1)
		parse_error(arg);
}


void dispatch(const int set_num, const int prob_num)
{
	switch (set_num) {
		/*  problem set 2: "set_2.c"
		▼▼▼▼▼▼▼▼▼▼▼▼▼▼▼▼▼▼▼▼▼▼▼▼▼▼ */
		case 2:
			switch (prob_num) {
				case 12: time_func(problem_12); break;
				case 13: time_func(problem_13); break;
				case 14: time_func(problem_14); break;
				case 15: time_func(problem_15); break;
				case 19: time_func(problem_19); break;
				default: missing_prob_error(prob_num, 2);
			}
			break;
		/* ▲▲▲▲▲▲▲▲▲▲▲▲▲▲▲▲▲▲▲▲▲▲▲▲▲▲

		problem set 3: "set_3.c"
		▼▼▼▼▼▼▼▼▼▼▼▼▼▼▼▼▼▼▼▼▼▼▼▼▼▼ */
		case 3:
			switch (prob_num) {
				case 22: time_func(problem_22); break;
				case 24: time_func(problem_24); break;
				case 29: time_func(problem_29); break;
				case 30: time_func(problem_30); break;
				default: missing_prob_error(prob_num, 3);
			}
			break;
		/* ▲▲▲▲▲▲▲▲▲▲▲▲▲▲▲▲▲▲▲▲▲▲▲▲▲▲

		problem set 4: "set_4.c"
		▼▼▼▼▼▼▼▼▼▼▼▼▼▼▼▼▼▼▼▼▼▼▼▼▼▼ */
		case 4:
			switch (prob_num) {
				case 33: time_func(problem_33); break;
				case 34: time_func(problem_34); break;
				case 35: time_func(problem_35); break;
				case 36: time_func(problem_36); break;
				case 37: time_func(problem_37); break;
				default: missing_prob_error(prob_num, 4);
			}
			break;
		/* ▲▲▲▲▲▲▲▲▲▲▲▲▲▲▲▲▲▲▲▲▲▲▲▲▲▲

		problem set 5: "set_5.c"
		▼▼▼▼▼▼▼▼▼▼▼▼▼▼▼▼▼▼▼▼▼▼▼▼▼▼ */
		case 5:
			switch (prob_num) {
				case 41: time_func(problem_41); break;
				default: missing_prob_error(prob_num, 5);
			}
			break;
		/* ▲▲▲▲▲▲▲▲▲▲▲▲▲▲▲▲▲▲▲▲▲▲▲▲▲▲

		problem set 7: "set_7.c"
		▼▼▼▼▼▼▼▼▼▼▼▼▼▼▼▼▼▼▼▼▼▼▼▼▼▼ */
		case 7:
			switch (prob_num) {
				case 67: time_func(problem_67); break;
				default: missing_prob_error(prob_num, 7);
			}
			break;
		/* ▲▲▲▲▲▲▲▲▲▲▲▲▲▲▲▲▲▲▲▲▲▲▲▲▲▲ */

		default: missing_set_error(set_num, prob_num);
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

void missing_set_error(const int set_num, const int prob_num)
{
	fprintf(stderr,
		FORMAT_ERROR(problem set "%d" housing problem number %d not
			     found),
		set_num,
		prob_num);
	exit(1);
}

void missing_prob_error(const int prob_num, const int set_num)
{
	fprintf(stderr,
		FORMAT_ERROR(problem number "%d" not found in problem set "%d"),
		prob_num,
		set_num);
	exit(1);
}
