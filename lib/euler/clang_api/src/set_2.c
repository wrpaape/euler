/************************************************************************************
 *                                     set_2.c                                      *
 *                                                                                  *
 * Module housing solutions to problems 11-20                                       *
 ************************************************************************************/
/************************************************************************************
 *                             PREPROCESSOR DIRECTIVES                              *
 ************************************************************************************/
#include "sets.h"
#include "set_2.h"
/************************************************************************************
 *                               INTIAL DECLARATIONS                                *
 ************************************************************************************/
const unsigned int DIG_MAT[100][50] = DIGITS;
/************************************************************************************
 *                               TOP LEVEL FUNCTIONS                                *
 ************************************************************************************/
/************************************************************************************
 *                                  - problem_12 -                                  *
 * 																																									*
 * The sequence of triangle numbers is generated by adding the natural numbers. So	*
 * the 7th triangle number would be 1 + 2 + 3 + 4 + 5 + 6 + 7 = 28. The first ten		*
 * terms would be:																																	*
 * 																																									*
 * 1, 3, 6, 10, 15, 21, 28, 36, 45, 55, ...																					*
 * 																																									*
 * Let us list the factors of the first seven triangle numbers:											*
 * 																																									*
 *  1: 1																																						*
 *  3: 1,3																																					*
 *  6: 1,2,3,6																																			*
 * 10: 1,2,5,10																																			*
 * 15: 1,3,5,15																																			*
 * 21: 1,3,7,21																																			*
 * 28: 1,2,4,7,14,28																																*
 * We can see that 28 is the first triangle number to have over five divisors.			*
 * 																																									*
 * What is the value of the first triangle number to have over five hundred					*
 * divisors? 																																				*
 ************************************************************************************/
void problem_12(void)
{
	const unsigned int N = 10; /* max number of iterations */

	unsigned int n;						 /* counter for calculating triangule number 'tri' */
	unsigned int tri;					 /* triangle number corresponding to counter 'n' */
	unsigned int num_divs;		 /* number of divisors for 'tri' */
	unsigned int smaller_div;  /* smaller of two divisors whose product is 'tri' */
	unsigned int min_big_div;  /* current smallest larger divisor */

  n   = 0;      /* zeroth counter */
  tri = 0;      /* zeroth triangle number */
  num_divs = 1; /* corresponds to 0 */ 

  while (num_divs < 500) {
    ++n;
    tri = tri + n;         /* calculate next triangle number */
		num_divs = 2;          /* corresponds to 1 and 'tri' */
    smaller_div = 1;
		min_big_div = tri;

    while (smaller_div <= min_big_div) {
      ++smaller_div;

      if (tri % smaller_div == 0) {
        min_big_div = tri / smaller_div;
        num_divs += 2;
      }
    }
	}

	printf("%u", tri);
}



/************************************************************************************
 *                                  - problem_13 -                                  *
 * 																																									*
 * Work out the first ten digits of the sum of the following one-hundred 50-digit   *
 * numbers. (DIG_MAT)                                                               *
 ************************************************************************************/
void problem_13(void)
{
  unsigned int result[52]; /* digits of total sum (extra digits for overflow) */
  unsigned int off_num;    /* number position counter (x-coordinate) */
  unsigned int off_dig;    /* digit place counter (y-coordinate) */
  unsigned int off_ten;    /* index of tenth sig digit in final result */

  memset(result, 0, sizeof(result)); /* initialize all digits to 0 */

  for (off_dig = 51; off_dig > 1; --off_dig) {
    for (off_num = 0; off_num < 100; ++off_num) {
      result[off_dig] += DIG_MAT[off_num][off_dig - 2]; /* sum column of digits */
    }

    result[off_dig - 1] += (result[off_dig] / 10);      /* shift overflow left */
    result[off_dig]     %= 10;                          /* set digit to remainder */
  }

  while (result[off_dig] > 9) {
    result[off_dig - 1] += (result[off_dig] / 10);      /* shift overflow left */
    result[off_dig]     %= 10;                          /* set digit to remainder */
    --off_dig;
  }

  off_ten = off_dig + 10;

  while (off_dig < off_ten) {
    printf("%u", result[off_dig]); /* print first ten significant digits */
    ++off_dig;
  }
}


/************************************************************************************
 *                                  - problem_14 -                                  *
 * 																																									*
 * The following iterative sequence is defined for the set of positive integers:    *
 *                                                                                  *
 * n → n/2 (n is even)                                                              *
 * n → 3n + 1 (n is odd)                                                            *
 *                                                                                  *
 * Using the rule above and starting with 13, we generate the following sequence:   *
 *                                                                                  *
 * 13 → 40 → 20 → 10 → 5 → 16 → 8 → 4 → 2 → 1                                       *
 * It can be seen that this sequence (starting at 13 and finishing at 1) contains   *
 * 10 terms. Although it has not been proved yet (Collatz Problem), it is thought   *
 * that all starting numbers finish at 1.                                           *
 *                                                                                  *
 * Which starting number, under one million, produces the longest chain?            *
 *                                                                                  *
 * NOTE: Once the chain starts the terms are allowed to go above one million.       *
 ************************************************************************************/
void problem_14(void)
{
  unsigned int n0;           /* number at start of sequence defined above */
  unsigned int n;            /* current number in sequence defined above */
  unsigned int count;        /* length of current sequence */
  unsigned int max_count;    /* length of longest sequence */
  unsigned int n0_max_count; /* starting number that produces 'max_count' */

  n0           = 1e6;
  max_count    = 0;
  n0_max_count = 0;

  while (n0 > 1) {
    --n0;       /* decrement n0 */
    n     = n0; /* initialize n */
    count = 2;  /* including initial and final values */

    while (n > 1) {
      if ((n & 1) == 1) {  /* n is odd */
        n = 3 * n + 1;
      } else {             /* n is even */
        n = n / 2;
      }
      ++count;
    }

    if (count > max_count) {
      max_count    = count;
      n0_max_count = n0;
    }
  }

  printf("%u", n0_max_count);
}


/************************************************************************************
 *                                  - problem_15 -                                  *
 * 																																									*
 * Starting in the top left corner of a 2×2 grid, and only being able to move to    *
 * the right and down, there are exactly 6 routes to the bottom right corner.       *
 * 																																									*
 * How many such routes are there through a 20×20 grid?                             *
 ************************************************************************************/
void problem_15(void)
{
  long long unsigned int left_num_routes;  /* running total of viable routes */
  long long unsigned int right_num_routes; /* running total of viable routes */

  left_num_routes  = 0;
  right_num_routes = 0;

  struct branch_args left_thread_args_ptr = {
    .turn_value     = 0,
    .rem_turns      = 40,
    .num_routes_ptr = &left_num_routes
  };

  struct branch_args right_thread_args_ptr = {
    .turn_value     = 1,
    .rem_turns      = 39,
    .num_routes_ptr = &right_num_routes
  };

  pthread_t left_thread_id;
  pthread_t right_thread_id;

  pthread_create(&left_thread_id,  NULL, branch_thread, &left_thread_args_ptr);
  /* pthread_create(&right_thread_id, NULL, branch_thread, &right_thread_args_ptr); */

  /* branch(0, 40, &num_routes);        /1* must make 40 turns to traverse grid *1/ */

  unsigned int time_elapsed;

  time_elapsed = 0;


  while (1) {
    /* printf("\n(%u seconds)\ntotal routes: %llu\n", time_elapsed, left_num_routes + right_num_routes); */
    printf("\n(%u seconds)\ntotal routes: %llu\n", time_elapsed, left_num_routes);

    sleep(1);

    ++time_elapsed;
  }
}

void *branch_thread(void *thread_args)
{
  struct branch_args *args = thread_args;

  branch(args -> turn_value, args -> rem_turns, args -> num_routes_ptr);

  return NULL;
}


/***********************************************************************************
 *                                   - branch -                                    *
 * turn_value:     current node turn value (+/-1 corresponds to right/left turn)   *
 * rem_turns:      remaining turns at current node                                 *
 * num_routes_ptr: points to running total of viable routes across all nodes       *
 ***********************************************************************************/
void branch(int turn_value, int rem_turns, long long unsigned int *num_routes_ptr)
{
  if ((turn_value <= -rem_turns) || (turn_value >= rem_turns)) {
    ++(*num_routes_ptr); /* grid traversed or hit a wall, increment counter */
    return;
  }

  branch(turn_value - 1, rem_turns - 1, num_routes_ptr);
  branch(turn_value + 1, rem_turns - 1, num_routes_ptr);
}
