/************************************************************************************
 *                                     set_2.c                                      *
 *                                                                                  *
 * Module housing solutions to problems 11-20                                       *
 ************************************************************************************/
/************************************************************************************
 *                             PREPROCESSOR DIRECTIVES                              *
 ************************************************************************************/
#include "set_2.h"
/************************************************************************************
 *                               INTIAL DECLARATIONS                                *
 ************************************************************************************/
const unsigned int DIG_MAT[100][50] = DIGITS;
const size_t DAY_NODE_BYTES         = sizeof(struct DayNode);
/* const size_t D_NODE_PTR_BYTES       = sizeof(struct DayNode); */
const size_t MONTH_NODE_BYTES       = sizeof(struct MonthNode);
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
 * 2, 3, 6, 10, 15, 21, 28, 36, 45, 55, ...																					*
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
void problem_12(char *result_buffer)
{
	const unsigned int N = 10; /* max number of iterations */

	unsigned int n;						 /* counter for calculating triangule number 'num_routes' */
	unsigned int num_routes;					 /* triangle number corresponding to counter 'n' */
	unsigned int num_divs;		 /* number of divisors for 'num_routes' */
	unsigned int smaller_div;  /* smaller of two divisors whose product is 'num_routes' */
	unsigned int min_big_div;  /* current smallest larger divisor */

  n   = 0;      /* zeroth counter */
  num_routes = 0;      /* zeroth triangle number */
  num_divs = 1; /* corresponds to 0 */ 

  while (num_divs < 500) {
    ++n;
    num_routes = num_routes + n;         /* calculate next triangle number */
		num_divs = 2;          /* corresponds to 1 and 'num_routes' */
    smaller_div = 1;
		min_big_div = num_routes;

    while (smaller_div <= min_big_div) {
      ++smaller_div;

      if (num_routes % smaller_div == 0) {
        min_big_div = num_routes / smaller_div;
        num_divs += 2;
      }
    }
	}

  sprintf(result_buffer, "%u", num_routes);
}



/************************************************************************************
 *                                  - problem_13 -                                  *
 * 																																									*
 * Work out the first ten digits of the sum of the following one-hundred 50-digit   *
 * numbers. (DIG_MAT)                                                               *
 ************************************************************************************/
void problem_13(char *result_buffer)
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
    sprintf(result_buffer, "%u", result[off_dig]); /* copy first ten significant digits */
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
void problem_14(char *result_buffer)
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

  sprintf(result_buffer, "%u", n0_max_count);
}


/************************************************************************************
 *                                  - problem_15 -                                  *
 * 																																									*
 * Starting in the top left corner of a 2×2 grid, and only being able to move to    *
 * the right and down, there are exactly 6 routes to the bottom right corner.       *
 * 																																									*
 * How many such routes are there through a 20×20 grid?                             *
 ************************************************************************************/
void problem_15(char *result_buffer)
{
  unsigned int n;                    /* counter where current grid is nxn */
  long long unsigned int num_routes; /* central bionomial coefficent */
  long long unsigned int adjacent;   /* adjacent number in sequence (see below) */

/*********************************
 *  n   ║ adjacent ║ num_routes  *
 * ═════╬══════════╬════════════ *
 *  1   ║    ∅     ║     2       *
 *  ↓   ║          ║  ↙          *
 *  + 1 ║          +             *
 *  ↓   ║       ↙  ║             *
 *  2   ║    1  → ✕ 2 →  6       *
 *  ↓   ║          ║  ↙          *
 *  + 1 ║          +             *
 *  ↓   ║       ↙  ║             *
 *  3   ║    4  → ✕ 2 →  20      *
 *  ⋮   ║    ⋮     ║     ⋮       *
 *********************************/

  n          = 1; /* for a 1x1 grid... */
  num_routes = 2; /* there are 2 possible routes */

  while (n < 20) {
    ++n;

    adjacent = num_routes * (n - 1) / n;

    num_routes = 2 * (num_routes + adjacent);
  }

  sprintf(result_buffer, "%llu", num_routes);
}

/**********************************************************************************
 *                                 - problem19 -                                  *
 *                                                                                *
 * You are given the following information, but you may prefer to do some         *
 * research for yourself.                                                         *
 *                                                                                *
 * 1 Jan 1900 was a Monday.                                                       *  
 * Thirty days has September,                                                     *  
 * April, June and November.                                                      *  
 * All the rest have thirty-one,                                                  *  
 * Saving February alone,                                                         *  
 * Which has twenty-eight, rain or shine.                                         *  
 * And on leap years, twenty-nine.                                                *  
 * A leap year occurs on any year evenly divisible by 4, but not on a century     *  
 * unless it is divisible by 400. How many Sundays fell on the first of the month *
 * during the twentieth century (1 Jan 1901 to 31 Dec 2000)?                      *
 **********************************************************************************/
void problem_19(char *result_buffer)
{
  struct DayNode   *current_day;
  struct MonthNode *current_month;

  init_day_cycle(current_day);
  init_month_cycle(current_month);

  printf("current_day:   %d\n", current_day -> name);
  printf("current_month: %d\n", current_month -> name);

}


void init_day_cycle(struct DayNode *head_ptr)
{
  struct DayNode *last_ptr;

  pushDayNode(last_ptr, SUNDAY);

  head_ptr = last_ptr;

  pushDayNode(head_ptr, SATURDAY);
  pushDayNode(head_ptr, FRIDAY);
  pushDayNode(head_ptr, THURSDAY);
  pushDayNode(head_ptr, WEDNESDAY);
  pushDayNode(head_ptr, TUESDAY);
  pushDayNode(head_ptr, MONDAY);

  last_ptr -> next_ptr = head_ptr;
}


void init_month_cycle(struct MonthNode *head_ptr)
{
  struct MonthNode *last_ptr;

  pushMonthNode(last_ptr, DECEMBER,  30);

  head_ptr = last_ptr;

  pushMonthNode(head_ptr, NOVEMBER,  30);
  pushMonthNode(head_ptr, OCTOBER,   31);
  pushMonthNode(head_ptr, SEPTEMBER, 30);
  pushMonthNode(head_ptr, AUGUST,    31);
  pushMonthNode(head_ptr, JULY,      31);
  pushMonthNode(head_ptr, JUNE,      30);
  pushMonthNode(head_ptr, MAY,       31);
  pushMonthNode(head_ptr, APRIL,     30);
  pushMonthNode(head_ptr, MARCH,     31);
  pushMonthNode(head_ptr, FEBRUARY,  28);
  pushMonthNode(head_ptr, JANUARY,   31);

  last_ptr -> next_ptr = head_ptr;
}


void pushDayNode(struct DayNode *prev_ptr,
                 const enum DayName name)
{
  struct DayNode day = {
    .name = name
  };
  /* prev_day_ptr = (struct DayNode *) malloc(DAY_NODE_BYTES); */
  /* if (prev_day_ptr == NULL) { */
  /*   mem_error(DAY_NODE_BYTES); */
  /* } */

  prev_ptr -> next_ptr = &day; 
}


void pushMonthNode(struct MonthNode *prev_ptr,
                   const enum MonthName name,
                   const int num_days)
{
  struct MonthNode month = {
    .name     = name,
    .num_days = num_days
  };

  prev_ptr -> next_ptr = &month; 
}

