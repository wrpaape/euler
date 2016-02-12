/************************************************************************************
 *                                     set_3.c                                      *
 *                                                                                  *
 * Module housing solutions to problems 21-30                                       *
 ************************************************************************************/
/************************************************************************************
 *                             PREPROCESSOR DIRECTIVES                              *
 ************************************************************************************/
#include "set_3.h"
/************************************************************************************
 *                               TOP LEVEL FUNCTIONS                                *
 ************************************************************************************/
/************************************************************************************
 *                                  - problem_22 -                                  *
 * 																																									*
 * Using names.txt (right click and 'Save Link/Target As...'), a 46K text file      *
 * containing over five-thousand first names, begin by sorting it into alphabetical *
 * order. Then working out the alphabetical value for each name, multiply this      *
 * value by its alphabetical position in the list to obtain a name score.           *
 * 																																									*
 * For example, when the list is sorted into alphabetical order, COLIN, which is    *
 * worth 3 + 15 + 12 + 9 + 14 = 53, is the 938th name in the list. So, COLIN would  *
 * obtain a score of 938 × 53 = 49714.                                              *
 * 																																									*
 * What is the total of all the name scores in the file?                            *
 ************************************************************************************/
void problem_22(char *result_buffer)
{
	unsigned int n;						/* counter for calculating triangule number 'tri_num' */
	unsigned int tri_num;	    /* triangle number corresponding to counter 'n' */
	unsigned int num_divs;		/* number of divisors for 'tri_num' */
	unsigned int smaller_div; /* smaller of two divisors whose product is 'tri_num' */
	unsigned int min_big_div; /* current smallest larger divisor */

	const unsigned int N = 10; /* max number of iterations */

  n        = 0; /* zeroth counter */
  tri_num  = 0; /* zeroth triangle number */
  num_divs = 1; /* corresponds to 'tri_num' of 0 */ 

  while (num_divs < 500) {
    ++n;                   /* increment counter */
    tri_num    += n;       /* calculate next triangle number */
    smaller_div = 1;       /* set first smaller divisor to 1 */
		min_big_div = tri_num; /* set smallest bigger divisor to 'tri_num' */
		num_divs    = 2;       /* corresponds to divisors 1 and 'tri_num' */

    while (smaller_div <= min_big_div) {
      ++smaller_div;

      if (tri_num % smaller_div == 0) {
        min_big_div = tri_num / smaller_div;
        num_divs += 2;
      }
    }
	}

  sprintf(result_buffer, "%u", tri_num); /* copy result to buffer */
}

