/************************************************************************
 *			    set_5.c					*
 *									*
 * Module housing solutions to problems 41-50                           *
 ************************************************************************/
/************************************************************************
 *			PREPROCESSOR DIRECTIVES				*
 ************************************************************************/
#include "sets.h"
#include "set_5.h"
/************************************************************************
 *			TOP LEVEL FUNCTIONS				*
 ************************************************************************/
/************************************************************************
 *				- problem_41 -				*
 *									*
 * We shall say that an n-digit number is pandigital if it makes use of	*
 * all the digits 1 to n exactly once. For example, 2143 is a 4-digit	*
 * pandigital and is also prime.					*
 *									*
 * What is the largest n-digit pandigital prime that exists?		*
 ************************************************************************/
void problem_41(char *result_buffer)
{
	/* unsigned long long int mod_val = two_pow_mod(63llu, 1031llu); */
	/* unsigned long long int mod_val = nth_pow_mod(2llu, 63lu, 1031llu); */

	/* sprintf(result_buffer, "%llu", mod_val); */


	if (bpsw_prime_test(97)) {
		sprintf(result_buffer, "%s", "prime");
	} else {
		sprintf(result_buffer, "%s", "composite");
	}
}
/************************************************************************
 *				HELPERS					*
 ************************************************************************/
