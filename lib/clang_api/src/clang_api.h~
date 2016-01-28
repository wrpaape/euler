/************************************************************************************
 *                                   clang_api.h                                    *
 *                                                                                  *
 * Declares preprocessor directives and function prototypes needed to dispatch      *
 * a call from 'main' to the correct problem set and number.                        *
 ************************************************************************************/
/************************************************************************************
 *                             PREPROCESSOR DIRECTIVES                              *
 ************************************************************************************/
#include <stdio.h>
#include "sets.h"
#include "set_2.h"

#define PROBLEMS_PER_SET 10
#define FORMAT_ERROR(msg) "\e[31m\e[5mERROR:\e[25m\n  " #msg "\e[0m"
/************************************************************************************
 *                               FUNCTION PROTOTYPES                                *
 ************************************************************************************/
void dispatch(const unsigned int set_num, const unsigned int prob_num);
void parse_error(const char *arg);
void missing_set_error(const unsigned int set_num, const unsigned int prob_num);
void missing_prob_error(const unsigned int prob_num, const unsigned int set_num);
