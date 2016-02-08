/************************************************************************************
 *                                     c_api.h                                      *
 *                                                                                  *
 * Declares preprocessor directives and function prototypes needed to dispatch      *
 * a call from 'main' to the correct problem set and number.                        *
 ************************************************************************************/
/************************************************************************************
 *                             PREPROCESSOR DIRECTIVES                              *
 ************************************************************************************/
#include <stdlib.h>
#include <stdio.h>
#include <time.h>
#include "errors.h"
#include "sets.h"
#include "set_2.h"
#include "set_7.h"

#define LEN_RES_BUF 100
/************************************************************************************
 *                               FUNCTION PROTOTYPES                                *
 ************************************************************************************/
void parse_num(char *arg, unsigned int *num_ptr);
void dispatch(const unsigned int set_num, const unsigned int prob_num);
void time_func(void (*func_ptr)(char *));
void arg_error(void);
void parse_error(const char *arg);
void missing_set_error(const unsigned int set_num, const unsigned int prob_num);
void missing_prob_error(const unsigned int prob_num, const unsigned int set_num);
