/************************************************************************************
 *                                    set_7.h                                       *
 *                                                                                  *
 * Houses preprocessor directives, constant declarations, and function prototypes   *
 * needed in module 'set_7.c'.                                                      *
 ************************************************************************************/
/************************************************************************************
 *                             PREPROCESSOR DIRECTIVES                              *
 ************************************************************************************/
#include <memory.h>
#include <math.h>
#include "sets.h"
#include <stdio.h> /* required for file operations */
#include <conio.h> /* for clrscr */
#include <dos.h>   /* for delay */
#include "set_7-prob_67-data.txt"

#define NUM_TRI_ROWS 100
/************************************************************************************
 *                               FUNCTION PROTOTYPES                                *
 ************************************************************************************/
char *problem_67(void);
int **load_triangle(void);
