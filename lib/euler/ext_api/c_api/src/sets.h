/************************************************************************************
 *                                     sets.h                                       *
 *                                                                                  *
 * Defines constants, declares helper functions, and includes libraries shared      *
 * amongst set modules.                                                             *
 ************************************************************************************/
/************************************************************************************
 *                             PREPROCESSOR DIRECTIVES                              *
 ************************************************************************************/
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <memory.h>
#include <stdbool.h>
#include <math.h>
#include <errno.h>
#include <limits.h>

#define FORMAT_ERROR(MSG) "\n\e[31m\e[5mERROR\e[25m\n  " #MSG "\e[0m\n"
/************************************************************************************
 *                               FUNCTION PROTOTYPES                                *
 ************************************************************************************/
void mem_error(const size_t num_bytes);
void file_error(const char *filename);

/* sorting functions */
int *insert_sort_by(int *data, const size_t length,
    int (*sort_by)(const int el1, const int el2));
int *merge_sort_by( int *data, const size_t length,
    int (*sort_by)(const int el1, const int el2));
int *select_sort_by(int *data, const size_t length,
    int (*sort_by)(const int el1, const int el2));
/* sorting helpers */
void do_split(int *data, const size_t i_start, const size_t i_end,
    int (*sort_by)(const int, const int), const int sentinel);
void merge(int *data, const size_t i_start, const size_t i_split, const size_t i_end,
    int (*sort_by)(const int, const int), const int sentinel);

