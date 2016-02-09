/************************************************************************************
 *                                     errors.h                                     *
 *                                                                                  *
 * Defines constants, declares helper functions, and includes libraries for         *
 * handling and reporting errors.                                                   *
 ************************************************************************************/
/************************************************************************************
 *                             PREPROCESSOR DIRECTIVES                              *
 ************************************************************************************/
#include <errno.h>

#define FORMAT_ERROR(MSG) "\n\e[31m\e[5mERROR\e[25m\n  " #MSG "\e[0m\n""]]]]"
/************************************************************************************
 *                               FUNCTION PROTOTYPES                                *
 ************************************************************************************/
void mem_error(const size_t num_bytes);
void file_error(const char *filename);
