/************************************************************************************
 *                                     sets.c                                       *
 *                                                                                  *
 * Defines constants, declares helper functions, and includes libraries shared      *
 * amongst set modules.                                                             *
 ************************************************************************************/
/************************************************************************************
 *                             PREPROCESSOR DIRECTIVES                              *
 ************************************************************************************/
#include "sets.h"
/************************************************************************************
 *                            INLINE FUNCTION PROTOTYPES                            *
 ************************************************************************************/
extern inline void *handle_malloc(const size_t total_bytes);
extern inline void *handle_calloc(const size_t count, const size_t indiv_bytes);
extern inline FILE *handle_fopen(const char *filename, const char *mode);
/************************************************************************************
 *                               TOP LEVEL FUNCTIONS                                *
 ************************************************************************************/
