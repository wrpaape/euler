/************************************************************************************
 *                                     c_api.h                                      *
 *                                                                                  *
 * Declares preprocessor directives and function prototypes needed to dispatch      *
 * a call from 'main' to the correct problem set and number.                        *
 ************************************************************************************/
/************************************************************************************
 *                             PREPROCESSOR DIRECTIVES                              *
 ************************************************************************************/
/************************************************************************************
 *                               FUNCTION PROTOTYPES                                *
 ************************************************************************************/
void parse_num(char *arg, int *num_ptr);
void dispatch(const int set_num, const int prob_num);
void time_func(void (*func_ptr)(char *));
void arg_error(void);
void parse_error(const char *arg);
void missing_set_error(const int set_num,
		       const int prob_num);
void missing_prob_error(const int prob_num,
			const int set_num);
