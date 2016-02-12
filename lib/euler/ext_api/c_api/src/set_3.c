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
  struct LoadInfo *info_ptr; /* points to final LoadInfo result */
  struct NameTup *name_tups; /* NameTup array referenced by 'load_info' */
  info_ptr = load_names();      /* load in names from txt file */

  printf("first name:  %s\n",  info_ptr -> name_tups -> name);
  printf("first score: %hu\n", info_ptr -> name_tups -> score);
  printf("num_names:   %lu\n", info_ptr -> num_names);

  sprintf(result_buffer, "%d", 42); /* copy score total to buffer */
}
/************************************************************************************
 *                                HELPER FUNCTIONS                                  *
 ************************************************************************************/
struct LoadInfo *load_names(void)
{
  FILE *names_file;            /* pointer to raw data file object */
  struct LoadInfo *result_ptr; /* points to final LoadInfo 'result' */
  struct NameTup *name_tups;   /* NameTup array referenced by 'result' */
  size_t name_i;               /* running counter of scanned names */
  char scan_char;              /* holds next char scanned from file */
  unsigned short *score_ptr;   /* points to accumulating 'score' of current NameTup */
  char *char_ptr;              /* points to current char of current NameTup 'name' */

  /* conservative estimate of required memory to safely load in data */
  const size_t SIZE_NAME_TUPS = sizeof(struct NameTup) * SAFE_LEN_NAMES;
  const size_t SIZE_LOAD_INFO = sizeof(struct LoadInfo);
  
  /* open the names data txt file */
  names_file = fopen(NAMES_FILENAME, "r");
  if (names_file == NULL) {
    file_error(NAMES_FILENAME);
  }
  /* allocate memory for 'result' */
  result_ptr = malloc(SIZE_LOAD_INFO);
  if (result_ptr == NULL) {
    mem_error(SIZE_LOAD_INFO);
  }
  /* allocate memory for 'name_tups' array */
  name_tups = malloc(SIZE_NAME_TUPS);
  if (name_tups == NULL) {
    mem_error(SIZE_NAME_TUPS);
  }

  fseek(names_file, 1, SEEK_SET); /* skip first opening quotation mark */
  scan_char = fgetc(names_file);  /* scan in first 'name' char */
  name_i    = 0;                  /* initialize name counter */
  while (scan_char != EOF) {
    score_ptr = &name_tups[name_i].score; /* point to 'score' of current NameTup */
    char_ptr  = name_tups[name_i].name;   /* point to start of 'name' buffer */

    /* until next closing quotation mark is found... */
    while (scan_char != '\"') {
      *score_ptr += (scan_char - '@'); /* add char value of 'scan_char' to score */
      *char_ptr   = scan_char;         /* set next 'name' char */
      ++char_ptr;                      /* point to next empty 'name' char */
      scan_char   = fgetc(names_file); /* scan in next 'scan_char' */
    }

    *char_ptr = '\0';               /* terminate completed 'name' string */
    ++name_i;                       /* increment name counter */
    fseek(names_file, 2, SEEK_CUR); /* skip comma delim and next opening '"' */
    scan_char = fgetc(names_file);  /* scan in next 'name' char */
  }

  fclose(names_file); /* close file */

  /* struct LoadInfo result = { */
  /*   .name_tups = name_tups, */
  /*   .num_names = name_i */
  /* }; */

  /* result_ptr = &result; */
  result_ptr -> name_tups = name_tups;
  result_ptr -> num_names = name_i;

  return result_ptr; /* return scanned data */
}
