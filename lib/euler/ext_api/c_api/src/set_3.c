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
  /* struct NameBucket *name_buckets = load_buckets();       /1* load names from txt file *1/ */
  /* struct NameNode *name_tups = name_buckets -> name_tups; /1* set pointer to 'name_tups' *1/ */
  /* const size_t num_names    = name_buckets -> num_names;  /1* set total count of names *1/ */

  /* printf("first name:  %s\n",  name_tups[0].name); */
  /* printf("first score: %hu\n", name_tups[0].score); */
  /* printf("last name:   %s\n",  name_tups[num_names - 1].name); */
  /* printf("last score:  %hu\n", name_tups[num_names - 1].score); */
  /* printf("num_names:   %lu\n", num_names); */ 

  sprintf(result_buffer, "%d", 42); /* copy score total to buffer */
}
/************************************************************************************
 *                                HELPER FUNCTIONS                                  *
 ************************************************************************************/
/* struct NameBucket *load_buckets(void) */
/* { */
/*   FILE *names_file;           /1* pointer to raw data file object *1/ */
/*   struct NameBucket *buckets; /1* 26 length array of NameNode lists *1/ */
/*   struct NameBucket *name_buckets; /1* points to final pre-sorted NameNodes *1/ */
/*   struct NameNode *name_tups;  /1* NameNode array referenced by 'name_buckets' *1/ */
/*   size_t name_i;              /1* running counter of scanned names *1/ */
/*   char scan_char;             /1* holds next char scanned from file *1/ */
/*   long long *score_ptr;  /1* points to accumulating 'score' of current NameNode *1/ */
/*   char *char_ptr;             /1* points to current char of current NameNode 'name' *1/ */

/*   /1* conservative estimate of required memory to safely load in data *1/ */
/*   const size_t SIZE_NODE_POOL = sizeof(struct NameNode) * SAFE_LEN_NAMES; */
/*   const size_t SIZE_BUCKETS   = sizeof(struct NameBucket) * 26; */
  
/*   /1* open the names data txt file *1/ */
/*   handle_fopen(names_file, NAMES_FILENAME, "r"); */
/*   /1* names_file = fopen(NAMES_FILENAME, "r"); *1/ */
/*   /1* if (names_file == NULL) { *1/ */
/*   /1*   file_error(NAMES_FILENAME); *1/ */
/*   /1* } *1/ */

/*   /1* allocate memory for 'name_buckets', initialize head pointers to 'NULL' *1/ */
/*   handle_calloc(name_buckets, 26, sizeof(struct NameBucket)); */
/*   /1* name_buckets = calloc(SIZE_LOAD_INFO); *1/ */
/*   /1* if (name_buckets == NULL) { *1/ */
/*   /1*   mem_error(SIZE_LOAD_INFO); *1/ */
/*   /1* } *1/ */

/*   /1* allocate memory for 'name_tups' array *1/ */
/*   handle_malloc(name_tups, sizeof(struct NameNode) * SAFE_LEN_NAMES); */
/*   /1* name_tups = malloc(SIZE_NODE_POOL); *1/ */
/*   /1* if (name_tups == NULL) { *1/ */
/*   /1*   mem_error(SIZE_NODE_POOL); *1/ */
/*   /1* } *1/ */

/*   fseek(names_file, 1, SEEK_SET); /1* skip first opening quotation mark *1/ */
/*   scan_char = fgetc(names_file);  /1* scan in first 'name' char *1/ */
/*   name_i    = 0;                  /1* initialize name counter *1/ */
/*   while (scan_char != EOF) { */
/*     score_ptr = &name_tups[name_i].score; /1* point to 'score' of current NameNode *1/ */
/*     char_ptr  = name_tups[name_i].name;   /1* point to start of 'name' buffer *1/ */

/*     /1* until next closing quotation mark is found... *1/ */
/*     while (scan_char != '\"') { */
/*       *score_ptr += (scan_char - '@'); /1* add char value of 'scan_char' to score *1/ */
/*       *char_ptr   = scan_char;         /1* set next 'name' char *1/ */
/*       ++char_ptr;                      /1* point to next empty 'name' char *1/ */
/*       scan_char   = fgetc(names_file); /1* scan in next 'scan_char' *1/ */
/*     } */

/*     *char_ptr = '\0';               /1* terminate completed 'name' string *1/ */
/*     ++name_i;                       /1* increment name counter *1/ */
/*     fseek(names_file, 2, SEEK_CUR); /1* skip comma delim and next opening '"' *1/ */
/*     scan_char = fgetc(names_file);  /1* scan in next 'name' char *1/ */
/*   } */

/*   fclose(names_file); /1* close file *1/ */

/*   name_buckets -> name_tups = name_tups; /1* reference loaded info *1/ */
/*   name_buckets -> num_names = name_i; */

/*   return name_buckets; /1* return scanned data *1/ */
/* } */
