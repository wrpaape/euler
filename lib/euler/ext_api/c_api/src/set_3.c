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
  int row_i;
  int col_i;
  NameTup *names; 

  names = load_names(); /* load in names from txt file */

  head_ptr = (struct BranchNode **) malloc(B_NODE_PTR_BYTES);
  if (head_ptr == NULL) {
    mem_error(B_NODE_PTR_BYTES);
  }

  *head_ptr = init_branches(tri_mat[NUM_TRI_ROWS - 1]); /* set init sums = last row */

  /* starting from the second-to-last-row and working upward... */
  for (row_i = NUM_TRI_ROWS - 2, num_cols = row_i + 1; row_i > -1; --row_i, --num_cols) {
    tri_row   = tri_mat[row_i];
    next_node = *head_ptr;
    for (col_i = 1; col_i < num_cols; ++col_i) {
      prev_node = next_node;              /* set 'prev_node' to last 'next_node' */
      next_node = prev_node -> next_node; /* set 'next_node' to next in list */
      fork_node = (struct BranchNode *) malloc(BRANCH_NODE_BYTES);
      if (fork_node == NULL) {
        mem_error(BRANCH_NODE_BYTES);
      }
      memcpy(fork_node, next_node, BRANCH_NODE_BYTES);

      /* insert 'fork_node' between 'prev_node' and 'next_node' */
      fork_node -> next_node = next_node;
      prev_node -> next_node = fork_node;
    }


    /* merge forked branches, deleting those nodes with lesser sums */
    prev_ptr = head_ptr;
    for (col_i = 0; col_i < num_cols; ++col_i) {
      prev_node = (*prev_ptr);
      next_node = prev_node -> next_node;

      if ((prev_node -> sum) > (next_node -> sum)) {
        prev_node -> sum      += tri_row[col_i];            /* increment branch sum */
        prev_node -> next_node = next_node -> next_node;    /* bridge pointer gap */
        prev_ptr               = &(prev_node -> next_node); /* set next 'prev_ptr' */
        free(next_node);                                    /* delete lesser node */
      } else {
        next_node -> sum      += tri_row[col_i];            /* increment branch sum */
        *prev_ptr              = next_node;                 /* bridge pointer gap */
        prev_ptr               = &(next_node -> next_node); /* set next 'prev_ptr' */
        free(prev_node);                                    /* delete lesser node */
      }
    }
  }
  /* the last remaining branch node should hold the greatest possible traversal sum */
  sprintf(result_buffer, "%d", (*head_ptr) -> sum);
}
/************************************************************************************
 *                                HELPER FUNCTIONS                                  *
 ************************************************************************************/
NameTup *load_names(void)
{
  FILE *names_file;          /* pointer to raw data file object */
  NameTup *name_tups;        /* points to array of 'NameTup's capturing names data */
  int name_i;                /* control counter for main loop */
  int char_i;                /* char index of current NameTup 'name' buffer */
  char scan_char;            /* holds next char scanned from file */
  unsigned short *score_ptr; /* points to accumulating 'score' of current NameTup */
  char *char_ptr;            /* points to current char of current NameTup 'name' */

  const size_t SIZE_NAMES = sizeof(NameTup) * NUM_NAMES; /* req bytes for 'name_tups' */
  
  /* open the names data txt file */
  names_file = fopen(NAMES_FILENAME, "r");
  if (names_file == NULL) {
    file_error(NAMES_FILENAME);
  }
  /* allocate memory for 'name_tups' array */
  name_tups = malloc(SIZE_NAMES);
  if (name_tups == NULL) {
    mem_error(SIZE_NAMES);
  }

  for (name_i = 0; name_i < NUM_NAMES; ++name_i) {
    /* scan by char until opening quotation mark is found */
    do {
      scan_char = fgetc(names_file);
    } while (scan_char != '\"');

    score_ptr = &name_tups[name_i].score; /* point to 'score' of current NameTup */
    char_ptr  = name_tups[name_i].name;   /* point to start of 'name' buffer */

    /* scan each name char until closing quotation mark is found */
    do {
      *char_ptr   = scan_char;         /* set next 'name' char */
      *score_ptr += (scan_char - '@'); /* add char value of 'scan_char' to score */
      ++char_ptr;                      /* point to next empty 'name' char */
      scan_char = fgetc(names_file);   /* scan in next 'scan_char' */
    } while (scan_char != '\"');

    *char_ptr = '\0'; /* terminate completed 'name' string */
  }

  fclose(names_file); /* close file */

  return name_tups; /* return scanned data */
}
