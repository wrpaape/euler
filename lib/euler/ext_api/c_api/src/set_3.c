/************************************************************************************
 *                                     set_3.c                                      *
 *                                                                                  *
 * Module housing solutions to problems 21-30                                       *
 ************************************************************************************/
/************************************************************************************
 *                             PREPROCESSOR DIRECTIVES                              *
 ************************************************************************************/
#include <unistd.h>
#include "sets.h"
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
  struct SortParams params_arr[4] = {
    [0 ... 1] = { .span = 7 },
    [2 ... 3] = { .span = 6 }
  };
  pthread_t sort_threads[4];
  int q_i;
  int bucket_i;
  int name_i;
  int sum_name_scores;
  struct NameNode *head_ptr;

  /* load names from txt file into an array of lists sorted by starting name char */
  struct NameNode **buckets;

  buckets = load_buckets();

  /* split buckets into 4 intervals, sort buckets of intervals in parallel threads */
  for (q_i = 0, bucket_i = 0; q_i < 4; bucket_i += params_arr[q_i].span, ++q_i) {
    /* point params at start of sorting thread's interval */
    params_arr[q_i].interval = &buckets[bucket_i];

    handle_pthread_create(&sort_threads[q_i],
                          NULL,
                          sort_buckets,
                          (void *) &params_arr[q_i]);
  }

  /* await threads */
  for (q_i = 0; q_i < 4; ++q_i) {
    handle_pthread_join(sort_threads[q_i], NULL);
  }

  sum_name_scores = 0;
  name_i = 1;
  /* calculate sum of name scores from sorted names */
  for (bucket_i = 0; bucket_i < 26; ++bucket_i) {
    head_ptr = buckets[bucket_i];

    while (head_ptr != NULL) {
      sum_name_scores += (head_ptr -> score * name_i);
      ++name_i;
      head_ptr = head_ptr -> next_ptr;
    }
  }

  sprintf(result_buffer, "%d", sum_name_scores); /* copy score total to buffer */
}


/************************************************************************************
 *                                  - problem_23 -                                  *
 * 																																									*
 * A permutation is an ordered arrangement of objects. For example, 3124 is one     *
 * possible permutation of the digits 1, 2, 3 and 4. If all of the permutations are *
 * listed numerically or alphabetically, we call it lexicographic order. The				*
 * lexicographic permutations of 0, 1 and 2 are:																		*
 * 																																									*
 *                        012   021   102   120   201   210                         *
 * 																																									*
 * What is the millionth lexicographic permutation of the digits 0, 1, 2, 3, 4, 5,  *
 * 6, 7, 8 and 9?																																		*
 ************************************************************************************/
void problem_23(char *result_buffer)
{

  long perm_count;
  int base_root_i;
  int perm_root_i;
  int last_perm_i;
  char dig_buff[] = "0123456789";
  char digit;
  struct DigitNode *initial_digits;
  struct DigitNode *curr_digit;
   
  initial_digits = handle_malloc(sizeof(struct DigitNode) * 10);
  curr_digit     = initial_digits;

  /* initialize linked list of ordered digit chars 0-9 */
  for (digit = '0'; digit < ':'; ++digit) {
    curr_digit -> value = digit;
    curr_digit = curr_digit -> next_ptr;
  }
  curr_digit -> next = NULL; /* terminate list */

  perm_count = 0;
  do_permute(initial_digits, dig_buff, &perm_count);

  sprintf(result_buffer, "%d", 42); /* copy score total to buffer */
}
/************************************************************************************
 *                                HELPER FUNCTIONS                                  *
 ************************************************************************************/
void do_permute(int num_rem_digs,
                char *digs_head_ptr,
                char *dig_buff_ptr,
                long *perm_count_ptr)
{
  *dig_buff_ptr = *digs_head_ptr; // copy next smallest digit to the buffer

  // if that was the last digit...
  if (num_rem_digs == 1) {
    ++(*perm_count_ptr);                        // permutation complete → inc counter
    free(digs_head_ptr);                        // delete head node
    printf("dig_buff: %s\n", dig_buff_ptr - 9); // print digits

    // TODO handle return from nesting
    if (*perm_count_ptr == 1e6) {
      exit(0);
      return;
    }
  }
  // if at least one digit remains...
  int head_i;
  int node_i;

  digs_tail_ptr = digs_head_ptr -> next_ptr; // point to remaining digits
  free(digs_head_ptr);                       // delete head node
  ++dig_buff_ptr;                            // point to next slot in buffer
  --num_rem_digs;                            // decrement count of remaining digits

  // first recurse with the original tail copy
  do_permute(num_rem_digs, digs_tail_ptr, dig_buff_ptr, perm_count_ptr);

  /* node_ptrs = handle_malloc(sizeof(DigitNode *) * num_rem_digs); */
  // for the remaining tail digits
  const size_t SIZE_NEXT_DIGS = sizeof(DigitNode) * num_rem_digs;
  for (head_i = 1, head_i < num_rem_digs, ++head_i) {
    head_ptr = handle_malloc(SIZE_NEXT_DIGS); // allocate memory for next list

    node_ptr = head_ptr;
    for (node_i = 0; node_i < head_i; ++node_i) {
      node_ptr -> next_ptr


    }



}


struct NameNode **load_buckets(void)
{
  FILE *names_file;          /* pointer to raw data file object */
  struct NameNode **buckets; /* 26 length array of NameNode lists */
  struct NameNode *node_ptr; /* points to next free space of NameNode pool */
  char slot_char;            /* corresponds to proper bucket index of next name */
  char scan_char;            /* holds next char scanned from file */
  char *char_ptr;            /* points to current char of NameNode's 'name' */
  int *score_ptr;            /* points to accumulating 'score' of NameNode */

  /* open the names data txt file in read mode */
  names_file = handle_fopen(NAMES_FILENAME, "r");
  /* allocate memory for 'buckets', initialize head pointers to 'NULL' */
  buckets    = handle_calloc(26, sizeof(struct NameNode *));
  /* allocate sufficient pool of memory for NameNodes */
  node_ptr   = handle_malloc(sizeof(struct NameNode) * SAFE_LEN_NAMES);

  /* read in raw data */
  fseek(names_file, 1, SEEK_SET); /* skip first opening quotation mark */
  scan_char = fgetc(names_file);  /* scan in first 'name' char */

  /* while there are remaining names... */
  while (scan_char != EOF) {
    /* link the next NameNode to the bucket corresponding to its name's first char */
    slot_char = scan_char - 'A'; /* bucket index = 'scan_char''s offset from 'A' */

    /* push new node into its bucket */
    node_ptr -> next_ptr = buckets[slot_char]; /* append bucket to NameNode */
    buckets[slot_char]   = node_ptr;           /* set NameNode as new bucket head */
    
    /* assign temporary pointers to minimize refs/derefs */
    char_ptr  = node_ptr -> name;     /* point to start of 'name' buffer */
    score_ptr = &(node_ptr -> score); /* point to 'score' */

    /* init score to 'slot_char''s char value */
    *score_ptr = (slot_char + 1);

    /* ignore first char, sorting will compare against only bucket members */

    while (1) {
      /* starting with second char... */
      scan_char = fgetc(names_file); /* scan in next 'scan_char' */
      /* if closing quotation mark is found... */
      if (scan_char == '\"') {
        break; /* name has been completely scanned, break */
      }

      *score_ptr += (scan_char - '@'); /* add char value of 'scan_char' */
      *char_ptr   = scan_char;         /* set next 'name' char */
      ++char_ptr;                      /* point to next empty 'name' char */
    }

    *char_ptr = '\0';               /* terminate completed 'name' string */
    ++node_ptr;                     /* point to next free node in NameNode pool */
    fseek(names_file, 2, SEEK_CUR); /* skip comma delim and next opening '"' */
    scan_char = fgetc(names_file);  /* scan in next 'name' char */
  }

  fclose(names_file); /* close file */

  return buckets;     /* return scanned data */
}



void *sort_buckets(void *params_ptr)
{
  struct SortParams *params;     /* pthread arg struct */
  struct NameNode **bucket_ptr;  /* points to current head of unsorted bucket list */
  struct NameNode *old_head_ptr; /* head of unsorted bucket */
  struct NameNode *new_head_ptr; /* head of sorted list */
  struct NameNode *this_ptr;     /* bucket node to be inserted into sorted list */
  struct NameNode *that_ptr;     /* comparison node when traversing sorted list */
  struct NameNode *prev_ptr;     /* prev node in sorted list, points to 'that_ptr' */
  int rem_buckets;               /* remaining unsorted buckets in interval */

  /* retrieve params from pthread args */
  params     = (struct SortParams *) params_ptr; 
  bucket_ptr = params -> interval;

  /* for all consecutive NameNode buckets this thread is responsible for... */
  for (rem_buckets = params -> span; rem_buckets > 0; ++bucket_ptr, --rem_buckets) {
    new_head_ptr = *bucket_ptr; /* init sorted list as bucket head node */

    /* if there are no nodes in the bucket, continue to the next one */
    if (new_head_ptr == NULL) {
      continue;
    }

    old_head_ptr = new_head_ptr -> next_ptr; /* pop head node from bucket */
    new_head_ptr -> next_ptr = NULL;         /* terminate new sorted list */

    /* while there are nodes left in the unsorted bucket... */
    while (old_head_ptr != NULL) {
      /* pop next head from bucket to be compared and inserted into sorted list */
      this_ptr     = old_head_ptr;
      old_head_ptr = old_head_ptr -> next_ptr;

      /* if 'this_ptr''s  name comes before 'new_head_ptr''s name... */
      if (strcmp(this_ptr     -> name,
                 new_head_ptr -> name) < 0) {

        /* make 'this_ptr' the new head of sorted list and continue */
        this_ptr -> next_ptr = new_head_ptr;
        new_head_ptr = this_ptr;
        continue;
      }

      /* otherwise traverse the sorted list until a home for 'this_ptr' is found */
      prev_ptr = new_head_ptr;
      that_ptr = prev_ptr -> next_ptr;

      /* while there are nodes remaining, and 'this' comes after 'that'...*/
      while (that_ptr != NULL &&
             (strcmp(this_ptr -> name,
                     that_ptr -> name) > 0)) {

        /* advance the comparison node 'that_ptr' and its prev node */
        prev_ptr = that_ptr;
        that_ptr = that_ptr -> next_ptr;
      }

      /* insert 'this_ptr' between 'prev_ptr' and 'that_ptr' and continue */
      prev_ptr -> next_ptr = this_ptr;
      this_ptr -> next_ptr = that_ptr;
    }

    *bucket_ptr = new_head_ptr; /* point bucket at new sorted list */
  }

  return NULL; /* must return something for pthread routine */
}
