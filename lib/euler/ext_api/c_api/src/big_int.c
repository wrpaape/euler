#include "sets.h"
#include "big_int.h"
#define CHARS_PER_DIGIT 20lu

int main(void)
{
	struct BigInt *big_int = new_big_int(-1031);

	printf("to_s: %s\n", big_int_to_s(big_int));

	return 0;
}


struct BigInt *new_big_int(int64_t integer)
{
	struct BigInt *big_int = handle_malloc(sizeof(struct BigInt));

	big_int->sign = (integer < 0) ? MINUS : PLUS;

	//TODO: everything

	big_int->mag = 1;
	big_int->digits[0] = integer;


	return big_int;
}


char *big_int_to_s(struct BigInt *big_int)
{
	char *dig_str = handle_malloc(sizeof(char) *
				      ((big_int->mag * CHARS_PER_DIGIT)
				       + 2lu));

	char *dig_char = dig_str;

	unsigned int *digit = big_int->digits;

	if (big_int->sign == MINUS) {
		*dig_char = '-';
		++dig_char;
	}

	sprintf(dig_char, "%u", *digit);

	return dig_str;
}

#undef CHARS_PER_DIGIT
