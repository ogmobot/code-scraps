/* BF-Integer interpreter
 * Please be gentle. */

#include <stdio.h>

int main(int argc, char** argv) {
	int memory[30000] = {0};
	int* m_ptr = memory;     /*Memory pointer*/
	char* p_ptr;             /*Program pointer*/

	FILE f = fopen(argv[1], "r");
	fclose(f);
	return 0;
}
