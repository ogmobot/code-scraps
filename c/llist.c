/* Linked list implentation.
 * Implements a list of ints.
 * Functions defined here:
 *   create_node(n): creates a node (or new list) with value n
 *   append_node(root, n): appends n to the list containing node
 *   insert_node(root, index, n): inserts n at the given index (doesn't overwrite)
 *   remove_node(root, index): removes the node at the given index
 *   set_node(root, index, n): changes the value of the node at the given index to n
 *   get_node(root, index): retrieves the value of the node at the given index
 *   display_node(root): displays each value in the list, line-by-line
 *
 *   Right now, this whole thing is done recursively, and it's a bit of a mess tbqhwyf.
 *   Maybe clean it up by using a doubly-linked list or for loops instead of recursion.
 */

#include <stdio.h>
#include <malloc.h>

typedef struct node_t {
	int n;
	struct node_t* next;
} node_t;

node_t* create_node (int n) {
	/* Use this method to create a new list. */
	node_t* node;
	node = (node_t*) malloc(sizeof(node_t));
	node->n = n;
	node->next = NULL;
	return node;
}

int append_node (node_t* ptr, int n) {
	/* Appends a value to the end of a list.
	 * The ptr will usually be the root. */
	if (ptr == NULL) {
		return 0;
	}
	while (ptr->next) {
		ptr = ptr->next;
	}
	ptr->next = create_node(n);
	return 1;
}

int insert_node (node_t* ptr, unsigned int index, int n) {
	/* Inserts a value at the given index.
	 * The ptr should be the root.
	 * If the index is longer than the list, appends the node instead.
	 * Returns 0 if the given ptr is null and 1 otherwise.*/
	if (ptr == NULL) {
		return 0;
	}
	if (ptr->next == NULL) {
		append_node(ptr, n);
		return 1;
	}
	if (index == 0) {
		/*PANIC!*/
		/*TODO*/
		return 0;
	}
	if (index == 1) {
		node_t* node = create_node(n);
		node->next = ptr->next;
		ptr->next = node;
		return 1;
	}
	return insert_node(ptr->next, index-1, n);
}

int remove_node(node_t* ptr, unsigned int index) {
	/* Removes the node at the given index.
	 * The ptr should be the root.
	 * Returns 0 if the node doesn't exist. */
	if (index == 1) {
		if (node->next == NULL) {
			return 0;
		}
		node_t* temp = node->next->next;
		free(node->next);
		node->next = temp;
		return 1;
	}
	return 0;
}

int set_node (node_t* ptr, unsigned int index, int n) {
	/* Sets the node at the given index to n.
	 * The ptr should be the root.
	 * Returns 0 if the node doesn't exist. */
	if (ptr == NULL) {
		return 0;
	}
	if (index == 0) {
		ptr->n = n;
		return 1;
	}
	return set_node (ptr->next, index - 1, n);
}

int get_node (node_t* ptr, unsigned int index) {
	/* Retrieves the value of the node at the given index.
	 * The ptr should be the root.
	 * Returns -999999 if the node doesn't exist. */
	if (ptr == NULL) {
		return -999999;
	}
	if (index == 0) {
		return ptr->n;
	}
	return get_node (ptr->next, index - 1);
}

void display_node (node_t* ptr) {
	/* Displays a list.
	 * The ptr should be the root. */
	if (!(ptr == NULL)) {
		printf("%d\n", ptr->n);
		ptr = ptr->next;
		display_node(ptr);
	}
	return;
}

int main() {
	node_t* root;
	root = create_node(1);
	append_node(root, 3);
	append_node(root, 5);
	set_node(root, 1, 11);
	printf("%d\n", get_node(root, 2));
	display_node(root);
	return 0;
}
