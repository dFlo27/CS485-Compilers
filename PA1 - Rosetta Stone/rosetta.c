#include <stdlib.h>
#include <stdio.h>
#include <string.h>

#define MAX_LENGTH 61

struct Edge {
    int valid;          // Used in Kahn's Algorithm to 'delete' an edge
    int from;           // From Node
    int to;             // To Node
};

struct Node {
    int edges;              // Number of incoming edges
    char node[MAX_LENGTH];  // Task or Requirement
};

/*  Adds a new node to nodes if the input string isn't already in nodes
    Returns the new length of nodes*/
int add_node(struct Node *nodes, char *string, int length) {
    int i;
    for (i = 0; i < length && strcmp(nodes[i].node, string) != 0; i++);
    if (i == length) {
        strcpy(nodes[length].node, string);
        nodes[length++].edges = 0;
    }
    return length;
}

/*  Finds the index of a string in nodes
    Returns the index of said string*/
int search(struct Node *nodes, char *string) {
    int i;
    for (i = 0; strcmp(nodes[i].node, string); i++);
    return i;
}

/*  Inserts a string to the string array S
    Returns nothing (void)
    */
void S_insert(char **S, char *string, int size) {
    int i, t;
    S[size] = malloc(sizeof(char) * MAX_LENGTH);
    for (i = 0; i < size && strcmp(S[i], string) < 0; i++);
    if (i == size)
        strcpy(S[i], string);
    else {
        for (t = i, i = size; i > t; i--)
            strcpy(S[i], S[i - 1]);
        strcpy(S[t], string); 
    }
}

/*  Removes the first string in array S
    Returns the pointer address of the first string*/
char *S_remove(char **S, int size) {
    int i;
    char *temp = S[0];
    for (i = 1; i < size; i++)
        S[i - 1] = S[i];
    return temp;
}

int main() {
    int i = 0, j = 0, nodes_length = 16, edges_length = 16, S_length, L_length = 0;
    char **S, **L;
    char *n;
    struct Node *nodes = malloc(sizeof(struct Node) * nodes_length);
    struct Edge *edges = malloc(sizeof(struct Edge) * edges_length);
    char task[MAX_LENGTH], req[MAX_LENGTH];
    while (fgets(task, sizeof(task), stdin) != NULL) {
        fgets(req, sizeof(req), stdin);
        task[strcspn(task, "\n")] = '\0';
        req[strcspn(req, "\n")] = '\0';
        i = add_node(nodes, task, i);
        if (i == nodes_length)
            nodes = realloc(nodes, sizeof(struct Node) * (nodes_length *= 2));
        i = add_node(nodes, req, i);
        if (i == nodes_length)
            nodes = realloc(nodes, sizeof(struct Node) * (nodes_length *= 2));
        edges[j].valid = 1;
        edges[j].to   = search(nodes, task);
        edges[j].from = search(nodes, req);
        nodes[edges[j].to].edges++;
        if (++j == edges_length)
            edges = realloc(edges, sizeof(struct Edge) * (edges_length *= 2));
    }
    nodes_length = i;
    edges_length = j;
    nodes = realloc(nodes, sizeof(struct Node) * nodes_length);
    edges = realloc(edges, sizeof(struct Edge) * edges_length);
    S = malloc(sizeof(char *) * nodes_length);
    L = malloc(sizeof(char *) * nodes_length);
    for (i = j = 0; i < nodes_length; i++)
        if (nodes[i].edges == 0)
            S_insert(S, nodes[i].node, j++);
    S = realloc(S, sizeof(char *) * (S_length = j));
    while (S_length > 0) {
        n = S_remove(S, S_length--);
        L[L_length++] = n;
        for (i = 0; i < edges_length; i++) {
            if (!edges[i].valid || strcmp(nodes[edges[i].from].node, n) != 0)
                continue;
            edges[i].valid = 0;
            nodes[edges[i].to].edges--;
            if (nodes[edges[i].to].edges == 0)
                S_insert(S, nodes[edges[i].to].node, S_length++);
        }
    }
    for (i = 0; i < edges_length && !edges[i].valid; i++);
    if (i != edges_length)
        printf("cycle\n");
    else {
        for (i = 0; i < L_length; i++)
            printf("%s\n", L[i]);
    }
    return 0;
}
