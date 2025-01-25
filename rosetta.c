#include <stdlib.h>
#include <stdio.h>
#include <string.h>

struct Node {
    int num_edges;          /* Keeps track of # of edges    */   
    char task[60];          /* Task that needs to be done   */
    char *edges[60];        /* Contains list of edges       */
};

void update(struct Node *graph, char *task, int index) {
    int i;
    for (i = 0; i < index; i++)
        if (!strcmp(graph[i].task, task))
            break;
    if (i != index) {
        graph[i].
    }
}

int main() {
    struct Node *graph = malloc(sizeof(struct Node) * 10);
    int i = 0;
    int max = 10;
    char in[60];
    while (1) {
        scanf("%s", in);
        update(graph, in, i);
        if (i == max) {
            max *= 2;
            graph = realloc(sizeof(struct Node) * max);
        }
    }
}
