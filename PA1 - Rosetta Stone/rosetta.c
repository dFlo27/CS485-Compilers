#include <stdlib.h>
#include <stdio.h>
#include <string.h>

#define MAX_LENGTH 60

struct Node {
    int num_edges;          /* Keeps track of # of edges    */   
    char task[MAX_LENGTH];          /* Task that needs to be done   */
    char *edges[MAX_LENGTH];        /* Contains list of edges       */
};

void update(struct Node *graph, char *task, char *req, int *index) {
    int i;
    for (i = 0; i < *index; i++)
        if (!strcmp(graph[i].task, task))
            break;
    if (i != *index) {
        graph[i].num_edges++;
        graph[i].edges = realloc(sizeof(struct Node) * graph[i].num_edges);
        strcpy(graph[i].edges[graph[i].num_edges - 1], req);
    }
    else {
        graph[i].num_edges = 1;
        strcpy(graph[i].task, task);
        graph[i].edges = malloc(sizeof(struct Node));
        strcpy(graph[i].edges[0], req);
    }
}

int main() {
    int i, length = 16;
    struct Node *graph = malloc(sizeof(struct Node) * length);
    char task[MAX_LENGTH], req[MAX_LENGTH];
    while(1) {
        scanf("%s\n%s", task, req);
        printf("%s\n%s", task, req);
        /*update(graph, task, req, &i);
        if (i == length) {
            size *= 2;
            graph = realloc(sizeof(struct Node) * length);
        }*/
    }
    /*graph = realloc(sizeof(struct Node) * i);
    length = i;*/
    return 0;
}
