#include <stdlib.h>
#include <stdio.h>
#include <string.h>

#define MAX_LENGTH 61

struct Node {
    char task[MAX_LENGTH];
    struct Node *next;
};

struct Edge {
    char from[MAX_LENGTH];
    char to[MAX_LENGTH];
    struct Edge *next;
};

struct List {
    struct Edge *hd;
    struct Edge *tl;
};

struct Set {
    struct Node *hd;
    struct Node *tl;
};

void append(struct List *lst, char *from, char *to) {
    struct Edge *e = malloc(sizeof(struct Edge));
    strcpy(e->from, from);
    strcpy(e->to, to);
    e->next = NULL;
    if (lst->hd == NULL)
        lst->hd = e;   
    else
        lst->tl->next = e;
    lst->tl = e;
}

void s_append(struct Set *set, char *task) {
    struct Node *iter, *n = malloc(sizeof(struct Node));
    strcpy(n->task, task);
    n->next = NULL;
    if (set->hd == NULL) 
        set->hd = n;
    else {
        for (iter = set->hd; iter != NULL && strcmp(task, iter->task) != 0; iter = iter->next);
        if (iter != NULL)
            return;
        set->tl->next = n;   
    }
    set->tl = n;
}

void swap(struct Node *n1, struct Node *n2) {
    char temp[MAX_LENGTH];
    strcpy(temp, n1->task);
    strcpy(n1->task, n2->task);
    strcpy(n2->task, temp);
}

void sort(struct Set *set) {
    struct Node *node, *iter;
    for (node = set->hd; node != NULL; node = node->next)
        for (iter = set->hd; iter != NULL; iter = iter->next)
            if (strcmp(node->task, iter->task) <= 0)
                swap(iter, iter);
}

struct Set *trim_leaves(struct Set *nodes, struct List *edges) {
    struct Set  *leaves = malloc(sizeof(struct Set));
    struct Node *n_iter;
    struct Edge *e_iter;
    int leaf = 1;
    leaves->hd = leaves->tl = NULL;
    for (n_iter = nodes->hd; n_iter != NULL; n_iter = n_iter->next) {
        leaf = 1;
        for (e_iter = edges->hd; e_iter != NULL && leaf; e_iter = e_iter->next) 
            if (strcmp(n_iter->task, e_iter->to) == 0)
                leaf = 0;
        if (leaf)
            s_append(leaves, n_iter->task);
    }
    return leaves;
}

struct Node *pop(struct Set *set) {
    struct Node *t = set->hd;
    set->hd = set->hd->next;
    return t;
}

struct Edge *e_remove(struct List *lst, struct Edge *r) {
    struct Edge *iter;
    if (lst->hd == r) 
        lst->hd = lst->tl = lst->hd->next;
    else {
        for (iter = lst->hd; iter->next != r; iter = iter->next);
        if (iter->next = lst->tl) {
            lst->tl = iter;
            lst->tl->next = NULL;
        }
        else
            iter->next = iter->next->next;
    }
    return r;
}

struct Set *trim_edges(struct Node *m, struct List *edges) {
    struct Set *set = malloc(sizeof(struct Set));
    struct Edge *iter;
    set->hd = set->tl = NULL;
    for (iter = edges->hd; iter != NULL; iter = iter->next)
        if (strcmp(m->task, iter->from) == 0) {
            s_append(set, e_remove(edges, iter)->to);
            free(iter);
        }
    return set;
}

void attach(struct Set *dst, struct Set *set) {
    struct Node *t;
    while (set->hd != NULL) {
        t = pop(set);
        s_append(dst, t->task);
        free(t);
    }
} 

int main() {
    struct Node *n;
    struct List edges;
    struct Set nodes, *S, L;
    char to[MAX_LENGTH], from[MAX_LENGTH];
    edges.hd = edges.tl = NULL;
    nodes.hd = nodes.tl = L.hd = L.tl = NULL;
    while (fgets(to, sizeof(to), stdin) != NULL) {
        fgets(from, sizeof(from), stdin);
        from[strcspn(from, "\n")] = 0;
        to[strcspn(to, "\n")] = 0;
        s_append(&nodes, from);
        s_append(&nodes, to);
        append(&edges, from, to);
    }
    sort(&nodes);
    S = trim_leaves(&nodes, &edges);
    while (S->hd != NULL) {
        n = pop(S);
        s_append(&L, n->task);
        attach(S, trim_leaves(trim_edges(n, &edges), &edges));
        sort(S);
        free(n);
    }
    if (edges.hd == NULL) 
        printf("cycle\n");
    else for (n = L.hd; n != NULL; n = n->next)
        printf("%s\n", n->task);
    return 0;
}
