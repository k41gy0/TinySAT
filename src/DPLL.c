#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <stdbool.h>


typedef struct {
    int *literals;
    size_t count;
} Clause;

typedef struct {
    Clause *clauses;
    size_t clause_count;
    size_t variable_count;
} CNF;

int ReadCNF(char *buf, size_t rSize, CNF** cnfptr) {
    char *bufEnd = buf + rSize;

    while (*buf == 'c') {
        while (buf < bufEnd && *buf != '\n') {
            buf++;
        }

        if (buf == bufEnd) {
            printf("Comment Only\n");
            return -1;
        }

        if (*buf == '\n') buf++;

    }

    if ((size_t)(bufEnd - buf) < 6 || strncmp(buf, "p cnf ", 6) != 0) {
        printf("Error: DIMACS format\n");
        return -1;
    }

    buf += 6;

    long value;
    char *tmp;

    CNF *cnf = (CNF *)malloc(sizeof(CNF));
    if (cnf == NULL) {
        printf("malloc err\n");
        return -1;
    }

    *cnfptr = cnf;

    value = strtol(buf, &tmp, 10);
    if (buf == tmp || value < 0) {
        printf("Missing variable_count\n");
        return -1;
    }
    cnf->variable_count = (size_t)value;
    buf = tmp;

    value = strtol(buf, &tmp, 10);
    if (buf == tmp || value < 0) {
        printf("Missing clause_count\n");
        return -1;
    }
    cnf->clause_count = (size_t)value;
    buf = tmp;

    cnf->clauses = (Clause *)malloc(sizeof(Clause) * cnf->clause_count);
    if (cnf->clauses == NULL) {
        printf("malloc err\n");
        return 1;
    }

    for (size_t i = 0; i < cnf->clause_count; i++) {
        int flag = 0;
        cnf->clauses[i].literals = (int *)malloc(sizeof(int) * cnf->variable_count);
        if (cnf->clauses == NULL) {
            printf("malloc err\n");
            return -1;
        }

        cnf->clauses[i].count = 0;

        while (buf < bufEnd) {
            if (cnf->clauses[i].count > cnf->variable_count) {
                printf("Too many symbols!\n");
                return -1;
            }

            value = strtol(buf, &tmp, 10);
            if (buf == tmp) {
                printf("Miss Read the symbol\n");
                return -1;
            }
            buf = tmp;

            if (value == 0) {
                flag = 1;
                break;
            }

            cnf->clauses[i].literals[cnf->clauses[i].count] = (int)value;
            cnf->clauses[i].count++;
        }

        if (!flag) {
            printf("Missing the end symbol 0\n");
            return -1;
        }
    }
    return 0;
}

typedef enum {
VAL_UNASSIGNED,
VAL_FALSE,
VAL_TRUE,
} Value;

bool isFalsified(CNF *cnf, Value *assignments) {
    for (size_t i = 0; i < cnf->clause_count; i++) {

        bool flag = true;

        for (size_t j = 0; j < cnf->clauses[i].count; j++) {
            int litelal = cnf->clauses[i].literals[j];
            if (litelal > 0) {
                switch (assignments[(size_t)litelal]) {
                    case VAL_UNASSIGNED:
                    case VAL_TRUE:
                        flag = false;
                        break;
                    case VAL_FALSE:
                        break;
                }
            } else {
                litelal = -1 * litelal;
                switch (assignments[(size_t)litelal]) {
                    case VAL_UNASSIGNED:
                    case VAL_FALSE:
                        flag = false;
                        break;
                    case VAL_TRUE:
                        break;
                }
            }
            if (flag == false) {
                break;
            }
        }

        if (flag == true) {
            return true;
        }
    }
    return false;
}

bool propagation(CNF *cnf, Value *assignments) {
    for (size_t i = 0; i < cnf->clause_count; i++) {

        int unassigned = 0;
        int candidate = 0;
        bool flag = true;

        for (size_t j = 0; j < cnf->clauses[i].count; j++) {
            int litelal = cnf->clauses[i].literals[j];
            if (litelal > 0) {
                if (assignments[(size_t)litelal] == VAL_UNASSIGNED && unassigned == 0) {
                    unassigned++;
                    candidate = litelal;
                } else if (assignments[(size_t)litelal] == VAL_UNASSIGNED && unassigned > 0) {
                    flag = false;
                    break;
                } else if (assignments[(size_t)litelal] == VAL_TRUE) {
                    flag = false;
                    break;
                } else {
                    continue;
                }
            } else {
                litelal = -1 * litelal;
                if (assignments[(size_t)litelal] == VAL_UNASSIGNED && unassigned == 0) {
                    unassigned++;
                    candidate = -1 * litelal;
                } else if (assignments[(size_t)litelal] == VAL_UNASSIGNED && unassigned > 0) {
                    flag = false;
                    break;
                } else if (assignments[(size_t)litelal] == VAL_FALSE) {
                    flag = false;
                    break;
                } else {
                    continue;
                }
            }
        }

        if (flag == true && unassigned == 1) {
            if (candidate > 0) {
                assignments[(size_t)candidate] = VAL_TRUE;
            } else {
                candidate = -1 * candidate;
                assignments[(size_t)candidate] = VAL_FALSE;
            }
            return true;
        }

    }
    return false;
}

bool isSatisfied(CNF *cnf, Value *assignments) {
    for (size_t i = 0; i < cnf->clause_count; i++) {

        bool flag = false;

        for (size_t j = 0; j < cnf->clauses[i].count; j++) {
            int litelal = cnf->clauses[i].literals[j];
            if (litelal > 0) {
                if (assignments[(size_t)litelal] == VAL_TRUE) {
                    flag = true;
                }
            } else {
                litelal = -1 * litelal;
                if (assignments[(size_t)litelal] == VAL_FALSE) {
                    flag = true;
                }
            }
        }

        if (flag == false) {
            return false;
        }
    }
    return true;
}

int chooseLitelal(CNF *cnf, Value *assignments) {
    for (size_t i = 1; i < (cnf->variable_count + 1); i++) {
        if (assignments[i] == VAL_UNASSIGNED) return i;
    }

    return 0;
}

//false -> UNSAT / true -> SAT
bool DPLL(CNF *cnf, Value *assignments) {
    Value assignmentsLocal[cnf->variable_count + 1];
    memcpy(assignmentsLocal, assignments, sizeof(Value) * (cnf->variable_count + 1));

    if (isFalsified(cnf, assignmentsLocal)) return false;

    // propagation
    while (propagation(cnf, assignmentsLocal)) {
        if (isFalsified(cnf, assignmentsLocal)) return false;
    }

    if (isSatisfied(cnf, assignmentsLocal)) return true;

    int litelal = chooseLitelal(cnf, assignmentsLocal);
    if (litelal == 0) printf("Something wrong.....\n Dont trust this answer!\n");

    assignmentsLocal[litelal] = VAL_FALSE;
    if (DPLL(cnf, assignmentsLocal) == true) return true;
    assignmentsLocal[litelal] = VAL_TRUE;
    if (DPLL(cnf, assignmentsLocal) == true) return true;

    return false;
}

int main(int argc, char **argv) {
    if (argc < 2) {
        fprintf(stderr, "./a.out input_file.DIMACS\n");
        return 1;
    }

    FILE *fp = fopen(argv[1], "rb");
    if (fp == NULL) {
        printf("fopen err\n");
        return 1;
    }

    if (fseek(fp, 0, SEEK_END) < 0) {
        printf("fseek err\n");
        return 1;
    }

    long fSize = ftell(fp);
    if (fSize < 0) {
        printf("ftell err\n");
        return 1;
    }

    if (fseek(fp, 0, SEEK_SET) < 0) {
        printf("fseek err\n");
        return 1;
    }

    char *buf = (char *)malloc((size_t)fSize + 1);
    if (buf == NULL) {
        printf("malloc err\n");
        return 1;
    }

    size_t rSize = fread(buf, 1, fSize, fp);
    buf[rSize] = '\0';

    CNF *cnf;
    if (ReadCNF(buf, rSize, &cnf) < 0) {
        printf("Read Error!\n");
        return 1;
    }

    Value assignments[cnf->variable_count + 1];
    for (size_t i = 0; i < cnf->variable_count + 1; i++) {
        assignments[i] = VAL_UNASSIGNED;
    }

    bool result = DPLL(cnf, assignments);
    if (result == true) {
        printf("SAT\n");
    } else {
        printf("UNSAT\n");
    }

    return 0;
}
