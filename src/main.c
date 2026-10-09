#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <stdbool.h>

typedef enum {
TOKEN_TYPE_AND,
TOKEN_TYPE_OR,
TOKEN_TYPE_TO,
TOKEN_TYPE_NOT,
TOKEN_TYPE_CONST,
TOKEN_TYPE_VAL,
TOKEN_TYPE_OPEN,
TOKEN_TYPE_CLOSE,
} TokenType;

typedef struct _Token{
    struct _Token *next;
    TokenType type;
    union {
        int name;
        bool val;
    } data;
} Token;

Token *tokenizer(char *buf, size_t bufSize) {
    Token *init = NULL;
    Token *prev = NULL;
    Token *tmp = NULL;

    for (size_t i = 0; i < bufSize && buf[i] != '\0'; i++) {
        if (buf[i] == '(') {
            tmp = (Token *)malloc(sizeof(Token));
            tmp->next = NULL;
            tmp->type = TOKEN_TYPE_OPEN;
        } else if (buf[i] == ')') {
            tmp = (Token *)malloc(sizeof(Token));
            tmp->next = NULL;
            tmp->type = TOKEN_TYPE_CLOSE;
        } else if (strncmp(&buf[i], "AND", 3) == 0) {
            tmp = (Token *)malloc(sizeof(Token));
            tmp->next = NULL;
            tmp->type = TOKEN_TYPE_AND;
            i+=2;
        } else if (strncmp(&buf[i], "OR", 2) == 0) {
            tmp = (Token *)malloc(sizeof(Token));
            tmp->next = NULL;
            tmp->type = TOKEN_TYPE_OR;
            i++;
        } else if (strncmp(&buf[i], "NOT", 3) == 0) {
            tmp = (Token *)malloc(sizeof(Token));
            tmp->next = NULL;
            tmp->type = TOKEN_TYPE_NOT;
            i+=2;
        } else if (strncmp(&buf[i], "TO", 2) == 0) {
            tmp = (Token *)malloc(sizeof(Token));
            tmp->next = NULL;
            tmp->type = TOKEN_TYPE_TO;
            i++;
        } else if (strncmp(&buf[i], "True", 4) == 0) {
            tmp = (Token *)malloc(sizeof(Token));
            tmp->next = NULL;
            tmp->type = TOKEN_TYPE_CONST;
            tmp->data.val = true;
            i+=3;
        } else if (strncmp(&buf[i], "False", 5) == 0) {
            tmp = (Token *)malloc(sizeof(Token));
            tmp->next = NULL;
            tmp->type = TOKEN_TYPE_CONST;
            tmp->data.val = false;
            i+=4;
        } else if ('0' <= buf[i] && buf[i] <= '9') {
            tmp = (Token *)malloc(sizeof(Token));
            tmp->next = NULL;
            tmp->type = TOKEN_TYPE_VAL;
            tmp->data.name = atoi(&buf[i]);
            while ('0' <= buf[i+1] && buf[i+1] <= '9') i++;
        } else {
            continue;
        }

        if (prev == NULL) {
            init = tmp;
            prev = tmp;
        } else {
            prev->next = tmp;
            prev = tmp;
        }
    }

    return init;
}

void testTokenizer(Token *tokenList) {
    Token *tmp = tokenList;
    while (tmp != NULL) {
        switch (tmp->type) {
            case TOKEN_TYPE_OPEN:
                printf("OPEN");
                break;
            case TOKEN_TYPE_CLOSE:
                printf("CLOSE");
                break;
            case TOKEN_TYPE_AND:
                printf("AND");
                break;
            case TOKEN_TYPE_OR:
                printf("OR");
                break;
            case TOKEN_TYPE_NOT:
                printf("NOT");
                break;
            case TOKEN_TYPE_TO:
                printf("TO");
                break;
            case TOKEN_TYPE_CONST:
                printf("CONST");
                break;
            case TOKEN_TYPE_VAL:
                printf("VAL");
                break;
            default:
                printf("Anything wrong...");
        }
        printf(" ");
        tmp = tmp->next;
    }
    printf("\n");

}

struct _astNode;

typedef enum {
NODE_TYPE_AND,
NODE_TYPE_OR,
NODE_TYPE_TO,
NODE_TYPE_NOT,
NODE_TYPE_CONST,
NODE_TYPE_VAL,
} NodeType;

typedef struct _astNode {
    NodeType type;
    struct _astNode *next;
    union {
        struct _astNode *list;
        int name;
        bool value;
    } data;
} AstNode;

typedef struct {
    AstNode *node;
    Token *token;
} RetStruct;

RetStruct _parser(Token *token) {
    RetStruct ret;
    if (token->type == TOKEN_TYPE_OPEN) {
        AstNode *node = (AstNode *)malloc(sizeof(AstNode));
        node->next = NULL;
        node->data.list = NULL;
        token = token->next;
        if (token->type == TOKEN_TYPE_AND) node->type = NODE_TYPE_AND;
        else if (token->type == TOKEN_TYPE_OR) node->type = NODE_TYPE_OR;
        else if (token->type == TOKEN_TYPE_TO) node->type = NODE_TYPE_TO;
        else if(token->type == TOKEN_TYPE_NOT) node->type = NODE_TYPE_NOT;
        else {
            puts("[!]Wrong...");
            exit(2);
        }

        token = token->next;
        AstNode *prev = NULL;
        AstNode *tmp = NULL;
        while (token->type != TOKEN_TYPE_CLOSE) {
            // const
            if (token->type == TOKEN_TYPE_CONST) {
                tmp = (AstNode *)malloc(sizeof(AstNode));
                tmp->type = NODE_TYPE_CONST;
                tmp->data.value = token->data.val;
                tmp->next = NULL;
                if (prev == NULL) node->data.list = tmp;
                else prev->next = tmp;
                prev = tmp;
                token = token->next;
            }
            // val
            else if (token->type == TOKEN_TYPE_VAL) {
                tmp = (AstNode *)malloc(sizeof(AstNode));
                tmp->type = NODE_TYPE_VAL;
                tmp->data.name = token->data.name;
                tmp->next = NULL;
                if (prev == NULL) node->data.list = tmp;
                else prev->next = tmp;
                prev = tmp;
                token = token->next;
            }
            // open
            else if (token->type == TOKEN_TYPE_OPEN) {
                RetStruct child = _parser(token);
                tmp = child.node;
                if (prev == NULL) node->data.list = tmp;
                else prev->next = tmp;
                prev = tmp;
                token = child.token;
            }

            else {
                puts("Err...!!");
                exit(5);
            }
        }

        token = token->next;
        ret.node = node;
        ret.token = token;

        return ret;
    }
    puts("Something wrong...");
    exit(4);
}

RetStruct parser(Token *token) {
    RetStruct ret;

    if (token->type == TOKEN_TYPE_CONST) {
        AstNode *node = (AstNode *)malloc(sizeof(AstNode));
        node->type = NODE_TYPE_CONST;
        node->data.value = token->data.val;
        node->next = NULL;
        ret.node = node;
        ret.token = token->next;
        return ret;
    }

    if (token->type == TOKEN_TYPE_VAL) {
        AstNode *node = (AstNode *)malloc(sizeof(AstNode));
        node->type = NODE_TYPE_VAL;
        node->data.name = token->data.name;
        node->next = NULL;
        ret.node = node;
        ret.token = token->next;
        return ret;
    }


    if (token->type == TOKEN_TYPE_OPEN) {
        return _parser(token);
    }

    puts("[!]Wrong!");
    exit(3);
}

void testParser(AstNode *node) {
    if (node == NULL)
        return;

    switch (node->type) {
        case NODE_TYPE_AND:
            printf("AND ");
            break;
        case NODE_TYPE_OR:
            printf("OR ");
            break;
        case NODE_TYPE_TO:
            printf("TO ");
            break;
        case NODE_TYPE_NOT:
            printf("NOT ");
            break;
        case NODE_TYPE_CONST:
            if (node->data.value)
                printf("True ");
            else
                printf("False ");
            break;
        case NODE_TYPE_VAL:
            printf("%d ", node->data.name);
            break;
        default:
            puts("Test missing!!");
            break;
    }

    if (node->type != NODE_TYPE_CONST && node->type != NODE_TYPE_VAL && node->data.list != NULL)
        testParser(node->data.list);

    if (node->next != NULL)
        testParser(node->next);
}

int main(int argc, char **argv) {
    if (argc < 2) {
        fprintf(stderr, "./a.out input_file");
        return 1;
    }

    FILE *fp = fopen(argv[1], "rb");
    fseek(fp, 0, SEEK_END);
    long fSize = ftell(fp);
    fseek(fp, 0, SEEK_SET);

    char *buf = (char *)malloc((size_t)fSize + 1);
    size_t rSize = fread(buf, 1, fSize, fp);
    buf[rSize] = '\0';

    Token *tokenList = tokenizer(buf, rSize);

    testTokenizer(tokenList);

    RetStruct treeinfo = parser(tokenList);
    AstNode *root = treeinfo.node;

    testParser(root);

    return 0;
}
