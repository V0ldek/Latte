#include <errno.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>
#include <stdbool.h>

// Helper used to conditionally concatenate __VA_ARGS__ using a comma.
#define VA_FWD(...) , ##__VA_ARGS__

#define TERMINATE(fmt, ...)                   \
    fprintf(stdout, fmt VA_FWD(__VA_ARGS__)), \
        exit(1)

// Prints info about a system error (set in errno) and terminates the process.
#define SYSERR(fmt, ...) \
    TERMINATE("ERROR: " fmt " (%d, %s)\n" VA_FWD(__VA_ARGS__), errno, strerror(errno))

// Checks if a call to the passed system function resulted in value `val`, and terminates with SYSERR if yes.
#define CHK_SYSERR_VAL(x, val, name)                                                                                 \
    if ((x) == (val))                                                                                                \
    {                                                                                                                \
        SYSERR("Error in: %s, file: %s, function: %s, line: %d: ", (name), __FILE__, __PRETTY_FUNCTION__, __LINE__); \
    }

// Checks if a call to the passed system function resulted in value `val`, and terminates with SYSERR if no.
#define CHK_SYSERR_NVAL(x, val, name)                                                                                \
    if ((x) != (val))                                                                                                \
    {                                                                                                                \
        SYSERR("Error in: %s, file: %s, function: %s, line: %d: ", (name), __FILE__, __PRETTY_FUNCTION__, __LINE__); \
    }

// Default version of CHK_SYSERR_VAL that tests against a -1 value.
#define CHK_SYSERR(x, name) CHK_SYSERR_VAL(x, -1, name)

#define NOTNULL(x)                                    \
    if ((x) == NULL)                                  \
    {                                                 \
        TERMINATE("internal error. null reference."); \
    }

typedef struct lat_string
{
    size_t length;
    const char *contents;
} lat_string;

void lat_print_int(int x)
{
    CHK_SYSERR(printf("%d\n", x), "printf")
}

void lat_print_string(const lat_string *str)
{
    CHK_SYSERR(printf("%s", str->contents), "printf")
    CHK_SYSERR(printf("\n"), "printf")
}

int lat_read_int()
{
    int n;
    CHK_SYSERR(scanf("%d", &n), "scanf")
    return n;
}

const lat_string *lat_new_string(const char *str, size_t len)
{
    NOTNULL(str)

    lat_string *result = malloc(sizeof(lat_string));
    CHK_SYSERR_VAL(result, NULL, "malloc")
    result->length = len;
    result->contents = str;

    return result;
}

const lat_string *lat_read_string()
{
    char *line = NULL;
    size_t len = 0;
    CHK_SYSERR(getline(&line, &len, stdin), "getline");

    return lat_new_string(line, len);
}

void lat_error()
{
    TERMINATE("runtime error");
}

void lat_nullchk(const void *ptr)
{
    if (ptr == NULL)
    {
        TERMINATE("runtime error. attempt to dereference a null.");
    }
}

const lat_string *lat_cat_strings(const lat_string *str1, const lat_string *str2)
{
    lat_nullchk(str1);
    lat_nullchk(str2);

    size_t new_length = str1->length + str2->length;
    char *result = malloc(new_length * sizeof(char));
    CHK_SYSERR_VAL(result, NULL, "malloc")

    strcpy(result, str1->contents);
    strcat(result, str2->contents);

    return lat_new_string(result, new_length);
}