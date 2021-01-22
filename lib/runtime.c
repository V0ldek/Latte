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

#define NOTNULL(x)                                      \
    if ((x) == NULL)                                    \
    {                                                   \
        TERMINATE("internal error. null reference.\n"); \
    }

typedef struct lat_string
{
    size_t length;
    const char *contents;
} lat_string;

void lat_print_int(int32_t x)
{
    CHK_SYSERR(printf("%d\n", x), "printf")
}

void lat_print_string(const lat_string *str)
{
    CHK_SYSERR(printf("%s", str->contents), "printf");
    CHK_SYSERR(printf("\n"), "printf")
}

int32_t lat_read_int()
{
    char *line = NULL;
    size_t len = 0;
    CHK_SYSERR(getline(&line, &len, stdin), "getline");

    return strtol(line, NULL, 10);
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

void *lat_new_instance(size_t size)
{
    if (size < 8)
    {
        TERMINATE("internal error. invalid instance size %ld, must be at least 8 to hold the vtable pointer", size);
    }

    void *result = calloc(1, size);
    CHK_SYSERR_VAL(result, NULL, "calloc")

    return result;
}

void *lat_new_array(int32_t count, size_t size)
{
    if (size == 0)
    {
        TERMINATE("internal error. invalid instance size 0");
    }
    if (count <= 0)
    {
        TERMINATE("runtime error. non-positive array size.\n");
    }

    void *result;

    if (size < 4)
    {
        if (4 % size != 0)
        {
            TERMINATE("internal error. element size does not divide 4");
        }

        size_t length_field_count = 4 / size;
        result = calloc(count + length_field_count, size);
    }
    else
    {
        result = calloc(count + 1, size);
    }

    CHK_SYSERR_VAL(result, NULL, "calloc")
    int32_t *length_field = result;
    *length_field = count;

    return result;
}

const lat_string *lat_read_string()
{
    char *line = NULL;
    size_t line_size;
    ssize_t len = getline(&line, &line_size, stdin);

    CHK_SYSERR(len, "getline");

    if (line[len - 1] == '\n')
    {
        line[len - 1] = '\0';
        len -= 1;
    }

    return lat_new_string(line, len);
}

void lat_error()
{
    TERMINATE("runtime error\n");
}

void lat_nullref()
{
    TERMINATE("runtime error. attempt to dereference a null.\n");
}

void lat_nullchk(const void *ptr)
{
    if (ptr == NULL)
    {
        lat_nullref();
    }
}

const lat_string *lat_cat_strings(const lat_string *str1, const lat_string *str2)
{
    lat_nullchk(str1);
    lat_nullchk(str2);

    size_t new_length = str1->length + str2->length;
    char *result = malloc((new_length + 1) * sizeof(char));
    CHK_SYSERR_VAL(result, NULL, "malloc")

    strcpy(result, str1->contents);
    strcat(result, str2->contents);
    result[new_length] = '\0';

    return lat_new_string(result, new_length);
}