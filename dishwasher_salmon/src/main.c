#include <stddef.h>
#include <stdio.h>

#include "ds/common.h"
#include "ds/bytecode.h"
#include "ds/debug.h"
#include "ds/vm.h"

enum {
    REPL_MAX_LINE = 1024,
};

static void repl(ds_vm *vm);
static int run_file(ds_vm *vm, const char *path);
static char *read_file(const char *path);

int main(int argc, char *argv[])
{
    int ret = 0;

    ds_vm vm;
    ds_vm_init(&vm);

    if (argc == 1) {
        repl(&vm);
    } else if (argc == 2) {
        ret = run_file(&vm, argv[1]);
    } else {
        fprintf(stderr, "Usage: %s lox-file\n", argv[0]);
        ret = 2;
    }

    ds_vm_free(&vm);
    return ret;
}

static void repl(ds_vm *vm)
{
    char line[REPL_MAX_LINE];
    for (;;) {
        printf("> ");
        if (!fgets(line, REPL_MAX_LINE, stdin)) {
            if (feof(stdin)) {
                printf("\n");
                break;
            } else {
                fprintf(stderr, "Sorry, too many characters!\n");
                continue;
            }
        }
        // TODO: error messages or w/e
        ds_vm_interpret(vm, line);
    }
}

static int run_file(ds_vm *vm, const char *path)
{
    char *text = read_file(path);
    if (!text)
        return 1;

    // TODO: print error messages or w/e
    int ret = ds_vm_interpret(vm, text) != DS_VM_OK;

    free(text);
    return ret;
}

static char *read_file(const char *path)
{
    char *text = NULL;
    FILE *fp = fopen(path, "r");
    if (!fp) {
        fprintf(stderr, "Unable to open %s\n", path);
        return text;
    }

    if (fseek(fp, 0, SEEK_END) != 0) {
        fprintf(stderr, "Unable to seek end of %s\n", path);
        goto CLEANUP_FILE;
    }

    long size;
    if ((size = ftell(fp)) == -1L) {
        fprintf(stderr, "Unable to measure size of %s\n", path);
        goto CLEANUP_FILE;
    }

    rewind(fp);

    text = malloc(size + 1);
    if (!text) {
        fprintf(stderr, "Unable to allocate source code buffer\n");
        goto CLEANUP_FILE;
    }

    if (fread(text, 1, size, fp) != (size_t)size) {
        fprintf(stderr, "Error reading %s\n", path);
        free(text);
        text = NULL;
        goto CLEANUP_FILE;
    }

    text[size] = '\0';

CLEANUP_FILE:
    fclose(fp);
    return text;
}
