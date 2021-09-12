/* Gavin Gray, 09.2021 */

#ifndef RUNTIME_H_
#define RUNTIME_H_

#include <stdint.h>

void _readPNG(int64_t *_H, int64_t *_W, double **_data, const char *fn);
void _writePNG(int64_t H, int64_t W, double *data, const char *fn);

/* functions exported for binding to runtime interpretation */

double get_time(void);
int32_t show(char *type_str, void *data);
void print(char *s);
/* struct pict read_image(char *filename); */
/* void write_image(struct pict input, char *filename); */

#endif // RUNTIME_H_
