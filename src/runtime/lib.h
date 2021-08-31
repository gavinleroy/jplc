/* Copyright John Regehr & Pavel Panchecka, University of Utah 2021 */

#ifndef RUNTIME_H_
#define RUNTIME_H_

#include <stdint.h>

void _readPNG(int64_t *_H, int64_t *_W, double **_data, const char *fn);
void _writePNG(int64_t H, int64_t W, double *data, const char *fn);

#endif // RUNTIME_H_
