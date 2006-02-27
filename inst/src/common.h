#include <Magick++.h>
#include <R.h>
#include <Rdefines.h>

#define MAT_ELT(x, i, j, n) x[(i)+(j)*(n)]
#define INDEX(i, j, n)      ((i) + (j)*(n))

using namespace std;
using namespace Magick;

typedef         list<Image>    MagickStack;
typedef         Image          MagickImage;

extern bool verbose;

extern "C" {
    SEXP setVerbose(SEXP);
};

