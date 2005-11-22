#include "common.h"

#include <Rinternals.h>
#include <R_ext/Rdynload.h>

/* think if I need these things here at all */

extern "C" {
    void R_init_imagine (DllInfo * info);
    void R_unload_imagine (DllInfo * info);
};

// Library initialization code
void R_init_imagine (DllInfo * info) {
}

// Library unload code
void R_unload_imagine (DllInfo * info) {
}

