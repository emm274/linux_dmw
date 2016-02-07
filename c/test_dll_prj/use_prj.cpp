#include <dlfcn.h>
#include "use_prj.h"

ptr lib_prj;

bool init_use_prj(){
    lib_prj = dlopen("/home/emm/neva/bin/lib_prj.so", 0);
    return(lib_prj);
}

void done_use_prj() {
    if (lib_prj) dlclose(lib_prj);
    lib_prj=0;
}
