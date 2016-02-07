#include <iostream>
#include <iomanip>
#include <string.h>
#include <stdlib.h>

#include <dlfcn.h>
#include "use_prj.h"

using namespace std;

const int tsize  = 1024;
const int tsize2 = 1024 * 1024 * 4;

void* lib_prj;

bool init_use_prj(){
    lib_prj = dlopen("lib_prj.so", RTLD_LAZY);
    return(lib_prj);
}

void done_use_prj() {
    if (lib_prj) dlclose(lib_prj);
    lib_prj=0;
}

tprj_init prj_init;
tprj_done prj_done;

tprj_GetMapCount prj_GetMapCount;
tprj_AddMap prj_AddMap;

tprj_GetBound prj_GetBound;

tprj_XY_BL prj_XY_BL;
tprj_BL_XY prj_BL_XY;

tprj_to_tile prj_to_tile;

tprj_bmp_saveAs prj_bmp_SaveAs;

bool sym_entries()
{
    *(void **) (&prj_init) = dlsym(lib_prj, "prj_init");
    *(void **) (&prj_done) = dlsym(lib_prj, "prj_done");
    *(void **) (&prj_GetMapCount) = dlsym(lib_prj,"prj_GetMapCount");
    *(void **) (&prj_AddMap) = dlsym(lib_prj, "prj_AddMap");
    *(void **) (&prj_GetBound) = dlsym(lib_prj, "prj_GetBound");
    *(void **) (&prj_BL_XY) = dlsym(lib_prj, "prj_BL_XY");
    *(void **) (&prj_XY_BL) = dlsym(lib_prj, "prj_XY_BL");
    *(void **) (&prj_to_tile) = dlsym(lib_prj, "prj_to_tile");
    *(void **) (&prj_bmp_SaveAs) = dlsym(lib_prj, "prj_bmp_SaveAs");

    if (!prj_init) return false;
    if (!prj_done) return false;
    if (!prj_GetMapCount) return false;
    if (!prj_AddMap) return false;
    if (!prj_GetBound) return false;
    if (!prj_BL_XY) return false;
    if (!prj_XY_BL) return false;
    if (!prj_to_tile) return false;
    if (!prj_bmp_SaveAs) return false;

    return true;
}

#pragma GCC diagnostic ignored "-Wwrite-strings"

bool test64()
{

    if (!sym_entries()) {
        cout << "sym_entries false" << endl;
        return false;
    }

    cout << "sym_entries OK" << endl;

    ptr prj=prj_init();
    if (!prj) {
        cout << "prj_init false" << endl;
        return false;
    }

    cout << "prj_init OK" << endl;


    prj_AddMap(prj,"/home/emm/neva/data/o-36-001-1.dm");
    if (prj_GetMapCount(prj)) {
        cout << "AddMap OK." << endl;

        double b[4], pix=5;
        prj_GetBound(prj,0, b);

        cout << setprecision(9) << b[0] << " " << b[2] << endl;
        cout << setprecision(9) << b[1] << " " << b[3] << endl;

        Bitmap map;
        memset(&map,0,sizeof(map));

        map.bmBitsPixel=32;
        map.bmHeight=tsize;
        map.bmWidth=tsize;
        map.bmWidthBytes=tsize*4;
        map.bmBits=malloc(tsize2);

        if (map.bmBits) {
            memset(map.bmBits,255,tsize2);

            int rc=prj_to_tile(prj,&map,b[1],b[2],pix,0);

            if (rc != 1)
              cout << "to_tile FALSE!" << endl;
            else {
                cout << "to_tile OK!" << endl;
                rc=prj_bmp_SaveAs(prj,"/home/emm/temp/1.bmp",&map);
                if (rc == 1)
                cout << "bmp_SaveAs OK!" << endl;
            }

            free(map.bmBits);
        }

    }

    prj_done(prj);
    return 0;
}

//#ifndef x86_64

bool get_dmwProject(pdmwProject* prj)
{
    *prj=0;

    if (lib_prj) {
        tdllGetInterface func;

       *(void **) (&func) = dlsym(lib_prj, "dllGetInterface");
       if (func) {
           ptr tmp;
           if (func(0,&tmp) == 0) {
               *prj=(tdmwProject*)tmp;
               return true;
           }
       }
    }

    return false;
}

int test32()
{
    pdmwProject prj;
    if (!get_dmwProject(&prj)) {
        cout << "dlsym(dllGetInterface) false" << endl;
        return 2;
    }

    cout << "dllGetInterface OK." << endl;

    prj->AddMap("/home/emm/neva/data/o-36-001-1.dm");
    if (prj->GetMapCount()) {
        cout << "AddMap OK." << endl;

        double b[4], pix=10;
        prj->GetBound(0,b);

        cout << setprecision(9) << b[0] << " " << b[2] << endl;
        cout << setprecision(9) << b[1] << " " << b[3] << endl;

        Bitmap map;
        memset(&map,0,sizeof(map));

        map.bmBitsPixel=32;
        map.bmHeight=tsize;
        map.bmWidth=tsize;
        map.bmWidthBytes=tsize*4;
        map.bmBits=malloc(tsize2);

        if (map.bmBits) {
            memset(map.bmBits,255,tsize2);

            int rc=prj->to_tile(&map,b[1],b[2],pix,0);

            if (rc != 1)
              cout << "to_tile FALSE!" << endl;
            else {
                cout << "to_tile OK!" << endl;
                rc=prj->bmp_saveAs("/home/emm/temp/3.bmp",&map);
                if (rc == 1)
                cout << "bmp_saveAs OK!" << endl;
            }

            free(map.bmBits);
        }
    }

    prj->Release();
    return 0;
}

//#endif // x86_64

int main()
{
    if (!init_use_prj())  {
        cout << "lib_prj.so not found!" << endl;
        return 1;
    }

    cout << "lib_prj.so loaded." << endl;

    test32();

    done_use_prj();

    cout << "lib_prj.so free." << endl;
    return 0;
}
