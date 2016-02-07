#ifndef USE_PRJ_H_INCLUDED
#define USE_PRJ_H_INCLUDED

#include <inttypes.h>

typedef char* pchar;
typedef void* ptr;

typedef struct tagBitmap {
  int32_t bmType;
  int32_t bmWidth;
  int32_t bmHeight;
  int32_t bmWidthBytes;
  int16_t bmPlanes;
  int16_t bmBitsPixel;
  ptr     bmBits;
} Bitmap, *PBitmap;

typedef ptr (*tprj_init)();
typedef void (*tprj_done)(ptr prj);

typedef int (*tprj_GetMapCount)(ptr prj);

typedef int (*tprj_AddMap)(ptr prj, pchar path);

typedef void (*tprj_GetBound)(ptr prj, int pps, double b[]);

typedef int (*tprj_XY_BL)(ptr prj, double x, double y, double* b, double* l);
typedef int (*tprj_BL_XY)(ptr prj, double b, double l, double* x, double* y);

typedef int (*tprj_to_tile)(ptr prj,
                            PBitmap map,
                            double left,
                            double top,
                            double pix,
                            int alfa);

typedef int (*tprj_bmp_saveAs)(ptr prj, pchar Dest, PBitmap map);

typedef int32_t HRESULT;
typedef int64_t REFIID;

#ifdef x86_64

class TUnknown {
public:
virtual HRESULT QueryInterface(REFIID riid,
                                         void **ppvObject);
virtual int AddRef();
virtual int Release();
};

class tdmwProject: public TUnknown {
public:
    virtual void Clear();

    // Загрузить проект "path"
    virtual int LoadFrom(pchar path);

    virtual pchar SaveAs(pchar path);

    virtual int GetMapCount();

    virtual int GetActiveMapIndex();

    virtual int AddMap(pchar path);

    virtual int DeleteMap(pchar path);

    virtual int GetActiveMap(pchar path);

    virtual int SetActiveMap(pchar path);

    virtual pchar GetMapPath(int i, pchar path);

    virtual int Contains_Map(pchar path);

    virtual void GetBound(int pps, double b[]);

    virtual int XY_BL(double x, double y, double* b, double* l);
    virtual int BL_XY(double b, double l, double* x, double* y);

    virtual int to_tile(PBitmap map,
                        double left,
                        double top,
                        double pix,
                        int alfa);

    virtual int bmp_saveAs(pchar Dest, PBitmap map);
};

typedef HRESULT (*tdllGetInterface) (ptr IID, ptr* Obj);

#else

#define stdcall __attribute__((stdcall))

class TUnknown {
public:
virtual HRESULT (stdcall QueryInterface)(REFIID riid,
                                         void **ppvObject);
virtual int (stdcall AddRef)();
virtual int (stdcall Release)();
};

class tdmwProject: public TUnknown {
public:
    // Очистить проект
    virtual void (stdcall Clear)();

    // Загрузить проект "path"
    virtual int (stdcall LoadFrom)(pchar path);

    // Сохранить текущий проект под именем "path"
    virtual pchar (stdcall SaveAs)(pchar path);

    // Вернуть количество карт в проекте
    virtual int (stdcall GetMapCount)();

    // Вернуть номер активной карты
    virtual int (stdcall GetActiveMapIndex)();

    // Добавить в проект карту
    virtual int (stdcall AddMap)(pchar path);

    // Удалить карту из проекта
    virtual int (stdcall DeleteMap)(pchar path);

    // Вернуть имя активной карты
    virtual int (stdcall GetActiveMap)(pchar path);

    // установить карту активной
    virtual int (stdcall SetActiveMap)(pchar path);

    // Вернуть путь [i] карты
    virtual pchar (stdcall GetMapPath)(int i, pchar path);

    // Вернуть индекс карты
    virtual int (stdcall prj_Contains_Map)(pchar path);

    // Вернуть габариты в проекции проекта (м)
    // 0 - прямоугольные координаты
    // 1 - геодезические координаты
    virtual void (stdcall GetBound)(int pps, double b[]);

    virtual int (stdcall XY_BL)(double x, double y, double* b, double* l);
    virtual int (stdcall BL_XY)(double b, double l, double* x, double* y);

    // вектор -> растр. [map] - куда рисовать
    // [left] - левая граница (м);
    // [top]  - верхняя граница (м);
    // [pix]  - размер пиксела;
    // [alfa] - прозрачность; [0..255]
    virtual int (stdcall to_tile)(PBitmap map,
                                  double left,
                                  double top,
                                  double pix,
                                  int alfa);

    virtual int (stdcall bmp_saveAs)(pchar Dest, PBitmap map);

};

typedef HRESULT (stdcall* tdllGetInterface) (ptr IID, ptr* Obj);

#endif // x86_64

typedef tdmwProject* pdmwProject;

#endif // USE_PRJ_H_INCLUDED
