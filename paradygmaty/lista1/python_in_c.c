#include <Python.h>
#include "libs/biblio.h"

size_t factorial_iterative(size_t n)
{
    Py_Initialize();
    PyObject *pFunc, *pArgs, *pValue, *pModule, *pName;
    PyRun_SimpleString("import sys");
    PyRun_SimpleString("sys.path.append(\".\")");

    pName = PyUnicode_FromString("libs.biblio");
    pModule = PyImport_Import(pName);
    pFunc = PyObject_GetAttrString(pModule, "factorial_iterative");
    pArgs = PyTuple_Pack(1, PyLong_FromSize_t(n));
    pValue = PyObject_CallObject(pFunc,pArgs);

    return PyLong_AsSize_t(pValue);
}

size_t factorial_recursive(size_t n)
{
    Py_Initialize();
    PyObject *pFunc, *pArgs, *pValue, *pModule, *pName;
    PyRun_SimpleString("import sys");
    PyRun_SimpleString("sys.path.append(\".\")");

    pName = PyUnicode_FromString("libs.biblio");
    pModule = PyImport_Import(pName);
    pFunc = PyObject_GetAttrString(pModule, "factorial_recursive");
    pArgs = PyTuple_Pack(1, PyLong_FromSize_t(n));
    pValue = PyObject_CallObject(pFunc,pArgs);

    return PyLong_AsSize_t(pValue);
}

size_t gcd_iterative(size_t a, size_t b)
{
    Py_Initialize();
    PyObject *pFunc, *pArgs, *pValue, *pModule, *pName;
    PyRun_SimpleString("import sys");
    PyRun_SimpleString("sys.path.append(\".\")");

    pName = PyUnicode_FromString("libs.biblio");
    pModule = PyImport_Import(pName);
    pFunc = PyObject_GetAttrString(pModule, "gcd_iterative");
    pArgs = PyTuple_Pack(2, PyLong_FromSize_t(a), PyLong_FromSize_t(b));
    pValue = PyObject_CallObject(pFunc,pArgs);

    return PyLong_AsSize_t(pValue);
}

size_t gcd_recursive(size_t a, size_t b)
{
    Py_Initialize();
    PyObject *pFunc, *pArgs, *pValue, *pModule, *pName;
    PyRun_SimpleString("import sys");
    PyRun_SimpleString("sys.path.append(\".\")");

    pName = PyUnicode_FromString("libs.biblio");
    pModule = PyImport_Import(pName);
    pFunc = PyObject_GetAttrString(pModule, "gcd_recursive");
    pArgs = PyTuple_Pack(2, PyLong_FromSize_t(a), PyLong_FromSize_t(b));
    pValue = PyObject_CallObject(pFunc,pArgs);

    return PyLong_AsSize_t(pValue);
}

struct coefficients diophantine_solution_recursive(int64_t a, int64_t b, int64_t c)
{
    Py_Initialize();
    PyObject *pFunc, *pArgs, *pValue, *pModule, *pName;
    PyRun_SimpleString("import sys");
    PyRun_SimpleString("sys.path.append(\".\")");

    pName = PyUnicode_FromString("libs.biblio");
    pModule = PyImport_Import(pName);
    pFunc = PyObject_GetAttrString(pModule, "diophantine_solution_recursive");
    pArgs = PyTuple_Pack(3, PyLong_FromLong(a), PyLong_FromLong(b), PyLong_FromLong(c));
    pValue = PyObject_CallObject(pFunc,pArgs);

    struct coefficients coeffs;
    PyArg_ParseTuple(pValue, "LL", &coeffs.x, &coeffs.y);

    return coeffs;
}

struct coefficients diophantine_solution_iterative(int64_t a, int64_t b, int64_t c)
{
    Py_Initialize();
    PyObject *pFunc, *pArgs, *pValue, *pModule, *pName;
    PyRun_SimpleString("import sys");
    PyRun_SimpleString("sys.path.append(\".\")");

    pName = PyUnicode_FromString("libs.biblio");
    pModule = PyImport_Import(pName);
    pFunc = PyObject_GetAttrString(pModule, "diophantine_solution_iterative");
    pArgs = PyTuple_Pack(3, PyLong_FromLong(a), PyLong_FromLong(b), PyLong_FromLong(c));
    pValue = PyObject_CallObject(pFunc,pArgs);

    struct coefficients coeffs;
    PyArg_ParseTuple(pValue, "LL", &coeffs.x, &coeffs.y);

    return coeffs;
}

int main(void){
    size_t f = factorial_iterative(3);
    size_t f2 = factorial_recursive(3);
    size_t g = gcd_iterative(54,36);
    size_t g2 = gcd_recursive(54,36);
    struct coefficients c = diophantine_solution_iterative(10,6,14);
    struct coefficients c2 = diophantine_solution_recursive(10,6,14);

    printf("3! || %ld, %ld\n", f, f2);
    printf("gcd(54,36) || %ld, %ld\n", g, g2);
    printf("10x + 6y = 14 || %ld, %ld || %ld, %ld\n", c.x, c.y, c2.x, c2.y);
}