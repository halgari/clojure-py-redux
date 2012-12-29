#include "Python.h"

static PyObject *simple_example(PyObject *self, PyObject *args)
{
  PyObject *x;
  x = PyTuple_GetItem(args, 0);
  PyObject *zero = PyInt_FromLong(0);
  if (PyObject_RichCompareBool(x, zero, Py_EQ)) 
    {
      Py_DECREF(args);
      return zero;
    }
  Py_DECREF(zero);

  PyObject *one = PyInt_FromLong(1);
  if (PyObject_RichCompareBool(x, one, Py_EQ)) 
    {
      Py_DECREF(args);
      return one;
    }
  Py_DECREF(one);

  PyObject *minusone = PyInt_FromLong(-1);
  PyObject *minustwo = PyInt_FromLong(-2);
  
  PyObject *leftnum = PyNumber_Add(x, minusone);
  PyObject *rightnum = PyNumber_Add(x, minustwo);

  Py_DECREF(args);
  Py_DECREF(minusone);
  Py_DECREF(minustwo);

  PyObject *leftargs = PyTuple_Pack(1, leftnum);
  PyObject *rightargs = PyTuple_Pack(2, rightnum);

  PyObject *left = simple_example(self, leftargs);
  PyObject *right = simple_example(self, rightargs);

  
  Py_DECREF(leftargs);
  Py_DECREF(rightargs);
  
  PyObject *ret = PyNumber_Add(left, right);
  
  Py_DECREF(left);
  Py_DECREF(right);
  
  return ret;


}


PyObject *_call_optimized_example(PyObject *);

static PyObject *call_optimized_example(PyObject *self, PyObject *args)
{
  PyObject *x;
  x = PyTuple_GetItem(args, 0);
  return _call_optimized_example(x);
}

PyObject *_call_optimized_example(PyObject *x)
{
  PyObject *zero = PyInt_FromLong(0);
  if (PyObject_RichCompareBool(x, zero, Py_EQ)) 
    {
      Py_DECREF(x);
      return zero;
    }
  Py_DECREF(zero);

  PyObject *one = PyInt_FromLong(1);
  if (PyObject_RichCompareBool(x, one, Py_EQ)) 
    {
      Py_DECREF(x);
      return one;
    }
  Py_DECREF(one);

  PyObject *minusone = PyInt_FromLong(-1);
  PyObject *minustwo = PyInt_FromLong(-2);
  
  PyObject *leftnum = PyNumber_Add(x, minusone);
  PyObject *rightnum = PyNumber_Add(x, minustwo);

  Py_DECREF(x);
  Py_DECREF(minusone);
  Py_DECREF(minustwo);

  PyObject *left = _call_optimized_example(leftnum);
  PyObject *right = _call_optimized_example(rightnum);

  PyObject *ret = PyNumber_Add(left, right);
  
  Py_DECREF(left);
  Py_DECREF(right);
  
  return ret;


}


long _type_hinted_example(long x);
static PyObject *type_hinted_example(PyObject *self, PyObject *args)
{
  PyObject *x;
  x = PyTuple_GetItem(args, 0);
  return PyInt_FromLong(_type_hinted_example(PyInt_AsLong(x)));
}

long _type_hinted_example(long x)
{
  if (x == 0) return 0;
  if (x == 1) return 1;
  return _type_hinted_example(x - 1) + _type_hinted_example(x - 2);
}


static PyMethodDef ExampleMethods[] = {

  {"simple_example", simple_example, METH_VARARGS, "A simple (slow) example"},
  {"call_optimized_example", call_optimized_example, METH_VARARGS, "A slightly optimized version"},
  {"type_hinted_example", type_hinted_example, METH_VARARGS, "Fully Optimized"},
  {NULL, NULL, 0, NULL}


};


PyMODINIT_FUNC
initbenchmarks (void)
{
  (void) Py_InitModule("benchmarks", ExampleMethods);
}
