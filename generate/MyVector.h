#define HAVE_ATLAS 1
#include <matrix/kaldi-vector.h>
using namespace kaldi;

class MyVector : public Vector <double> {
  public:
  MyVector(){ Resize (1,kSetZero); };
  void MyCopyFromPtr (const double* ptr, int size)
  { CopyFromPtr(ptr,size);};
  ~MyVector(){};
};
