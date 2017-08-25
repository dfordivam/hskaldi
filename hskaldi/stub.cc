#include <MacroPatternMatch.h>
#include <matrix/kaldi-vector.h>
using namespace kaldi;
#include "hskaldi_vector.h"


Vector_instance(float)

class MyVector : public Vector <float> {
  public:
  MyVector(){ Resize (1,kSetZero); };
  void MyCopyFromPtr (const float* ptr, int size)
  { CopyFromPtr(ptr,size);};
  ~MyVector(){};
};

MyVector tryob;
