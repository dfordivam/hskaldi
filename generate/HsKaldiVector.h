// FIXME -- This define should be part of build script
#define HAVE_ATLAS 1
#include <matrix/kaldi-vector.h>
using namespace kaldi;

class HsKaldiVector : public Vector <double> {
  public:
  HsKaldiVector(){ Resize (64,kSetZero); };
  void HsKaldiVectorCopyFromPtr (const double* ptr, int size)
  { CopyFromPtr(ptr,size);};
  ~HsKaldiVector(){};
};
