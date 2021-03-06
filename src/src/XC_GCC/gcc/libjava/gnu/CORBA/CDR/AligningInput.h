
// DO NOT EDIT THIS FILE - it is machine generated -*- c++ -*-

#ifndef __gnu_CORBA_CDR_AligningInput__
#define __gnu_CORBA_CDR_AligningInput__

#pragma interface

#include <java/io/ByteArrayInputStream.h>
#include <gcj/array.h>

extern "Java"
{
  namespace gnu
  {
    namespace CORBA
    {
      namespace CDR
      {
          class AligningInput;
      }
    }
  }
}

class gnu::CORBA::CDR::AligningInput : public ::java::io::ByteArrayInputStream
{

public:
  AligningInput(JArray< jbyte > *);
  virtual void setOffset(jint);
  virtual void align(jint);
  virtual JArray< jbyte > * getBuffer();
  virtual jint getPosition();
  virtual void seek(jint);
private:
  jint __attribute__((aligned(__alignof__( ::java::io::ByteArrayInputStream)))) offset;
public:
  static ::java::lang::Class class$;
};

#endif // __gnu_CORBA_CDR_AligningInput__
