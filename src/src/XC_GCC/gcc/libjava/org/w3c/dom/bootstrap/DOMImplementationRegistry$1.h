
// DO NOT EDIT THIS FILE - it is machine generated -*- c++ -*-

#ifndef __org_w3c_dom_bootstrap_DOMImplementationRegistry$1__
#define __org_w3c_dom_bootstrap_DOMImplementationRegistry$1__

#pragma interface

#include <java/lang/Object.h>
extern "Java"
{
  namespace org
  {
    namespace w3c
    {
      namespace dom
      {
          class DOMImplementation;
        namespace bootstrap
        {
            class DOMImplementationRegistry;
            class DOMImplementationRegistry$1;
        }
      }
    }
  }
}

class org::w3c::dom::bootstrap::DOMImplementationRegistry$1 : public ::java::lang::Object
{

public: // actually package-private
  DOMImplementationRegistry$1(::org::w3c::dom::bootstrap::DOMImplementationRegistry *, ::java::util::Vector *);
public:
  virtual ::org::w3c::dom::DOMImplementation * item(jint);
  virtual jint getLength();
public: // actually package-private
  ::org::w3c::dom::bootstrap::DOMImplementationRegistry * __attribute__((aligned(__alignof__( ::java::lang::Object)))) this$0;
private:
  ::java::util::Vector * val$implementations;
public:
  static ::java::lang::Class class$;
};

#endif // __org_w3c_dom_bootstrap_DOMImplementationRegistry$1__
