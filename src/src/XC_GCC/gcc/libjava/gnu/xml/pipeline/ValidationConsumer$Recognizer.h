
// DO NOT EDIT THIS FILE - it is machine generated -*- c++ -*-

#ifndef __gnu_xml_pipeline_ValidationConsumer$Recognizer__
#define __gnu_xml_pipeline_ValidationConsumer$Recognizer__

#pragma interface

#include <java/lang/Object.h>
extern "Java"
{
  namespace gnu
  {
    namespace xml
    {
      namespace pipeline
      {
          class ValidationConsumer$ElementInfo;
          class ValidationConsumer$Recognizer;
      }
    }
  }
}

class gnu::xml::pipeline::ValidationConsumer$Recognizer : public ::java::lang::Object
{

public: // actually package-private
  ValidationConsumer$Recognizer(::gnu::xml::pipeline::ValidationConsumer$ElementInfo *);
  virtual jboolean acceptCharacters();
  virtual ::gnu::xml::pipeline::ValidationConsumer$Recognizer * acceptElement(::java::lang::String *);
  virtual jboolean completed();
public:
  virtual ::java::lang::String * toString();
public: // actually package-private
  ::gnu::xml::pipeline::ValidationConsumer$ElementInfo * __attribute__((aligned(__alignof__( ::java::lang::Object)))) type;
public:
  static ::java::lang::Class class$;
};

#endif // __gnu_xml_pipeline_ValidationConsumer$Recognizer__
