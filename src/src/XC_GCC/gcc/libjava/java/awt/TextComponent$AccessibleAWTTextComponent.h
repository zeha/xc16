
// DO NOT EDIT THIS FILE - it is machine generated -*- c++ -*-

#ifndef __java_awt_TextComponent$AccessibleAWTTextComponent__
#define __java_awt_TextComponent$AccessibleAWTTextComponent__

#pragma interface

#include <java/awt/Component$AccessibleAWTComponent.h>
extern "Java"
{
  namespace java
  {
    namespace awt
    {
        class Point;
        class Rectangle;
        class TextComponent;
        class TextComponent$AccessibleAWTTextComponent;
      namespace event
      {
          class TextEvent;
      }
    }
  }
  namespace javax
  {
    namespace accessibility
    {
        class AccessibleRole;
        class AccessibleStateSet;
        class AccessibleText;
    }
    namespace swing
    {
      namespace text
      {
          class AttributeSet;
      }
    }
  }
}

class java::awt::TextComponent$AccessibleAWTTextComponent : public ::java::awt::Component$AccessibleAWTComponent
{

public:
  TextComponent$AccessibleAWTTextComponent(::java::awt::TextComponent *);
  virtual ::javax::accessibility::AccessibleRole * getAccessibleRole();
  virtual ::javax::accessibility::AccessibleStateSet * getAccessibleStateSet();
  virtual ::javax::accessibility::AccessibleText * getAccessibleText();
  virtual jint getIndexAtPoint(::java::awt::Point *);
  virtual ::java::awt::Rectangle * getCharacterBounds(jint);
  virtual jint getCharCount();
  virtual jint getCaretPosition();
  virtual ::java::lang::String * getAtIndex(jint, jint);
  virtual ::java::lang::String * getAfterIndex(jint, jint);
  virtual ::java::lang::String * getBeforeIndex(jint, jint);
  virtual ::javax::swing::text::AttributeSet * getCharacterAttribute(jint);
  virtual jint getSelectionStart();
  virtual jint getSelectionEnd();
  virtual ::java::lang::String * getSelectedText();
  virtual void textValueChanged(::java::awt::event::TextEvent *);
private:
  static const jlong serialVersionUID = 3631432373506317811LL;
public: // actually package-private
  ::java::awt::TextComponent * __attribute__((aligned(__alignof__( ::java::awt::Component$AccessibleAWTComponent)))) this$0;
public:
  static ::java::lang::Class class$;
};

#endif // __java_awt_TextComponent$AccessibleAWTTextComponent__