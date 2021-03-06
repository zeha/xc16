/* gnu/regexp/CharIndexedCharSequence.java
   Copyright (C) 2006 Free Software Foundation, Inc.

This file is part of GNU Classpath.

GNU Classpath is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 2, or (at your option)
any later version.

GNU Classpath is distributed in the hope that it will be useful, but
WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
General Public License for more details.

You should have received a copy of the GNU General Public License
along with GNU Classpath; see the file COPYING.  If not, write to the
Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA
02110-1301 USA.

Linking this library statically or dynamically with other modules is
making a combined work based on this library.  Thus, the terms and
conditions of the GNU General Public License cover the whole
combination.

As a special exception, the copyright holders of this library give you
permission to link this library with independent modules to produce an
executable, regardless of the license terms of these independent
modules, and to copy and distribute the resulting executable under
terms of your choice, provided that you also meet, for each linked
independent module, the terms and conditions of the license of that
module.  An independent module is a module which is not derived from
or based on this library.  If you modify this library, you may extend
this exception to your version of the library, but you are not
obligated to do so.  If you do not wish to do so, delete this
exception statement from your version. */

package gnu.java.util.regex;
import java.io.Serializable;

class CharIndexedCharSequence implements CharIndexed, Serializable
{
  private CharSequence s;
  private int anchor;
  private int len;

    CharIndexedCharSequence (CharSequence s, int index)
  {
    this.s = s;
    len = s.length ();
    anchor = index;
  }

  public char charAt (int index)
  {
    int pos = anchor + index;
    return ((pos < len) && (pos >= 0)) ? s.charAt (pos) : OUT_OF_BOUNDS;
  }

  public boolean isValid ()
  {
    return (anchor < len);
  }

  public boolean move (int index)
  {
    return ((anchor += index) < len);
  }

  public boolean move1 (int index)
  {
    return ((anchor += index) <= len);
  }

  public CharIndexed lookBehind (int index, int length)
  {
    if (length > (anchor + index))
      length = anchor + index;
    return new CharIndexedCharSequence (s, anchor + index - length);
  }

  public int length ()
  {
    return len - anchor;
  }

  private REMatch lastMatch;
  public void setLastMatch (REMatch match)
  {
    lastMatch = (REMatch) match.clone ();
    lastMatch.anchor = anchor;
  }
  public REMatch getLastMatch ()
  {
    return lastMatch;
  }

  private int rightmostTriedPosition = 0;
  public void setHitEnd (REMatch match)
  {
    int pos = anchor + match.index;
    if (pos > rightmostTriedPosition)
      rightmostTriedPosition = pos;
  }
  public boolean hitEnd ()
  {
    return rightmostTriedPosition >= len;
  }

  public int getAnchor ()
  {
    return anchor;
  }
  public void setAnchor (int anchor)
  {
    this.anchor = anchor;
  }

}
