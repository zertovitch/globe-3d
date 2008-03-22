
with interfaces.C.strings;



package X_Lib.Util is



   --function X_Lookup_String (event_struct  : access X_Key_Event;
   function X_Lookup_String (event_struct  : access X_Event;
                             buffer_return : in     interfaces.C.strings.chars_Ptr;
                             bytes_buffer  : in     interfaces.c.Int;
                             keysym_return : access Key_Sym_Id;
                             status_in_out : in     system.Address := system.null_Address) return Integer;


--extern int XLookupString(
--    XKeyEvent*		/* event_struct */,
--    char*		/* buffer_return */,
--    int			/* bytes_buffer */,
--    KeySym*		/* keysym_return */,
--    XComposeStatus*	/* status_in_out */          -- nb: not used in portable apps
                                                     -- (http://tronche.com/gui/x/xlib/utilities/XLookupString.html)
--);




private

   pragma import (C, X_Lookup_String, "XLookupString");


end X_Lib.Util;
