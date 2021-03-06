(* ADS Unit Test 4                         19.04.2017 *)
(* ----------                                         *)
(*                                                    *)
(* ================================================== *)

PROGRAM StackTADT4;

  USES 
    StackADT4;
    
  VAR 
    s1, s2: Stack;
    e: INTEGER;
    i: INTEGER;
    
BEGIN
  NewStack(s1,10);
  NewStack(s2,10);
  
  FOR i := 1 TO 10 DO BEGIN
    WriteLn('Push: ',i);
    Push(s1, i);
  END;
  
  FOR i := 1 TO 10 DO BEGIN
    WriteLn('Push2: ',i);
    Push(s2, i);
  END;
  
  FOR i := 1 TO 10 DO BEGIN
    Pop(s2, e);
    WriteLn('Pop2: ',e);
  END;
  
  FOR i := 1 TO 10 DO BEGIN
    Pop(s1, e);
    WriteLn('Pop: ',e);
  END;
  
  DisposeStack(s1);
  DisposeStack(s2);

END. (* StackTADT4 *)