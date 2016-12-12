(* BigInts:                                       F.Li, 1998-11-22
   -------                                         HDO, 2000-11-13
   Artihmetic for arbitrary size integers which are
   represented as singly-linked lists.
==================================================================*)

PROGRAM BigInts;

(*$IFDEF WINDOWS, for Borland Pascal only*)
  USES
    WinCrt, Math;
(*$ENDIF*)

(*$DEFINE SIGNED*)   (*when defined: first digit is sign +1 or -1*)

  CONST
    base = 1000;     (*base of number system used in all*)
                     (*  calculations, big digits: 0 .. base - 1*)

  TYPE
    NodePtr = ^Node;
    Node = RECORD
      next: NodePtr;
      val: INTEGER;
    END; (*RECORD*)
    BigIntPtr = NodePtr;


  FUNCTION NewNode(val: INTEGER): NodePtr;
    VAR
      n: NodePtr;
  BEGIN
    New(n);
    (*IF n == NIL THEN ...*)
    n^.next := NIL;
    n^.val := val;
    NewNode := n;
  END; (*NewNode*)

  FUNCTION Zero: BigIntPtr;
  BEGIN
    Zero := NewNode(0);
  END; (*Zero*)

  PROCEDURE Append(VAR bi: BigIntPtr; val: INTEGER);
    VAR
      n, last: NodePtr;
  BEGIN
    n := NewNode(val);
    IF bi = NIL THEN
      bi := n
    ELSE BEGIN (*l <>NIL*)
      last := bi;
      WHILE last^.next <> NIL DO BEGIN
        last := last^.next;
      END; (*WHILE*)
      last^.next := n;
    END; (*ELSE*)
  END; (*Append*)

  PROCEDURE Prepend(VAR bi: BigIntPtr; val: INTEGER);
    VAR
      n: NodePtr;
  BEGIN
    n := NewNode(val);
    n^.next := bi;
    bi := n;
  END; (*Prepend*)

  FUNCTION Sign(bi: BigIntPtr): INTEGER;
  BEGIN
(*$IFDEF SIGNED*)
      (*assert: bi <> NIL*)
      Sign := bi^.val; (*results in +1 or -1*)
(*$ELSE*)
      WriteLn('Error in Sign: no sign node available');
      Halt;
(*$ENDIF*)
  END; (*Sign*)

  FUNCTION CopyOfBigInt(bi: BigIntPtr): BigIntPtr;
    VAR
      n: NodePtr;
      cBi: BigIntPtr; (*cBi = copy of BigIntPtr*)
  BEGIN
    cBi := NIL;
    n := bi;
    WHILE n <> NIL DO BEGIN
      Append(cBi, n^.val);
      n := n^.next;
    END; (*WHILE*)
    CopyOfBigInt := cBi;
  END; (*CopyOfBigInt*)

  PROCEDURE InvertBigInt(VAR bi: BigIntPtr);
    VAR
      iBi, next: NodePtr; (*iBi = inverted BigIntPtr*)
  BEGIN
    IF bi <> NIL THEN BEGIN
      iBi := bi;
      bi := bi^.next;
      iBi^.next := NIL;
      WHILE bi <> NIL DO BEGIN
        next := bi^.next;
        bi^.next := iBi;
        iBi := bi;
        bi := next;
      END; (*WHILE*)
      bi := iBi;
    END; (*IF*)
  END; (*InvertBigInt*)

  PROCEDURE DisposeBigInt(VAR bi: BigIntPtr);
    VAR
      next: NodePtr;
  BEGIN
    WHILE bi <> NIL DO BEGIN
      next := bi^.next;
      Dispose(bi);
      bi := next;
    END; (*WHILE*)
  END; (*DisposeBigInt*)


  (* ReadBigInt: reads BigIntPtr, version for base = 1000
     Input syntax: BigIntPtr = { digit }.
                   BigIntPtr = [+ | -] digit { digit }.
     The empty string is treated as zero, and as the whole
     input is read into one STRING, max. length is 255.
  -------------------------------------------------------*)
  PROCEDURE ReadBigInt(VAR bi: BigIntPtr);
    VAR
      s: STRING;           (*input string*)
      iBeg, iEnd: INTEGER; (*begin and end of proper input *)
      bigDig, decDig: INTEGER;
      nrOfBigDigits, lenOfFirst: INTEGER;
      sign, i, j: INTEGER;

    PROCEDURE WriteWarning(warnPos: INTEGER);
    BEGIN
      WriteLn('Warning in ReadBigInt: ',
              'character ', s[warnPos],
              ' in column ', warnPos, ' is treated as zero');
    END; (*WriteWarning*)

  BEGIN (*ReadBigInt*)
    IF base <> 1000 THEN BEGIN
      WriteLn('Error in ReadBigInt: ',
              'procedure currently works for base = 1000 only');
      Halt;
    END; (*IF*)
    ReadLn(s);
    iEnd := Length(s);
    IF iEnd = 0 THEN
      bi := Zero
    ELSE BEGIN

(*$IFDEF SIGNED*)
      IF s[1] = '-' THEN BEGIN
        sign := -1;
        iBeg :=  2;
      END (*THEN*)
      ELSE IF s[1] = '+' THEN BEGIN
        sign := 1;
        iBeg := 2;
      END (*THEN*)
      ELSE BEGIN
(*$ENDIF*)
        sign := 1;
        iBeg := 1;
(*$IFDEF SIGNED*)
      END; (*ELSE*)
(*$ENDIF*)

      WHILE (iBeg <= iEnd) AND
            ((s[iBeg] < '1') OR (s[iBeg] > '9')) DO BEGIN
        IF (s[iBeg] <> '0') AND (s[iBeg] <> ' ') THEN
          WriteWarning(iBeg);
        iBeg := iBeg + 1;
      END; (*WHILE*)

      (*get value from s[iBeg .. iEnd]*)
      IF iBeg > iEnd THEN
        bi := Zero
      ELSE BEGIN
        bi := NIL;
        nrOfBigDigits := (iEnd - iBeg) DIV 3 + 1;
        lenOfFirst    := (iEnd - iBeg) MOD 3 + 1;
        FOR i := 1 TO nrOfBigDigits DO BEGIN
          bigDig := 0;
          FOR j := iBeg TO iBeg + lenOfFirst - 1 DO BEGIN
            IF (s[j] >= '0') AND (s[j] <= '9') THEN
              decDig := Ord(s[j]) - Ord('0')
            ELSE BEGIN
              WriteWarning(j);
              decDig := 0;
            END; (*ELSE*)
            bigDig := bigDig * 10 + decDig;
          END; (*FOR*)
          Prepend(bi, bigDig);
          iBeg := iBeg + lenOfFirst;
          lenOfFirst := 3;
        END; (*FOR*)
(*$IFDEF SIGNED*)
        Prepend(bi, sign);
(*$ENDIF*)
      END; (*IF*)
    END; (*ELSE*)
  END; (*ReadBigInt*)


  (* WriteBigInt: writes BigIntPtr, version for base = 1000
  -------------------------------------------------------*)
  PROCEDURE WriteBigInt(bi: BigIntPtr);
    VAR
      revBi: BigIntPtr;
      n: NodePtr;
  BEGIN
    IF base <> 1000 THEN BEGIN
      WriteLn('Error in WriteBigInt: ',
              'procedure currently works for base = 1000 only');
      Halt;
    END; (*IF*)
    IF bi = NIL THEN
      Write('0')
    ELSE BEGIN
(*$IFDEF SIGNED*)
        IF Sign(bi) = -1 THEN
          Write('-');
        revBi := CopyOfBigInt(bi^.next);
(*$ELSE*)
        revBi := CopyOfBigInt(bi);
(*$ENDIF*)
      InvertBigInt(revBi);
      n := revBi;
      Write(n^.val); (*first big digit printed without leading zeros*)
      n := n^.next;
      WHILE n <> NIL DO BEGIN
        IF n^.val >= 100 THEN
          Write(n^.val)
        ELSE IF n^.val >= 10 THEN
          Write('0', n^.val)
        ELSE (*n^.val < 10*)
          Write('00', n^.val);
        n := n^.next;
      END; (*WHILE*)
      DisposeBigInt(revBi); (*release the copy*)
    END; (*IF*)
  END; (*WriteBigInt*)

FUNCTION Sum (a, b: BigIntPtr): BigIntPtr; (*compute sum = a + b*)
  VAR
    c : BigIntPtr;
    summe, Cover : integer;
BEGIN
  summe := 0;
  Cover := 0;
  c:=NewNode(1);
  a := a^.next;
  b := b^.next;
  WHILE (a <> NIL) AND (b <> NIL) DO BEGIN
    summe := a^.val + b^.val + Cover;
    Cover := 0;
    if summe < base then
      Append(c,summe)
    else begin
      summe := summe - base;
      Cover := 1;
      Append(c,summe);
    end;
    a := a^.next;
    b := b^.next;
  end; (*While*)
  WHILE a <> NIL DO BEGIN
    Append(c,(a^.val+cover));
    cover := 0;
    a := a^.next;
  end; (*While*)
  WHILE b <> NIL DO BEGIN
    Append(c,(b^.val+cover));
    cover := 0;
    b := b^.next;
  end; (*While*)
  Sum := c;
END;

FUNCTION Product(a, b: BigIntPtr): BigIntPtr; (*compute product = a * b*)
  VAR
    c : BigIntPtr;
    sum1,sum2,sum3,i : int64;
BEGIN
  c:=NewNode(1);
  i:=0;
  sum1 := 0;
  sum2 := 0;
  a := a^.next;
  b := b^.next;
  WHILE a <> NIL DO BEGIN
    sum1 := sum1 + (a^.val * (base ** i));
    a := a^.next;
    inc(i);
  END;
  i:=0;
  WHILE b <> NIL DO BEGIN
    sum2 := sum2 + (b^.val * (base ** i));
    b := b^.next;
    inc(i);
  END;
  sum3 := sum1 * sum2;
  While sum3 <> 0 DO BEGIN
  Append(c,(sum3 mod base));
  sum3 := sum3 div base;
  END;
  Product := c;
END;

(*=== main program, for test purposes ===*)

  VAR
    bi1,bi2: BigIntPtr;

BEGIN (*BigInts*)

  (*tests for ReadBigInt and WriteBigInt only*)
  Write('1. big int > ');
  ReadBigInt(bi1);
  Write('2. big int > ');
  ReadBigInt(bi2);
  Write('big int = ');
  WriteBigInt(Product(bi1,bi2));
  WriteLn;
  //ReadLn;

END. (*BigInts*)

