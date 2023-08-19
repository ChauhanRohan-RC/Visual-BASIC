Open "numerical_roots_output5.txt" For Output As #1
Cls
Screen 12

Dim Shared FORCE_IVT% ' Should check Intermediate Value Theorem
FORCE_IVT% = 1

' Function used all over the program
Dim Shared FUNCTION_NUMBER%
FUNCTION_NUMBER% = 1

MAX_ITERATIONS% = 100
TOLERANCE## = 1E-5 ' Error that can be tolerated

pl "............. Numerical root finding methods ..............."
main:
lb
FUNCTION_NUMBER% = in_int%(" -> Enter Function Number (as defined in program): ")
x0## = in_float64##(" -> Enter the first initial approximation: ")
x1## = in_float64##(" -> Enter the second initial approximation: ")
FORCE_IVT% = in_int%(" -> IVT Enabled (1: enable, any: disable): ")

lb
pl "## 1. Binary Bisection Method"
root_bisect## = bisect##(x0##, x1##, MAX_ITERATIONS%, TOLERANCE##)
pl "Bisection Root: " + s$(root_bisect##)
lb
pl "## 2. Regular Falsi (Chord) Method"
root_chord## = chord##(x0##, x1##, MAX_ITERATIONS%, TOLERANCE##)
pl "Chord Root: " + s$(root_chord##)
lb
pl "## 3. Secant Method"
root_secant## = secant##(x0##, x1##, MAX_ITERATIONS%, TOLERANCE##)
pl "Secant Root: " + s$(root_secant##)
lb
pl "## 4. Newton-Raphson Method"
root_raphson## = raphson##(x0##, MAX_ITERATIONS%, TOLERANCE##)
pl "Raphson Root: " + s$(root_raphson##)
lb
GoTo main


' Function Definitions
Function f1## (x##)
    f1## = x## - (3 ^ 0.5)
End Function

Function f2## (x##)
    f2## = (x## * x## * x##) - (x## * x##) + 1
End Function

Function f3## (x##)
    f3## = (x## * Exp(x##)) - Cos(x##)
End Function

Function f4## (x##)
    f4## = (x## * x## * x##) - Sin(x##) + 4
End Function

Function f5## (x##)
    f5## = (x## * Exp(x##)) - 1
End Function

' Function used all over the program
' Change the base function here
Function f## (x##)
    Select Case FUNCTION_NUMBER%
        Case 2
            f## = f2##(x##)
        Case 3
            f## = f3##(x##)
        Case 4
            f## = f4##(x##)
        Case 5
            f## = f5##(x##)
        Case Else
            f## = f1##(x##)
    End Select
End Function

Function numericalDerivative## (x##, h##)
    numericalDerivative## = (f##(x## + h##) - f##(x##)) / h##
End Function

Function numericalDerivativeDef## (x##)
    numericalDerivativeDef## = numericalDerivative##(x##, DEFAULT_NUMERICAL_DERIVATIVE_STEP##)
End Function

Function isBw% (v##, left##, right##) ' checks if a number is in between left and right values
    If (v## >= left## And v## <= right##) Or (v## <= left## And v## >= right##) Then
        isBw% = 1
    Else
        isBw% = 0
    End If
End Function

Function bisect## (firstInitialApproximation##, secondInitialApproximation##, maxItrs%, tolerance##)
    left## = firstInitialApproximation##
    right## = secondInitialApproximation##
    leftVal## = f##(left##)
    rightVal## = f##(right##)

    If FORCE_IVT% = 1 And isBw%(0, leftVal##, rightVal##) = 0 Then
        pl "Bisection -> FATAL: IVT violated due to bad initial approximation  x0: " + s$(left##) + ",  x1: " + s$(right##)
        bisect## = -1 ' Invalid initial approximations
    Else
        mid## = -1
        midVal## = -1
        itr% = 1

        Do
            mid## = (left## + right##) / 2
            midVal## = f##(mid##)
            pl "Bisection Iteration " + s$(itr%) + " -> Root: " + s$(mid##) + ",  Root Value: " + s$(midVal##)

            If Abs(midVal##) <= tolerance## Then
                pl "Bisection -> Root Found at iteration " + s$(itr%)
                Exit Do
            End If

            If isBw%(0, leftVal##, midVal##) = 1 Then
                right## = mid##
            ElseIf isBw%(0, midVal##, rightVal##) = 1 Then
                left## = mid##
            ElseIf FORCE_IVT% = 1 Then
                pl "Bisection -> FATAL: IVT violated, returning last root..."
                Exit Do
            End If

            itr% = itr% + 1
        Loop While itr% <= maxItrs%

        bisect## = mid##
    End If
End Function

Function chord## (firstInitialApproximation##, secondInitialApproximation##, maxItrs%, tolerance##)
    left## = firstInitialApproximation##
    right## = secondInitialApproximation##
    leftVal## = f##(left##)
    rightVal## = f##(right##)

    If FORCE_IVT% = 1 And isBw%(0, leftVal##, rightVal##) = 0 Then
        pl "Chord -> FATAL: IVT violated due to bad initial approximation  x0: " + s$(left##) + ",  x1: " + s$(right##)
        chord## = -1 ' Invalid initial approximations
    Else
        mid## = -1
        midVal## = -1
        itr% = 1

        Do
            mid## = ((right## * leftVal##) - (left## * rightVal##)) / (leftVal## - rightVal##)
            midVal## = f##(mid##)
            pl "Chord Iteration " + s$(itr%) + " -> Root: " + s$(mid##) + ",  Root Value: " + s$(midVal##)

            If Abs(midVal##) <= tolerance## Then
                pl "Chord -> Root found at iteration " + s$(itr%)
                Exit Do
            End If

            If isBw%(0, leftVal##, midVal##) = 1 Then
                right## = mid##
            ElseIf isBw%(0, midVal##, rightVal##) = 1 Then
                left## = mid##
            ElseIf FORCE_IVT% = 1 Then
                pl "Chord -> FATAL: IVT violated, returning last root..."
                Exit Do
            End If

            itr% = itr% + 1
        Loop While itr% <= maxItrs%

        chord## = mid##
    End If
End Function

Function secant## (firstInitialApproximation##, secondInitialApproximation##, maxItrs%, tolerance##)
    left## = firstInitialApproximation##
    right## = secondInitialApproximation##
    leftVal## = f##(left##)
    rightVal## = f##(right##)

    If FORCE_IVT% = 1 And isBw%(0, leftVal##, rightVal##) = 0 Then
        pl "Secant -> FATAL: IVT violated due to bad initial approximation  x0: " + s$(left##) + ",  x1: " + s$(right##)
        secant## = -1 ' Invalid initial approximations
    Else
        mid## = -1
        midVal## = -1
        itr% = 1

        Do
            mid## = ((right## * leftVal##) - (left## * rightVal##)) / (leftVal## - rightVal##)
            midVal## = f##(mid##)
            pl "Secant Iteration " + s$(itr%) + " -> Root: " + s$(mid##) + ",  Root Value: " + s$(midVal##)

            If Abs(midVal##) <= tolerance## Then
                pl "Secant -> Root Found at iteration " + s$(itr%)
                Exit Do
            End If

            left## = right##
            right## = mid##

            itr% = itr% + 1
        Loop While itr% <= maxItrs%

        secant## = mid##
    End If
End Function

Function raphson## (initialApproximation##, maxItrs%, tolerance##)
    cur## = initialApproximation##
    curVal## = -1
    itr% = 0

    Do
        curVal## = f##(cur##)
        pl "Raphson Iteration " + s$(itr%) + " -> Root: " + s$(cur##) + ",  Root Value: " + s$(curVal##)

        If Abs(curVal##) <= tolerance## Then
            pl "Raphson -> Root Found at iteration " + s$(itr%)
            Exit Do
        End If

        derVal## = numericalDerivativeDef##(cur##)
        If derVal## = 0 Then
            pl "Raphson -> FATAL: Derivative is 0 at current root. Terminating..."
            Exit Do
        End If

        cur## = cur## - (curVal## / derVal##)
        itr% = itr% + 1
    Loop While itr% <= maxItrs%

    raphson## = cur##
End Function

' ........................... Formatting .......................
Sub p (st$) ' Print and log a given string WITHOUT line break
    Print st$;
    Print #1, st$;
End Sub

Sub lb ' Print and log a line break
    Print
    Print #1, ""
End Sub

Sub pl (st$) ' Print and log given string WITH line break
    Print st$
    Print #1, st$
End Sub

Function s$ (i##)
    s$ = LTrim$(RTrim$(Str$(i##)))
End Function

'............. INPUT ............
Function in_str$ (caption$)
    p (caption$)
    Input "", v$
    Print #1, v$
    in_str$ = v$
End Function

Function in_int% (caption$)
    p (caption$)
    Input "", v%
    Print #1, s$(v%)
    in_int% = v%
End Function

Function in_float64## (caption$)
    p (caption$)
    Input "", v##
    Print #1, s$(v##)
    in_float64## = v##
End Function

