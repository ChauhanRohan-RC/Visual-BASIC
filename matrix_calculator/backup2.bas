Open "OUTPUT.txt" For Output As #1

Cls
Screen 12

lb
pl "=================== MATRIX CALCULATOR (Made by RC) ========================="
lb


Dim Shared matAName$
matAName$ = "Matrix-A"

Dim Shared matBName$
matBName$ = "Matrix-B"


'MATRIX A

input_mat_a:
pl "................. " + matAName$ + " ..............."
AR% = in_int%("ROWS: ")
AC% = in_int%("COLS: ")

If AR% < 1 Or AC% < 1 Then
    pl "Matrix Rows And Columns must be > 0"
    lb
    GoTo input_mat_a
End If


Dim matA#(arr_len%(AR%, AC%))

Call input_matrix(matAName$, matA#(), AR%, AC%)
Call print_matrix(matAName$, matA#(), AR%, AC%)


commands_info:
lb
pl "................... COMMANDS ................."
pl "## Unitary Operations"
pl " 1 -> Add Scaler"
pl " 2 -> Multipliy Scaler"
pl " 3 -> Determinant"
pl " 4 -> Transpose"
pl " 5 -> Minor Matrix"
pl " 6 -> Cofactor Matrix"
pl " 7 -> Adjoint Matrix"
pl " 8 -> Inverse Matrix"
pl " 9 -> Power Matrix"
lb
pl "## Binary Operations"
pl " 10 -> Add Matrices (with scale)"
pl " 11 -> Multipliy Matrices (with scale)"
lb


enter_command:
lb
command% = in_int%("ENTER COMMAND: ")
lb
If command% = 1 Then
    Call main_add_scaler(matA#(), AR%, AC%)
ElseIf command% = 2 Then
    Call main_mult_scaler(matA#(), AR%, AC%)
ElseIf command% = 3 Then
    Call main_determinant(matA#(), AR%, AC%)
ElseIf command% = 4 Then
    Call main_transpose(matA#(), AR%, AC%)
ElseIf command% = 5 Then
    Call main_minor_matrix(matA#(), AR%, AC%)
ElseIf command% = 6 Then
    Call main_cofactor_matrix(matA#(), AR%, AC%)
ElseIf command% = 7 Then
    Call main_adjoint_matrix(matA#(), AR%, AC%)
ElseIf command% = 8 Then
    Call main_invert_matrix(matA#(), AR%, AC%)
ElseIf command% = 9 Then
    Call main_pow_matrix(matA#(), AR%, AC%)
ElseIf command% = 10 Then
    Call main_add_matrices(matA#(), AR%, AC%)
ElseIf command% = 11 Then
    Call main_mult_matrices(matA#(), AR%, AC%)
Else
    pl "Invalid Command: " + trim_val$(command%)
End If

GoTo enter_command

Sub main_add_scaler (mat#(), rows%, cols%)
    add# = in_doub#("Enter Scaler Addant: ")

    Dim res#(arr_len%(rows%, cols%))
    Call mat_add_scaler(mat#(), res#(), add#, rows%, cols%)
    Call print_matrix("[Result] = " + trim_val$(add#) + " + [" + matAName$ + "]", res#(), rows%, cols%)
End Sub

Sub main_mult_scaler (mat#(), rows%, cols%)
    mult# = in_doub#("Enter Scaler Multiplier: ")

    Dim res#(arr_len%(rows%, cols%))
    Call mat_mult_scaler(mat#(), res#(), mult#, rows%, cols%)
    Call print_matrix("[Result] = " + trim_val$(mult#) + " * [" + matAName$ + "]", res#(), rows%, cols%)
End Sub


Sub main_determinant (mat#(), rows%, cols%)
    If rows% <> cols% Then
        pl "ERR: Determinant is only defined for a square matrix, given matrix order: " + format_ij$(rows%, cols%, "x")
    Else
        det# = determinant#(mat#(), rows%)
        pl "DETERMINANT([" + matAName$ + "]): " + trim_val$(det#)
    End If
End Sub


Sub main_transpose (mat#(), rows%, cols%)
    Dim res#(arr_len%(cols%, rows%))
    Call transpose(mat#(), res#(), rows%, cols%)

    Call print_matrix("[Result] = TRANSPOSE([" + matAName$ + "])", res#(), cols%, rows%)
End Sub

Sub main_minor_matrix (mat#(), rows%, cols%)
    If rows% <> cols% Then
        pl "ERR: Minor matrix is only defined for a square matrix, given matrix order: " + format_ij$(rows%, cols%, "x")
    Else
        Dim res#(arr_len%(rows%, cols%))
        Call minor_matrix(mat#(), res#(), rows%)
        Call print_matrix("[Result] = MINOR([" + matAName$ + "])", res#(), rows%, cols%)
    End If
End Sub

Sub main_cofactor_matrix (mat#(), rows%, cols%)
    If rows% <> cols% Then
        pl "ERR: Cofactor matrix is only defined for a square matrix, given matrix order: " + format_ij$(rows%, cols%, "x")
    Else
        Dim res#(arr_len%(rows%, cols%))
        Call cofactor_matrix(mat#(), res#(), rows%)
        Call print_matrix("[Result] = COFACTOR([" + matAName$ + "])", res#(), rows%, cols%)
    End If
End Sub

Sub main_adjoint_matrix (mat#(), rows%, cols%)
    If rows% <> cols% Then
        pl "ERR: Adjoint matrix is only defined for a square matrix, given matrix order: " + format_ij$(rows%, cols%, "x")
    Else
        Dim res#(arr_len%(rows%, cols%))
        Call adjoint_matrix(mat#(), res#(), rows%)
        Call print_matrix("[Result] = ADJOINT([" + matAName$ + "])", res#(), rows%, cols%)
    End If
End Sub

Sub main_invert_matrix (mat#(), rows%, cols%)
    If rows% <> cols% Then
        pl "ERR: Inverse matrix is only defined for a square matrix, given matrix order: " + format_ij$(rows%, cols%, "x")
    Else
        det# = determinant#(mat#(), rows%)
        If det# = 0 Then
            pl "ERR: " + matAName$ + " is not invertible since it is SINGULAR (det = 0)"
        Else
            Dim res#(arr_len%(rows%, cols%))
            Call adjoint_matrix(mat#(), res#(), rows%)
            Call mat_mult_scaler(res#(), res#(), 1 / det#, rows%, cols%)
            Call print_matrix("[Result] = INVERSE([" + matAName$ + "])", res#(), rows%, cols%)
        End If
    End If

End Sub


Sub main_pow_matrix (mat#(), rows%, cols%)
    If rows% <> cols% Then
        pl "ERR: Power matrix is only defined for a square matrix, given matrix order: " + format_ij$(rows%, cols%, "x")
    Else
        pow% = in_int%("Enter the power to raise matrix to: ")
        pow_copy% = pow%
        Dim res#(arr_len%(rows%, cols%))
        Call pow_mat(mat#(), res#(), pow_copy%, rows%)
        Call print_matrix("[Result] = [" + matAName$ + "] ^ " + trim_val$(pow%), res#(), rows%, cols%)
    End If
End Sub


Sub main_add_matrices (mat#(), rows%, cols%)
    Dim matB#(arr_len%(rows%, cols%))

    Call input_matrix(matBName$, matB#(), rows%, cols%)
    Call print_matrix(matBName$, matB#(), rows%, cols%)

    lb
    a_scale# = in_doub#("Enter " + matAName$ + " entries scale: ")
    b_scale# = in_doub#("Enter " + matBName$ + " entries scale: ")

    Dim res#(arr_len%(rows%, cols%))
    Call add_mat_with_scale(mat#(), matB#(), res#(), rows%, cols%, a_scale#, b_scale#)
    Call print_matrix("[Result] = (" + trim_val$(a_scale#) + " * [" + matAName$ + "]) + (" + trim_val$(b_scale#) + " * [" + matBName$ + "])", res#(), rows%, cols%)

End Sub


Sub main_mult_matrices (mat#(), rows%, cols%)
    b_cols% = in_int%("Enter " + matBName$ + " COLS: ")

    Dim matB#(arr_len%(cols%, b_cols%))

    Call input_matrix(matBName$, matB#(), cols%, b_cols%)
    Call print_matrix(matBName$, matB#(), cols%, b_cols%)
    lb

    a_scale# = in_doub#("Enter " + matAName$ + " entries scale: ")
    b_scale# = in_doub#("Enter " + matBName$ + " entries scale: ")

    Dim res#(arr_len%(rows%, b_cols%))
    Call mult_mat_with_scale(mat#(), matB#(), res#(), rows%, cols%, b_cols%, a_scale#, b_scale#)
    Call print_matrix("[Result] = (" + trim_val$(a_scale#) + " * [" + matAName$ + "]) X (" + trim_val$(b_scale#) + " * [" + matBName$ + "])", res#(), rows%, b_cols%)

End Sub




' Matrix representation as 1D array Utility
' Mostly depend on column count of the matrix

Function is_even (num%, even_res, odd_res)
    If (num% Mod 2) = 0 Then
        is_even = even_res
    Else
        is_even = odd_res
    End If
End Function

Function is_even11% (num%)
    is_even11% = is_even(num%, 1, -1)
End Function


Function arr_len% (rows%, cols%)
    arr_len% = rows% * cols%
End Function

Function arr_index% (i%, j%, cols%)
    arr_index% = ((i% - 1) * cols%) + j%
End Function


Function mat_i% (arr_ind%, cols%)
    temp% = (arr_ind% - 1) \ cols%
    mat_i% = temp% + 1
End Function

Function mat_j% (arr_ind%, cols%)
    temp% = (arr_ind% - 1) Mod cols%
    mat_j% = temp% + 1
End Function

Sub mat_ij (arr_ind%, cols%, ij())
    ij(1) = mat_i%(arr_ind%, cols%)
    ij(2) = mat_j%(arr_ind%, cols%)
End Sub

Function get_ij# (arr#(), i%, j%, cols%)
    get_ij# = arr#(arr_index%(i%, j%, cols%))
End Function




' ....................... Matrix Unitary Transformations ............................
' CAUTION: To overwrite source, pass ource as Result Array/Matrix (NOT RECOMMENDED)

Sub fill_arr (arr#(), num#, length%)
    For i% = 1 To length%
        arr#(i%) = num#
    Next i%
End Sub

Sub fill_mat (mat#(), num#, rows%, cols%)
    Call fill_arr(mat#(), num#, arr_len%(rows%, cols%))
End Sub

Sub copy_arr (src#(), dest#(), length%)
    For i% = 1 To length%
        dest#(i%) = src#(i%)
    Next i%
End Sub


Sub copy_mat (src#(), dest#(), rows%, cols%)
    Call copy_arr(src#(), dest#(), arr_len%(rows%, cols%))
End Sub



Sub arr_add_scaler (arr#(), result#(), addant#, length%)
    For i% = 1 To length%
        result#(i%) = arr#(i%) + addant#
    Next i%
End Sub

Sub arr_mult_scaler (arr#(), result#(), multiplier#, length%)
    For i% = 1 To length%
        result#(i%) = arr#(i%) * multiplier#
    Next i%
End Sub

Sub mat_add_scaler (mat#(), result#(), addant#, rows%, cols%)
    Call arr_add_scaler(mat#(), result#(), addant#, arr_len%(rows%, cols%))
End Sub

Sub mat_mult_scaler (mat#(), result#(), multiplier#, rows%, cols%)
    Call arr_mult_scaler(mat#(), result#(), multiplier#, arr_len%(rows%, cols%))
End Sub

Sub transpose (mat#(), result#(), src_rows%, src_cols%)
    For i% = 1 To src_rows%
        For j% = 1 To src_cols%
            result#(arr_index%(j%, i%, src_rows%)) = mat#(arr_index%(i%, j%, src_cols%))
        Next j%
    Next i%
End Sub

' Result matrix must be atleast of len = (rows - 1) * (cols - 1)
Sub delete_row_col (mat#(), result#(), row%, col%, rows%, cols%)
    cur_i% = 1
    For i% = 1 To rows%
        If i% <> row% Then
            For j% = 1 To cols%
                If j% <> col% Then
                    result#(cur_i%) = mat#(arr_index%(i%, j%, cols%))
                    cur_i% = cur_i% + 1
                End If
            Next j%
        End If
    Next i%
End Sub


' SQUARE MATRIX

Function determinant# (mat#(), size%)
    If size% = 1 Then
        determinant# = mat#(1)
    ElseIf size% = 2 Then
        determinant# = (mat#(1) * mat#(4)) - (mat#(2) * mat#(3))
    Else
        sum# = 0

        Dim temp#(arr_len%(size% - 1, size% - 1))
        For i% = 1 To size%
            Call delete_row_col(mat#(), temp#(), 1, i%, size%, size%)
            sum# = sum# + (get_ij#(mat#(), 1, i%, size%) * is_even11%(1 + i%) * determinant#(temp#(), size% - 1))
        Next i%
        determinant# = sum#
    End If

End Function

Function minor# (mat#(), i%, j%, size%)
    Dim temp#(arr_len%(size% - 1, size% - 1))
    Call delete_row_col(mat#(), temp#(), i%, j%, size%, size%)
    minor# = determinant#(temp#(), size% - 1)
End Function

Function cofactor# (mat#(), i%, j%, size%)
    cofactor# = is_even11%(i% + j%) * minor#(mat#(), i%, j%, size%)
End Function


Function is_singular# (mat#(), size%, true_val#, false_val#)
    det# = determinant#(mat#(), size%)
    If det# = 0 Then
        is_singular# = true_val#
    Else
        is_singular# = false_val#
    End If
End Function

Function is_singular10% (mat#(), size%)
    is_singular10% = is_singular#(mat#(), size%, 1, 0)
End Function


Sub minor_matrix_internal (mat#(), result#(), size%, do_cofactor%, do_transpose%)
    For i% = 1 To size%
        For j% = 1 To size%
            If do_transpose% = 1 Then
                ind% = arr_index%(j%, i%, size%)
            Else
                ind% = arr_index%(i%, j%, size%)
            End If

            If do_cofactor% = 1 Then
                result#(ind%) = cofactor#(mat#(), i%, j%, size%)
            Else
                result#(ind%) = minor#(mat#(), i%, j%, size%)
            End If

        Next j%
    Next i%
End Sub

Sub minor_matrix (mat#(), result#(), size%)
    Call minor_matrix_internal(mat#(), result#(), size%, 0, 0)
End Sub

Sub cofactor_matrix (mat#(), result#(), size%)
    Call minor_matrix_internal(mat#(), result#(), size%, 1, 0)
End Sub

Sub adjoint_matrix (mat#(), result#(), size%)
    Call minor_matrix_internal(mat#(), result#(), size%, 1, 1)
End Sub


' Matrix should be invertible (i.e det is non zero)

Sub invert_matrix (mat#(), result#(), size%)
    Call adjoint_matrix(mat#(), result#(), size%)
    det# = determinant#(mat#(), size%) ' TODO: caheck non-singular

    Call mat_mult_scaler(result#(), result#(), 1 / det#, size%, size%)
End Sub




'............................. Matrix Binary Transformations .............................


Sub add_arr_with_scale (arr_a#(), arr_b#(), result#(), length%, scale_a#, scale_b#)
    For i% = 0 To length%
        result#(i%) = (scale_a# * arr_a#(i%)) + (scale_b# * arr_b#(i%))
    Next i%
End Sub

Sub add_arr (arr_a#(), arr_b#(), result#(), length%)
    Call add_arr_with_scale(arr_a#(), arr_b#(), result#(), length%, 1, 1)
End Sub

Sub subtract_arr (arr_a#(), arr_b#(), result#(), length%)
    Call add_arr_with_scale(arr_a#(), arr_b#(), result#(), length%, 1, -1)
End Sub


Sub add_mat_with_scale (mat_a#(), mat_b#(), result#(), rows%, cols%, scale_a#, scale_b#)
    Call add_arr_with_scale(mat_a#(), mat_b#(), result#(), arr_len%(rows%, cols%), scale_a#, scale_b#)
End Sub

Sub add_mat (mat_a#(), mat_b#(), result#(), rows%, cols%)
    Call add_mat_with_scale(mat_a#(), mat_b#(), result#(), rows%, cols%, 1, 1)
End Sub

Sub subtract_mat (mat_a#(), mat_b#(), result#(), rows%, cols%)
    Call add_mat_with_scale(mat_a#(), mat_b#(), result#(), rows%, cols%, 1, -1)
End Sub


' Result matrix is of order  (rows_a% * cols_b% )
Sub mult_mat_with_scale (mat_a#(), mat_b#(), result#(), rows_a%, cols_a%, cols_b%, scale_a#, scale_b#)
    For i% = 1 To rows_a%
        For j% = 1 To cols_b%
            e# = 0
            For k% = 1 To cols_a%
                e# = e# + ((scale_a# * mat_a#(arr_index%(i%, k%, cols_a%))) * (scale_b# * mat_b#(arr_index%(k%, j%, cols_b%))))
            Next k%
            result#(arr_index%(i%, j%, cols_b%)) = e#
        Next j%
    Next i%
End Sub


Sub mult_mat (mat_a#(), mat_b#(), result#(), rows_a%, cols_a%, cols_b%)
    Call mult_mat_with_scale(mat_a#(), mat_b#(), result#(), rows_a%, cols_a%, cols_b%, 1, 1)
End Sub


Sub pow_mat (mat#(), result#(), pow%, size%)
    If pow% = 0 Then
        Call fill_mat(result#(), 1, size%, size%)
    Else
        If pow% < 0 Then
            Dim inv#(arr_len%(size%, size%))
            Call invert_matrix(mat#(), inv#(), size%)
            Call copy_mat(inv#(), mat#(), size%, size%)
            pow% = Int(Abs(pow%))
        End If

        If pow% = 1 Then
            Call copy_mat(mat#(), result#(), size%, size%)
        Else
            Dim temp#(arr_len%(size%, size%))
            Call copy_mat(mat#(), temp#(), size%, size%)

            For i% = 2 To pow%
                Call mult_mat(mat#(), temp#(), result#(), size%, size%, size%)
                Call copy_mat(result#(), temp#(), size%, size%)
            Next i%
        End If

    End If

End Sub



' .................................. Formatting ...............................

' Prints a given string WITHOUT line break
Sub p (s$)
    Print s$;
    Print #1, s$;
End Sub

' Prints a line break
Sub lb
    Print
    Print #1, ""
End Sub

' Prints given string WITH line break
Sub pl (s$)
    Print s$
    Print #1, s$
End Sub


Function trim_str$ (s$)
    trim_str$ = LTrim$(RTrim$(s$))
End Function

Function trim_val$ (i#)
    trim_val$ = trim_str$(Str$(i#))
End Function

Function format_ij$ (i%, j%, delimiter$)
    format_ij$ = trim_str$("(" + trim_val$(i%) + delimiter$ + trim_val$(j%) + ")")
End Function


Sub print_matrix (matrix_name$, matrix_arr#(), rows%, cols%)
    lb
    pl "............. " + matrix_name$ + " ................"
    lb
    For i% = 1 To rows%
        For j% = 1 To cols%
            p trim_val$(get_ij#(matrix_arr#(), i%, j%, cols%)) + "  "
        Next j%
        lb
    Next i%
    lb
End Sub


'....................................... INPUT .............................................

Function in_str$ (caption$)
    p (caption$)
    Input "", v$
    Print #1, v$;
    in_str$ = v$
End Function


Function in_int% (caption$)
    p (caption$)
    Input "", v%
    Print #1, trim_val$(v%)
    in_int% = v%
End Function

Function in_float! (caption$)
    p (caption$)
    Input "", v!
    Print #1, trim_val$(v!)
    in_float! = v!
End Function

Function in_doub# (caption$)
    p (caption$)
    Input "", v#
    Print #1, trim_val$(v#)
    in_doub# = v#
End Function



Sub input_matrix (matrix_name$, matrix_arr#(), rows%, cols%)
    lb
    pl "............. Input " + matrix_name$ + " " + format_ij$(rows%, cols%, "x") + " ................"
    lb
    For i% = 1 To rows%
        pl "# ROW " + Str$(i%)
        For j% = 1 To cols%
            e# = in_doub#(matrix_name$ + " " + format_ij$(i%, j%, ",") + " : ")
            matrix_arr#(arr_index%(i%, j%, cols%)) = e#
        Next j%
        lb
    Next i%
    lb
End Sub


Sub read_array (arr#(), length%)
    For i% = 1 To length%
        Read e#
        arr#(i%) = e#
    Next i%
End Sub

Sub read_matrix (mat#(), rows%, cols%)
    Call read_array(mat#(), arr_len%(rows%, cols%))
End Sub
