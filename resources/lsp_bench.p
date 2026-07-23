/* Synthetic LSP benchmark fixture (generated).

   Sized to approximate the pipeline cost of a large real-world ABL editing
   buffer (the real file is kept outside the repo). Pulls shared definitions
   and utilities from includes, then exercises preprocess -> tokenize -> parse
   -> semantic -> lint over many procedures. All content is synthetic. */

&scoped-define MODULE order-entry
&scoped-define MAX-LINES 500

{lsp_bench_defs.i}
{lsp_bench_utils.i}

define variable gv-count as integer no-undo.
define variable gv-acc   as decimal no-undo.
define variable gv-msg   as character no-undo.

procedure process-batch-001:
    define input  parameter pnum   as integer no-undo.
    define output parameter ptotal as decimal no-undo.
    define variable li  as integer no-undo.
    define variable lj  as integer no-undo.
    define variable lsub as decimal no-undo.
    define variable ltax as decimal no-undo.
    define variable lstat as character no-undo.

    assign ptotal = 0.
    do li = 1 to pnum:
        create tt-order.
        assign
            tt-order.order-num   = li + 1 * 1000
            tt-order.cust-num    = li modulo 25 + 1
            tt-order.order-date  = today
            tt-order.order-status      = "O"
            tt-order.order-total = 0.
        assign lsub = 0.
        do lj = 1 to 6:
            create tt-line.
            assign
                tt-line.order-num = tt-order.order-num
                tt-line.line-num  = lj
                tt-line.item-num  = lj * 100 + li
                tt-line.qty       = lj + 1 modulo 5
                tt-line.price     = lj * 2.5 + 1
                tt-line.extended  = calc-extended(tt-line.qty, tt-line.price).
            assign lsub = lsub + tt-line.extended.
        end.
        assign
            ltax  = calc-tax(lsub, gv-tax-rate)
            lstat = format-status(tt-order.order-status)
            tt-order.order-total = lsub + ltax
            ptotal = ptotal + tt-order.order-total.
        if gv-verbose then
            message "Batch 1 order" tt-order.order-num "total" tt-order.order-total lstat.
    end.
end procedure.

procedure process-batch-002:
    define input  parameter pnum   as integer no-undo.
    define output parameter ptotal as decimal no-undo.
    define variable li  as integer no-undo.
    define variable lj  as integer no-undo.
    define variable lsub as decimal no-undo.
    define variable ltax as decimal no-undo.
    define variable lstat as character no-undo.

    assign ptotal = 0.
    do li = 1 to pnum:
        create tt-order.
        assign
            tt-order.order-num   = li + 2 * 1000
            tt-order.cust-num    = li modulo 25 + 1
            tt-order.order-date  = today
            tt-order.order-status      = "O"
            tt-order.order-total = 0.
        assign lsub = 0.
        do lj = 1 to 6:
            create tt-line.
            assign
                tt-line.order-num = tt-order.order-num
                tt-line.line-num  = lj
                tt-line.item-num  = lj * 100 + li
                tt-line.qty       = lj + 2 modulo 5
                tt-line.price     = lj * 2.5 + 2
                tt-line.extended  = calc-extended(tt-line.qty, tt-line.price).
            assign lsub = lsub + tt-line.extended.
        end.
        assign
            ltax  = calc-tax(lsub, gv-tax-rate)
            lstat = format-status(tt-order.order-status)
            tt-order.order-total = lsub + ltax
            ptotal = ptotal + tt-order.order-total.
        if gv-verbose then
            message "Batch 2 order" tt-order.order-num "total" tt-order.order-total lstat.
    end.
end procedure.

procedure process-batch-003:
    define input  parameter pnum   as integer no-undo.
    define output parameter ptotal as decimal no-undo.
    define variable li  as integer no-undo.
    define variable lj  as integer no-undo.
    define variable lsub as decimal no-undo.
    define variable ltax as decimal no-undo.
    define variable lstat as character no-undo.

    assign ptotal = 0.
    do li = 1 to pnum:
        create tt-order.
        assign
            tt-order.order-num   = li + 3 * 1000
            tt-order.cust-num    = li modulo 25 + 1
            tt-order.order-date  = today
            tt-order.order-status      = "O"
            tt-order.order-total = 0.
        assign lsub = 0.
        do lj = 1 to 6:
            create tt-line.
            assign
                tt-line.order-num = tt-order.order-num
                tt-line.line-num  = lj
                tt-line.item-num  = lj * 100 + li
                tt-line.qty       = lj + 3 modulo 5
                tt-line.price     = lj * 2.5 + 3
                tt-line.extended  = calc-extended(tt-line.qty, tt-line.price).
            assign lsub = lsub + tt-line.extended.
        end.
        assign
            ltax  = calc-tax(lsub, gv-tax-rate)
            lstat = format-status(tt-order.order-status)
            tt-order.order-total = lsub + ltax
            ptotal = ptotal + tt-order.order-total.
        if gv-verbose then
            message "Batch 3 order" tt-order.order-num "total" tt-order.order-total lstat.
    end.
end procedure.

procedure process-batch-004:
    define input  parameter pnum   as integer no-undo.
    define output parameter ptotal as decimal no-undo.
    define variable li  as integer no-undo.
    define variable lj  as integer no-undo.
    define variable lsub as decimal no-undo.
    define variable ltax as decimal no-undo.
    define variable lstat as character no-undo.

    assign ptotal = 0.
    do li = 1 to pnum:
        create tt-order.
        assign
            tt-order.order-num   = li + 4 * 1000
            tt-order.cust-num    = li modulo 25 + 1
            tt-order.order-date  = today
            tt-order.order-status      = "O"
            tt-order.order-total = 0.
        assign lsub = 0.
        do lj = 1 to 6:
            create tt-line.
            assign
                tt-line.order-num = tt-order.order-num
                tt-line.line-num  = lj
                tt-line.item-num  = lj * 100 + li
                tt-line.qty       = lj + 4 modulo 5
                tt-line.price     = lj * 2.5 + 4
                tt-line.extended  = calc-extended(tt-line.qty, tt-line.price).
            assign lsub = lsub + tt-line.extended.
        end.
        assign
            ltax  = calc-tax(lsub, gv-tax-rate)
            lstat = format-status(tt-order.order-status)
            tt-order.order-total = lsub + ltax
            ptotal = ptotal + tt-order.order-total.
        if gv-verbose then
            message "Batch 4 order" tt-order.order-num "total" tt-order.order-total lstat.
    end.
end procedure.

procedure process-batch-005:
    define input  parameter pnum   as integer no-undo.
    define output parameter ptotal as decimal no-undo.
    define variable li  as integer no-undo.
    define variable lj  as integer no-undo.
    define variable lsub as decimal no-undo.
    define variable ltax as decimal no-undo.
    define variable lstat as character no-undo.

    assign ptotal = 0.
    do li = 1 to pnum:
        create tt-order.
        assign
            tt-order.order-num   = li + 5 * 1000
            tt-order.cust-num    = li modulo 25 + 1
            tt-order.order-date  = today
            tt-order.order-status      = "O"
            tt-order.order-total = 0.
        assign lsub = 0.
        do lj = 1 to 6:
            create tt-line.
            assign
                tt-line.order-num = tt-order.order-num
                tt-line.line-num  = lj
                tt-line.item-num  = lj * 100 + li
                tt-line.qty       = lj + 5 modulo 5
                tt-line.price     = lj * 2.5 + 5
                tt-line.extended  = calc-extended(tt-line.qty, tt-line.price).
            assign lsub = lsub + tt-line.extended.
        end.
        assign
            ltax  = calc-tax(lsub, gv-tax-rate)
            lstat = format-status(tt-order.order-status)
            tt-order.order-total = lsub + ltax
            ptotal = ptotal + tt-order.order-total.
        if gv-verbose then
            message "Batch 5 order" tt-order.order-num "total" tt-order.order-total lstat.
    end.
end procedure.

procedure process-batch-006:
    define input  parameter pnum   as integer no-undo.
    define output parameter ptotal as decimal no-undo.
    define variable li  as integer no-undo.
    define variable lj  as integer no-undo.
    define variable lsub as decimal no-undo.
    define variable ltax as decimal no-undo.
    define variable lstat as character no-undo.

    assign ptotal = 0.
    do li = 1 to pnum:
        create tt-order.
        assign
            tt-order.order-num   = li + 6 * 1000
            tt-order.cust-num    = li modulo 25 + 1
            tt-order.order-date  = today
            tt-order.order-status      = "O"
            tt-order.order-total = 0.
        assign lsub = 0.
        do lj = 1 to 6:
            create tt-line.
            assign
                tt-line.order-num = tt-order.order-num
                tt-line.line-num  = lj
                tt-line.item-num  = lj * 100 + li
                tt-line.qty       = lj + 6 modulo 5
                tt-line.price     = lj * 2.5 + 6
                tt-line.extended  = calc-extended(tt-line.qty, tt-line.price).
            assign lsub = lsub + tt-line.extended.
        end.
        assign
            ltax  = calc-tax(lsub, gv-tax-rate)
            lstat = format-status(tt-order.order-status)
            tt-order.order-total = lsub + ltax
            ptotal = ptotal + tt-order.order-total.
        if gv-verbose then
            message "Batch 6 order" tt-order.order-num "total" tt-order.order-total lstat.
    end.
end procedure.

procedure process-batch-007:
    define input  parameter pnum   as integer no-undo.
    define output parameter ptotal as decimal no-undo.
    define variable li  as integer no-undo.
    define variable lj  as integer no-undo.
    define variable lsub as decimal no-undo.
    define variable ltax as decimal no-undo.
    define variable lstat as character no-undo.

    assign ptotal = 0.
    do li = 1 to pnum:
        create tt-order.
        assign
            tt-order.order-num   = li + 7 * 1000
            tt-order.cust-num    = li modulo 25 + 1
            tt-order.order-date  = today
            tt-order.order-status      = "O"
            tt-order.order-total = 0.
        assign lsub = 0.
        do lj = 1 to 6:
            create tt-line.
            assign
                tt-line.order-num = tt-order.order-num
                tt-line.line-num  = lj
                tt-line.item-num  = lj * 100 + li
                tt-line.qty       = lj + 7 modulo 5
                tt-line.price     = lj * 2.5 + 7
                tt-line.extended  = calc-extended(tt-line.qty, tt-line.price).
            assign lsub = lsub + tt-line.extended.
        end.
        assign
            ltax  = calc-tax(lsub, gv-tax-rate)
            lstat = format-status(tt-order.order-status)
            tt-order.order-total = lsub + ltax
            ptotal = ptotal + tt-order.order-total.
        if gv-verbose then
            message "Batch 7 order" tt-order.order-num "total" tt-order.order-total lstat.
    end.
end procedure.

procedure process-batch-008:
    define input  parameter pnum   as integer no-undo.
    define output parameter ptotal as decimal no-undo.
    define variable li  as integer no-undo.
    define variable lj  as integer no-undo.
    define variable lsub as decimal no-undo.
    define variable ltax as decimal no-undo.
    define variable lstat as character no-undo.

    assign ptotal = 0.
    do li = 1 to pnum:
        create tt-order.
        assign
            tt-order.order-num   = li + 8 * 1000
            tt-order.cust-num    = li modulo 25 + 1
            tt-order.order-date  = today
            tt-order.order-status      = "O"
            tt-order.order-total = 0.
        assign lsub = 0.
        do lj = 1 to 6:
            create tt-line.
            assign
                tt-line.order-num = tt-order.order-num
                tt-line.line-num  = lj
                tt-line.item-num  = lj * 100 + li
                tt-line.qty       = lj + 8 modulo 5
                tt-line.price     = lj * 2.5 + 8
                tt-line.extended  = calc-extended(tt-line.qty, tt-line.price).
            assign lsub = lsub + tt-line.extended.
        end.
        assign
            ltax  = calc-tax(lsub, gv-tax-rate)
            lstat = format-status(tt-order.order-status)
            tt-order.order-total = lsub + ltax
            ptotal = ptotal + tt-order.order-total.
        if gv-verbose then
            message "Batch 8 order" tt-order.order-num "total" tt-order.order-total lstat.
    end.
end procedure.

procedure process-batch-009:
    define input  parameter pnum   as integer no-undo.
    define output parameter ptotal as decimal no-undo.
    define variable li  as integer no-undo.
    define variable lj  as integer no-undo.
    define variable lsub as decimal no-undo.
    define variable ltax as decimal no-undo.
    define variable lstat as character no-undo.

    assign ptotal = 0.
    do li = 1 to pnum:
        create tt-order.
        assign
            tt-order.order-num   = li + 9 * 1000
            tt-order.cust-num    = li modulo 25 + 1
            tt-order.order-date  = today
            tt-order.order-status      = "O"
            tt-order.order-total = 0.
        assign lsub = 0.
        do lj = 1 to 6:
            create tt-line.
            assign
                tt-line.order-num = tt-order.order-num
                tt-line.line-num  = lj
                tt-line.item-num  = lj * 100 + li
                tt-line.qty       = lj + 9 modulo 5
                tt-line.price     = lj * 2.5 + 9
                tt-line.extended  = calc-extended(tt-line.qty, tt-line.price).
            assign lsub = lsub + tt-line.extended.
        end.
        assign
            ltax  = calc-tax(lsub, gv-tax-rate)
            lstat = format-status(tt-order.order-status)
            tt-order.order-total = lsub + ltax
            ptotal = ptotal + tt-order.order-total.
        if gv-verbose then
            message "Batch 9 order" tt-order.order-num "total" tt-order.order-total lstat.
    end.
end procedure.

procedure process-batch-010:
    define input  parameter pnum   as integer no-undo.
    define output parameter ptotal as decimal no-undo.
    define variable li  as integer no-undo.
    define variable lj  as integer no-undo.
    define variable lsub as decimal no-undo.
    define variable ltax as decimal no-undo.
    define variable lstat as character no-undo.

    assign ptotal = 0.
    do li = 1 to pnum:
        create tt-order.
        assign
            tt-order.order-num   = li + 10 * 1000
            tt-order.cust-num    = li modulo 25 + 1
            tt-order.order-date  = today
            tt-order.order-status      = "O"
            tt-order.order-total = 0.
        assign lsub = 0.
        do lj = 1 to 6:
            create tt-line.
            assign
                tt-line.order-num = tt-order.order-num
                tt-line.line-num  = lj
                tt-line.item-num  = lj * 100 + li
                tt-line.qty       = lj + 10 modulo 5
                tt-line.price     = lj * 2.5 + 10
                tt-line.extended  = calc-extended(tt-line.qty, tt-line.price).
            assign lsub = lsub + tt-line.extended.
        end.
        assign
            ltax  = calc-tax(lsub, gv-tax-rate)
            lstat = format-status(tt-order.order-status)
            tt-order.order-total = lsub + ltax
            ptotal = ptotal + tt-order.order-total.
        if gv-verbose then
            message "Batch 10 order" tt-order.order-num "total" tt-order.order-total lstat.
    end.
end procedure.

procedure process-batch-011:
    define input  parameter pnum   as integer no-undo.
    define output parameter ptotal as decimal no-undo.
    define variable li  as integer no-undo.
    define variable lj  as integer no-undo.
    define variable lsub as decimal no-undo.
    define variable ltax as decimal no-undo.
    define variable lstat as character no-undo.

    assign ptotal = 0.
    do li = 1 to pnum:
        create tt-order.
        assign
            tt-order.order-num   = li + 11 * 1000
            tt-order.cust-num    = li modulo 25 + 1
            tt-order.order-date  = today
            tt-order.order-status      = "O"
            tt-order.order-total = 0.
        assign lsub = 0.
        do lj = 1 to 6:
            create tt-line.
            assign
                tt-line.order-num = tt-order.order-num
                tt-line.line-num  = lj
                tt-line.item-num  = lj * 100 + li
                tt-line.qty       = lj + 11 modulo 5
                tt-line.price     = lj * 2.5 + 11
                tt-line.extended  = calc-extended(tt-line.qty, tt-line.price).
            assign lsub = lsub + tt-line.extended.
        end.
        assign
            ltax  = calc-tax(lsub, gv-tax-rate)
            lstat = format-status(tt-order.order-status)
            tt-order.order-total = lsub + ltax
            ptotal = ptotal + tt-order.order-total.
        if gv-verbose then
            message "Batch 11 order" tt-order.order-num "total" tt-order.order-total lstat.
    end.
end procedure.

procedure process-batch-012:
    define input  parameter pnum   as integer no-undo.
    define output parameter ptotal as decimal no-undo.
    define variable li  as integer no-undo.
    define variable lj  as integer no-undo.
    define variable lsub as decimal no-undo.
    define variable ltax as decimal no-undo.
    define variable lstat as character no-undo.

    assign ptotal = 0.
    do li = 1 to pnum:
        create tt-order.
        assign
            tt-order.order-num   = li + 12 * 1000
            tt-order.cust-num    = li modulo 25 + 1
            tt-order.order-date  = today
            tt-order.order-status      = "O"
            tt-order.order-total = 0.
        assign lsub = 0.
        do lj = 1 to 6:
            create tt-line.
            assign
                tt-line.order-num = tt-order.order-num
                tt-line.line-num  = lj
                tt-line.item-num  = lj * 100 + li
                tt-line.qty       = lj + 12 modulo 5
                tt-line.price     = lj * 2.5 + 12
                tt-line.extended  = calc-extended(tt-line.qty, tt-line.price).
            assign lsub = lsub + tt-line.extended.
        end.
        assign
            ltax  = calc-tax(lsub, gv-tax-rate)
            lstat = format-status(tt-order.order-status)
            tt-order.order-total = lsub + ltax
            ptotal = ptotal + tt-order.order-total.
        if gv-verbose then
            message "Batch 12 order" tt-order.order-num "total" tt-order.order-total lstat.
    end.
end procedure.

procedure process-batch-013:
    define input  parameter pnum   as integer no-undo.
    define output parameter ptotal as decimal no-undo.
    define variable li  as integer no-undo.
    define variable lj  as integer no-undo.
    define variable lsub as decimal no-undo.
    define variable ltax as decimal no-undo.
    define variable lstat as character no-undo.

    assign ptotal = 0.
    do li = 1 to pnum:
        create tt-order.
        assign
            tt-order.order-num   = li + 13 * 1000
            tt-order.cust-num    = li modulo 25 + 1
            tt-order.order-date  = today
            tt-order.order-status      = "O"
            tt-order.order-total = 0.
        assign lsub = 0.
        do lj = 1 to 6:
            create tt-line.
            assign
                tt-line.order-num = tt-order.order-num
                tt-line.line-num  = lj
                tt-line.item-num  = lj * 100 + li
                tt-line.qty       = lj + 13 modulo 5
                tt-line.price     = lj * 2.5 + 13
                tt-line.extended  = calc-extended(tt-line.qty, tt-line.price).
            assign lsub = lsub + tt-line.extended.
        end.
        assign
            ltax  = calc-tax(lsub, gv-tax-rate)
            lstat = format-status(tt-order.order-status)
            tt-order.order-total = lsub + ltax
            ptotal = ptotal + tt-order.order-total.
        if gv-verbose then
            message "Batch 13 order" tt-order.order-num "total" tt-order.order-total lstat.
    end.
end procedure.

procedure process-batch-014:
    define input  parameter pnum   as integer no-undo.
    define output parameter ptotal as decimal no-undo.
    define variable li  as integer no-undo.
    define variable lj  as integer no-undo.
    define variable lsub as decimal no-undo.
    define variable ltax as decimal no-undo.
    define variable lstat as character no-undo.

    assign ptotal = 0.
    do li = 1 to pnum:
        create tt-order.
        assign
            tt-order.order-num   = li + 14 * 1000
            tt-order.cust-num    = li modulo 25 + 1
            tt-order.order-date  = today
            tt-order.order-status      = "O"
            tt-order.order-total = 0.
        assign lsub = 0.
        do lj = 1 to 6:
            create tt-line.
            assign
                tt-line.order-num = tt-order.order-num
                tt-line.line-num  = lj
                tt-line.item-num  = lj * 100 + li
                tt-line.qty       = lj + 14 modulo 5
                tt-line.price     = lj * 2.5 + 14
                tt-line.extended  = calc-extended(tt-line.qty, tt-line.price).
            assign lsub = lsub + tt-line.extended.
        end.
        assign
            ltax  = calc-tax(lsub, gv-tax-rate)
            lstat = format-status(tt-order.order-status)
            tt-order.order-total = lsub + ltax
            ptotal = ptotal + tt-order.order-total.
        if gv-verbose then
            message "Batch 14 order" tt-order.order-num "total" tt-order.order-total lstat.
    end.
end procedure.

procedure process-batch-015:
    define input  parameter pnum   as integer no-undo.
    define output parameter ptotal as decimal no-undo.
    define variable li  as integer no-undo.
    define variable lj  as integer no-undo.
    define variable lsub as decimal no-undo.
    define variable ltax as decimal no-undo.
    define variable lstat as character no-undo.

    assign ptotal = 0.
    do li = 1 to pnum:
        create tt-order.
        assign
            tt-order.order-num   = li + 15 * 1000
            tt-order.cust-num    = li modulo 25 + 1
            tt-order.order-date  = today
            tt-order.order-status      = "O"
            tt-order.order-total = 0.
        assign lsub = 0.
        do lj = 1 to 6:
            create tt-line.
            assign
                tt-line.order-num = tt-order.order-num
                tt-line.line-num  = lj
                tt-line.item-num  = lj * 100 + li
                tt-line.qty       = lj + 15 modulo 5
                tt-line.price     = lj * 2.5 + 15
                tt-line.extended  = calc-extended(tt-line.qty, tt-line.price).
            assign lsub = lsub + tt-line.extended.
        end.
        assign
            ltax  = calc-tax(lsub, gv-tax-rate)
            lstat = format-status(tt-order.order-status)
            tt-order.order-total = lsub + ltax
            ptotal = ptotal + tt-order.order-total.
        if gv-verbose then
            message "Batch 15 order" tt-order.order-num "total" tt-order.order-total lstat.
    end.
end procedure.

procedure process-batch-016:
    define input  parameter pnum   as integer no-undo.
    define output parameter ptotal as decimal no-undo.
    define variable li  as integer no-undo.
    define variable lj  as integer no-undo.
    define variable lsub as decimal no-undo.
    define variable ltax as decimal no-undo.
    define variable lstat as character no-undo.

    assign ptotal = 0.
    do li = 1 to pnum:
        create tt-order.
        assign
            tt-order.order-num   = li + 16 * 1000
            tt-order.cust-num    = li modulo 25 + 1
            tt-order.order-date  = today
            tt-order.order-status      = "O"
            tt-order.order-total = 0.
        assign lsub = 0.
        do lj = 1 to 6:
            create tt-line.
            assign
                tt-line.order-num = tt-order.order-num
                tt-line.line-num  = lj
                tt-line.item-num  = lj * 100 + li
                tt-line.qty       = lj + 16 modulo 5
                tt-line.price     = lj * 2.5 + 16
                tt-line.extended  = calc-extended(tt-line.qty, tt-line.price).
            assign lsub = lsub + tt-line.extended.
        end.
        assign
            ltax  = calc-tax(lsub, gv-tax-rate)
            lstat = format-status(tt-order.order-status)
            tt-order.order-total = lsub + ltax
            ptotal = ptotal + tt-order.order-total.
        if gv-verbose then
            message "Batch 16 order" tt-order.order-num "total" tt-order.order-total lstat.
    end.
end procedure.

procedure process-batch-017:
    define input  parameter pnum   as integer no-undo.
    define output parameter ptotal as decimal no-undo.
    define variable li  as integer no-undo.
    define variable lj  as integer no-undo.
    define variable lsub as decimal no-undo.
    define variable ltax as decimal no-undo.
    define variable lstat as character no-undo.

    assign ptotal = 0.
    do li = 1 to pnum:
        create tt-order.
        assign
            tt-order.order-num   = li + 17 * 1000
            tt-order.cust-num    = li modulo 25 + 1
            tt-order.order-date  = today
            tt-order.order-status      = "O"
            tt-order.order-total = 0.
        assign lsub = 0.
        do lj = 1 to 6:
            create tt-line.
            assign
                tt-line.order-num = tt-order.order-num
                tt-line.line-num  = lj
                tt-line.item-num  = lj * 100 + li
                tt-line.qty       = lj + 17 modulo 5
                tt-line.price     = lj * 2.5 + 17
                tt-line.extended  = calc-extended(tt-line.qty, tt-line.price).
            assign lsub = lsub + tt-line.extended.
        end.
        assign
            ltax  = calc-tax(lsub, gv-tax-rate)
            lstat = format-status(tt-order.order-status)
            tt-order.order-total = lsub + ltax
            ptotal = ptotal + tt-order.order-total.
        if gv-verbose then
            message "Batch 17 order" tt-order.order-num "total" tt-order.order-total lstat.
    end.
end procedure.

procedure process-batch-018:
    define input  parameter pnum   as integer no-undo.
    define output parameter ptotal as decimal no-undo.
    define variable li  as integer no-undo.
    define variable lj  as integer no-undo.
    define variable lsub as decimal no-undo.
    define variable ltax as decimal no-undo.
    define variable lstat as character no-undo.

    assign ptotal = 0.
    do li = 1 to pnum:
        create tt-order.
        assign
            tt-order.order-num   = li + 18 * 1000
            tt-order.cust-num    = li modulo 25 + 1
            tt-order.order-date  = today
            tt-order.order-status      = "O"
            tt-order.order-total = 0.
        assign lsub = 0.
        do lj = 1 to 6:
            create tt-line.
            assign
                tt-line.order-num = tt-order.order-num
                tt-line.line-num  = lj
                tt-line.item-num  = lj * 100 + li
                tt-line.qty       = lj + 18 modulo 5
                tt-line.price     = lj * 2.5 + 18
                tt-line.extended  = calc-extended(tt-line.qty, tt-line.price).
            assign lsub = lsub + tt-line.extended.
        end.
        assign
            ltax  = calc-tax(lsub, gv-tax-rate)
            lstat = format-status(tt-order.order-status)
            tt-order.order-total = lsub + ltax
            ptotal = ptotal + tt-order.order-total.
        if gv-verbose then
            message "Batch 18 order" tt-order.order-num "total" tt-order.order-total lstat.
    end.
end procedure.

procedure process-batch-019:
    define input  parameter pnum   as integer no-undo.
    define output parameter ptotal as decimal no-undo.
    define variable li  as integer no-undo.
    define variable lj  as integer no-undo.
    define variable lsub as decimal no-undo.
    define variable ltax as decimal no-undo.
    define variable lstat as character no-undo.

    assign ptotal = 0.
    do li = 1 to pnum:
        create tt-order.
        assign
            tt-order.order-num   = li + 19 * 1000
            tt-order.cust-num    = li modulo 25 + 1
            tt-order.order-date  = today
            tt-order.order-status      = "O"
            tt-order.order-total = 0.
        assign lsub = 0.
        do lj = 1 to 6:
            create tt-line.
            assign
                tt-line.order-num = tt-order.order-num
                tt-line.line-num  = lj
                tt-line.item-num  = lj * 100 + li
                tt-line.qty       = lj + 19 modulo 5
                tt-line.price     = lj * 2.5 + 19
                tt-line.extended  = calc-extended(tt-line.qty, tt-line.price).
            assign lsub = lsub + tt-line.extended.
        end.
        assign
            ltax  = calc-tax(lsub, gv-tax-rate)
            lstat = format-status(tt-order.order-status)
            tt-order.order-total = lsub + ltax
            ptotal = ptotal + tt-order.order-total.
        if gv-verbose then
            message "Batch 19 order" tt-order.order-num "total" tt-order.order-total lstat.
    end.
end procedure.

procedure process-batch-020:
    define input  parameter pnum   as integer no-undo.
    define output parameter ptotal as decimal no-undo.
    define variable li  as integer no-undo.
    define variable lj  as integer no-undo.
    define variable lsub as decimal no-undo.
    define variable ltax as decimal no-undo.
    define variable lstat as character no-undo.

    assign ptotal = 0.
    do li = 1 to pnum:
        create tt-order.
        assign
            tt-order.order-num   = li + 20 * 1000
            tt-order.cust-num    = li modulo 25 + 1
            tt-order.order-date  = today
            tt-order.order-status      = "O"
            tt-order.order-total = 0.
        assign lsub = 0.
        do lj = 1 to 6:
            create tt-line.
            assign
                tt-line.order-num = tt-order.order-num
                tt-line.line-num  = lj
                tt-line.item-num  = lj * 100 + li
                tt-line.qty       = lj + 20 modulo 5
                tt-line.price     = lj * 2.5 + 20
                tt-line.extended  = calc-extended(tt-line.qty, tt-line.price).
            assign lsub = lsub + tt-line.extended.
        end.
        assign
            ltax  = calc-tax(lsub, gv-tax-rate)
            lstat = format-status(tt-order.order-status)
            tt-order.order-total = lsub + ltax
            ptotal = ptotal + tt-order.order-total.
        if gv-verbose then
            message "Batch 20 order" tt-order.order-num "total" tt-order.order-total lstat.
    end.
end procedure.

procedure process-batch-021:
    define input  parameter pnum   as integer no-undo.
    define output parameter ptotal as decimal no-undo.
    define variable li  as integer no-undo.
    define variable lj  as integer no-undo.
    define variable lsub as decimal no-undo.
    define variable ltax as decimal no-undo.
    define variable lstat as character no-undo.

    assign ptotal = 0.
    do li = 1 to pnum:
        create tt-order.
        assign
            tt-order.order-num   = li + 21 * 1000
            tt-order.cust-num    = li modulo 25 + 1
            tt-order.order-date  = today
            tt-order.order-status      = "O"
            tt-order.order-total = 0.
        assign lsub = 0.
        do lj = 1 to 6:
            create tt-line.
            assign
                tt-line.order-num = tt-order.order-num
                tt-line.line-num  = lj
                tt-line.item-num  = lj * 100 + li
                tt-line.qty       = lj + 21 modulo 5
                tt-line.price     = lj * 2.5 + 21
                tt-line.extended  = calc-extended(tt-line.qty, tt-line.price).
            assign lsub = lsub + tt-line.extended.
        end.
        assign
            ltax  = calc-tax(lsub, gv-tax-rate)
            lstat = format-status(tt-order.order-status)
            tt-order.order-total = lsub + ltax
            ptotal = ptotal + tt-order.order-total.
        if gv-verbose then
            message "Batch 21 order" tt-order.order-num "total" tt-order.order-total lstat.
    end.
end procedure.

procedure process-batch-022:
    define input  parameter pnum   as integer no-undo.
    define output parameter ptotal as decimal no-undo.
    define variable li  as integer no-undo.
    define variable lj  as integer no-undo.
    define variable lsub as decimal no-undo.
    define variable ltax as decimal no-undo.
    define variable lstat as character no-undo.

    assign ptotal = 0.
    do li = 1 to pnum:
        create tt-order.
        assign
            tt-order.order-num   = li + 22 * 1000
            tt-order.cust-num    = li modulo 25 + 1
            tt-order.order-date  = today
            tt-order.order-status      = "O"
            tt-order.order-total = 0.
        assign lsub = 0.
        do lj = 1 to 6:
            create tt-line.
            assign
                tt-line.order-num = tt-order.order-num
                tt-line.line-num  = lj
                tt-line.item-num  = lj * 100 + li
                tt-line.qty       = lj + 22 modulo 5
                tt-line.price     = lj * 2.5 + 22
                tt-line.extended  = calc-extended(tt-line.qty, tt-line.price).
            assign lsub = lsub + tt-line.extended.
        end.
        assign
            ltax  = calc-tax(lsub, gv-tax-rate)
            lstat = format-status(tt-order.order-status)
            tt-order.order-total = lsub + ltax
            ptotal = ptotal + tt-order.order-total.
        if gv-verbose then
            message "Batch 22 order" tt-order.order-num "total" tt-order.order-total lstat.
    end.
end procedure.

procedure process-batch-023:
    define input  parameter pnum   as integer no-undo.
    define output parameter ptotal as decimal no-undo.
    define variable li  as integer no-undo.
    define variable lj  as integer no-undo.
    define variable lsub as decimal no-undo.
    define variable ltax as decimal no-undo.
    define variable lstat as character no-undo.

    assign ptotal = 0.
    do li = 1 to pnum:
        create tt-order.
        assign
            tt-order.order-num   = li + 23 * 1000
            tt-order.cust-num    = li modulo 25 + 1
            tt-order.order-date  = today
            tt-order.order-status      = "O"
            tt-order.order-total = 0.
        assign lsub = 0.
        do lj = 1 to 6:
            create tt-line.
            assign
                tt-line.order-num = tt-order.order-num
                tt-line.line-num  = lj
                tt-line.item-num  = lj * 100 + li
                tt-line.qty       = lj + 23 modulo 5
                tt-line.price     = lj * 2.5 + 23
                tt-line.extended  = calc-extended(tt-line.qty, tt-line.price).
            assign lsub = lsub + tt-line.extended.
        end.
        assign
            ltax  = calc-tax(lsub, gv-tax-rate)
            lstat = format-status(tt-order.order-status)
            tt-order.order-total = lsub + ltax
            ptotal = ptotal + tt-order.order-total.
        if gv-verbose then
            message "Batch 23 order" tt-order.order-num "total" tt-order.order-total lstat.
    end.
end procedure.

procedure process-batch-024:
    define input  parameter pnum   as integer no-undo.
    define output parameter ptotal as decimal no-undo.
    define variable li  as integer no-undo.
    define variable lj  as integer no-undo.
    define variable lsub as decimal no-undo.
    define variable ltax as decimal no-undo.
    define variable lstat as character no-undo.

    assign ptotal = 0.
    do li = 1 to pnum:
        create tt-order.
        assign
            tt-order.order-num   = li + 24 * 1000
            tt-order.cust-num    = li modulo 25 + 1
            tt-order.order-date  = today
            tt-order.order-status      = "O"
            tt-order.order-total = 0.
        assign lsub = 0.
        do lj = 1 to 6:
            create tt-line.
            assign
                tt-line.order-num = tt-order.order-num
                tt-line.line-num  = lj
                tt-line.item-num  = lj * 100 + li
                tt-line.qty       = lj + 24 modulo 5
                tt-line.price     = lj * 2.5 + 24
                tt-line.extended  = calc-extended(tt-line.qty, tt-line.price).
            assign lsub = lsub + tt-line.extended.
        end.
        assign
            ltax  = calc-tax(lsub, gv-tax-rate)
            lstat = format-status(tt-order.order-status)
            tt-order.order-total = lsub + ltax
            ptotal = ptotal + tt-order.order-total.
        if gv-verbose then
            message "Batch 24 order" tt-order.order-num "total" tt-order.order-total lstat.
    end.
end procedure.

procedure process-batch-025:
    define input  parameter pnum   as integer no-undo.
    define output parameter ptotal as decimal no-undo.
    define variable li  as integer no-undo.
    define variable lj  as integer no-undo.
    define variable lsub as decimal no-undo.
    define variable ltax as decimal no-undo.
    define variable lstat as character no-undo.

    assign ptotal = 0.
    do li = 1 to pnum:
        create tt-order.
        assign
            tt-order.order-num   = li + 25 * 1000
            tt-order.cust-num    = li modulo 25 + 1
            tt-order.order-date  = today
            tt-order.order-status      = "O"
            tt-order.order-total = 0.
        assign lsub = 0.
        do lj = 1 to 6:
            create tt-line.
            assign
                tt-line.order-num = tt-order.order-num
                tt-line.line-num  = lj
                tt-line.item-num  = lj * 100 + li
                tt-line.qty       = lj + 25 modulo 5
                tt-line.price     = lj * 2.5 + 25
                tt-line.extended  = calc-extended(tt-line.qty, tt-line.price).
            assign lsub = lsub + tt-line.extended.
        end.
        assign
            ltax  = calc-tax(lsub, gv-tax-rate)
            lstat = format-status(tt-order.order-status)
            tt-order.order-total = lsub + ltax
            ptotal = ptotal + tt-order.order-total.
        if gv-verbose then
            message "Batch 25 order" tt-order.order-num "total" tt-order.order-total lstat.
    end.
end procedure.

procedure process-batch-026:
    define input  parameter pnum   as integer no-undo.
    define output parameter ptotal as decimal no-undo.
    define variable li  as integer no-undo.
    define variable lj  as integer no-undo.
    define variable lsub as decimal no-undo.
    define variable ltax as decimal no-undo.
    define variable lstat as character no-undo.

    assign ptotal = 0.
    do li = 1 to pnum:
        create tt-order.
        assign
            tt-order.order-num   = li + 26 * 1000
            tt-order.cust-num    = li modulo 25 + 1
            tt-order.order-date  = today
            tt-order.order-status      = "O"
            tt-order.order-total = 0.
        assign lsub = 0.
        do lj = 1 to 6:
            create tt-line.
            assign
                tt-line.order-num = tt-order.order-num
                tt-line.line-num  = lj
                tt-line.item-num  = lj * 100 + li
                tt-line.qty       = lj + 26 modulo 5
                tt-line.price     = lj * 2.5 + 26
                tt-line.extended  = calc-extended(tt-line.qty, tt-line.price).
            assign lsub = lsub + tt-line.extended.
        end.
        assign
            ltax  = calc-tax(lsub, gv-tax-rate)
            lstat = format-status(tt-order.order-status)
            tt-order.order-total = lsub + ltax
            ptotal = ptotal + tt-order.order-total.
        if gv-verbose then
            message "Batch 26 order" tt-order.order-num "total" tt-order.order-total lstat.
    end.
end procedure.

procedure process-batch-027:
    define input  parameter pnum   as integer no-undo.
    define output parameter ptotal as decimal no-undo.
    define variable li  as integer no-undo.
    define variable lj  as integer no-undo.
    define variable lsub as decimal no-undo.
    define variable ltax as decimal no-undo.
    define variable lstat as character no-undo.

    assign ptotal = 0.
    do li = 1 to pnum:
        create tt-order.
        assign
            tt-order.order-num   = li + 27 * 1000
            tt-order.cust-num    = li modulo 25 + 1
            tt-order.order-date  = today
            tt-order.order-status      = "O"
            tt-order.order-total = 0.
        assign lsub = 0.
        do lj = 1 to 6:
            create tt-line.
            assign
                tt-line.order-num = tt-order.order-num
                tt-line.line-num  = lj
                tt-line.item-num  = lj * 100 + li
                tt-line.qty       = lj + 27 modulo 5
                tt-line.price     = lj * 2.5 + 27
                tt-line.extended  = calc-extended(tt-line.qty, tt-line.price).
            assign lsub = lsub + tt-line.extended.
        end.
        assign
            ltax  = calc-tax(lsub, gv-tax-rate)
            lstat = format-status(tt-order.order-status)
            tt-order.order-total = lsub + ltax
            ptotal = ptotal + tt-order.order-total.
        if gv-verbose then
            message "Batch 27 order" tt-order.order-num "total" tt-order.order-total lstat.
    end.
end procedure.

procedure process-batch-028:
    define input  parameter pnum   as integer no-undo.
    define output parameter ptotal as decimal no-undo.
    define variable li  as integer no-undo.
    define variable lj  as integer no-undo.
    define variable lsub as decimal no-undo.
    define variable ltax as decimal no-undo.
    define variable lstat as character no-undo.

    assign ptotal = 0.
    do li = 1 to pnum:
        create tt-order.
        assign
            tt-order.order-num   = li + 28 * 1000
            tt-order.cust-num    = li modulo 25 + 1
            tt-order.order-date  = today
            tt-order.order-status      = "O"
            tt-order.order-total = 0.
        assign lsub = 0.
        do lj = 1 to 6:
            create tt-line.
            assign
                tt-line.order-num = tt-order.order-num
                tt-line.line-num  = lj
                tt-line.item-num  = lj * 100 + li
                tt-line.qty       = lj + 28 modulo 5
                tt-line.price     = lj * 2.5 + 28
                tt-line.extended  = calc-extended(tt-line.qty, tt-line.price).
            assign lsub = lsub + tt-line.extended.
        end.
        assign
            ltax  = calc-tax(lsub, gv-tax-rate)
            lstat = format-status(tt-order.order-status)
            tt-order.order-total = lsub + ltax
            ptotal = ptotal + tt-order.order-total.
        if gv-verbose then
            message "Batch 28 order" tt-order.order-num "total" tt-order.order-total lstat.
    end.
end procedure.

procedure process-batch-029:
    define input  parameter pnum   as integer no-undo.
    define output parameter ptotal as decimal no-undo.
    define variable li  as integer no-undo.
    define variable lj  as integer no-undo.
    define variable lsub as decimal no-undo.
    define variable ltax as decimal no-undo.
    define variable lstat as character no-undo.

    assign ptotal = 0.
    do li = 1 to pnum:
        create tt-order.
        assign
            tt-order.order-num   = li + 29 * 1000
            tt-order.cust-num    = li modulo 25 + 1
            tt-order.order-date  = today
            tt-order.order-status      = "O"
            tt-order.order-total = 0.
        assign lsub = 0.
        do lj = 1 to 6:
            create tt-line.
            assign
                tt-line.order-num = tt-order.order-num
                tt-line.line-num  = lj
                tt-line.item-num  = lj * 100 + li
                tt-line.qty       = lj + 29 modulo 5
                tt-line.price     = lj * 2.5 + 29
                tt-line.extended  = calc-extended(tt-line.qty, tt-line.price).
            assign lsub = lsub + tt-line.extended.
        end.
        assign
            ltax  = calc-tax(lsub, gv-tax-rate)
            lstat = format-status(tt-order.order-status)
            tt-order.order-total = lsub + ltax
            ptotal = ptotal + tt-order.order-total.
        if gv-verbose then
            message "Batch 29 order" tt-order.order-num "total" tt-order.order-total lstat.
    end.
end procedure.

procedure process-batch-030:
    define input  parameter pnum   as integer no-undo.
    define output parameter ptotal as decimal no-undo.
    define variable li  as integer no-undo.
    define variable lj  as integer no-undo.
    define variable lsub as decimal no-undo.
    define variable ltax as decimal no-undo.
    define variable lstat as character no-undo.

    assign ptotal = 0.
    do li = 1 to pnum:
        create tt-order.
        assign
            tt-order.order-num   = li + 30 * 1000
            tt-order.cust-num    = li modulo 25 + 1
            tt-order.order-date  = today
            tt-order.order-status      = "O"
            tt-order.order-total = 0.
        assign lsub = 0.
        do lj = 1 to 6:
            create tt-line.
            assign
                tt-line.order-num = tt-order.order-num
                tt-line.line-num  = lj
                tt-line.item-num  = lj * 100 + li
                tt-line.qty       = lj + 30 modulo 5
                tt-line.price     = lj * 2.5 + 30
                tt-line.extended  = calc-extended(tt-line.qty, tt-line.price).
            assign lsub = lsub + tt-line.extended.
        end.
        assign
            ltax  = calc-tax(lsub, gv-tax-rate)
            lstat = format-status(tt-order.order-status)
            tt-order.order-total = lsub + ltax
            ptotal = ptotal + tt-order.order-total.
        if gv-verbose then
            message "Batch 30 order" tt-order.order-num "total" tt-order.order-total lstat.
    end.
end procedure.

procedure process-batch-031:
    define input  parameter pnum   as integer no-undo.
    define output parameter ptotal as decimal no-undo.
    define variable li  as integer no-undo.
    define variable lj  as integer no-undo.
    define variable lsub as decimal no-undo.
    define variable ltax as decimal no-undo.
    define variable lstat as character no-undo.

    assign ptotal = 0.
    do li = 1 to pnum:
        create tt-order.
        assign
            tt-order.order-num   = li + 31 * 1000
            tt-order.cust-num    = li modulo 25 + 1
            tt-order.order-date  = today
            tt-order.order-status      = "O"
            tt-order.order-total = 0.
        assign lsub = 0.
        do lj = 1 to 6:
            create tt-line.
            assign
                tt-line.order-num = tt-order.order-num
                tt-line.line-num  = lj
                tt-line.item-num  = lj * 100 + li
                tt-line.qty       = lj + 31 modulo 5
                tt-line.price     = lj * 2.5 + 31
                tt-line.extended  = calc-extended(tt-line.qty, tt-line.price).
            assign lsub = lsub + tt-line.extended.
        end.
        assign
            ltax  = calc-tax(lsub, gv-tax-rate)
            lstat = format-status(tt-order.order-status)
            tt-order.order-total = lsub + ltax
            ptotal = ptotal + tt-order.order-total.
        if gv-verbose then
            message "Batch 31 order" tt-order.order-num "total" tt-order.order-total lstat.
    end.
end procedure.

procedure process-batch-032:
    define input  parameter pnum   as integer no-undo.
    define output parameter ptotal as decimal no-undo.
    define variable li  as integer no-undo.
    define variable lj  as integer no-undo.
    define variable lsub as decimal no-undo.
    define variable ltax as decimal no-undo.
    define variable lstat as character no-undo.

    assign ptotal = 0.
    do li = 1 to pnum:
        create tt-order.
        assign
            tt-order.order-num   = li + 32 * 1000
            tt-order.cust-num    = li modulo 25 + 1
            tt-order.order-date  = today
            tt-order.order-status      = "O"
            tt-order.order-total = 0.
        assign lsub = 0.
        do lj = 1 to 6:
            create tt-line.
            assign
                tt-line.order-num = tt-order.order-num
                tt-line.line-num  = lj
                tt-line.item-num  = lj * 100 + li
                tt-line.qty       = lj + 32 modulo 5
                tt-line.price     = lj * 2.5 + 32
                tt-line.extended  = calc-extended(tt-line.qty, tt-line.price).
            assign lsub = lsub + tt-line.extended.
        end.
        assign
            ltax  = calc-tax(lsub, gv-tax-rate)
            lstat = format-status(tt-order.order-status)
            tt-order.order-total = lsub + ltax
            ptotal = ptotal + tt-order.order-total.
        if gv-verbose then
            message "Batch 32 order" tt-order.order-num "total" tt-order.order-total lstat.
    end.
end procedure.

procedure process-batch-033:
    define input  parameter pnum   as integer no-undo.
    define output parameter ptotal as decimal no-undo.
    define variable li  as integer no-undo.
    define variable lj  as integer no-undo.
    define variable lsub as decimal no-undo.
    define variable ltax as decimal no-undo.
    define variable lstat as character no-undo.

    assign ptotal = 0.
    do li = 1 to pnum:
        create tt-order.
        assign
            tt-order.order-num   = li + 33 * 1000
            tt-order.cust-num    = li modulo 25 + 1
            tt-order.order-date  = today
            tt-order.order-status      = "O"
            tt-order.order-total = 0.
        assign lsub = 0.
        do lj = 1 to 6:
            create tt-line.
            assign
                tt-line.order-num = tt-order.order-num
                tt-line.line-num  = lj
                tt-line.item-num  = lj * 100 + li
                tt-line.qty       = lj + 33 modulo 5
                tt-line.price     = lj * 2.5 + 33
                tt-line.extended  = calc-extended(tt-line.qty, tt-line.price).
            assign lsub = lsub + tt-line.extended.
        end.
        assign
            ltax  = calc-tax(lsub, gv-tax-rate)
            lstat = format-status(tt-order.order-status)
            tt-order.order-total = lsub + ltax
            ptotal = ptotal + tt-order.order-total.
        if gv-verbose then
            message "Batch 33 order" tt-order.order-num "total" tt-order.order-total lstat.
    end.
end procedure.

procedure process-batch-034:
    define input  parameter pnum   as integer no-undo.
    define output parameter ptotal as decimal no-undo.
    define variable li  as integer no-undo.
    define variable lj  as integer no-undo.
    define variable lsub as decimal no-undo.
    define variable ltax as decimal no-undo.
    define variable lstat as character no-undo.

    assign ptotal = 0.
    do li = 1 to pnum:
        create tt-order.
        assign
            tt-order.order-num   = li + 34 * 1000
            tt-order.cust-num    = li modulo 25 + 1
            tt-order.order-date  = today
            tt-order.order-status      = "O"
            tt-order.order-total = 0.
        assign lsub = 0.
        do lj = 1 to 6:
            create tt-line.
            assign
                tt-line.order-num = tt-order.order-num
                tt-line.line-num  = lj
                tt-line.item-num  = lj * 100 + li
                tt-line.qty       = lj + 34 modulo 5
                tt-line.price     = lj * 2.5 + 34
                tt-line.extended  = calc-extended(tt-line.qty, tt-line.price).
            assign lsub = lsub + tt-line.extended.
        end.
        assign
            ltax  = calc-tax(lsub, gv-tax-rate)
            lstat = format-status(tt-order.order-status)
            tt-order.order-total = lsub + ltax
            ptotal = ptotal + tt-order.order-total.
        if gv-verbose then
            message "Batch 34 order" tt-order.order-num "total" tt-order.order-total lstat.
    end.
end procedure.

procedure process-batch-035:
    define input  parameter pnum   as integer no-undo.
    define output parameter ptotal as decimal no-undo.
    define variable li  as integer no-undo.
    define variable lj  as integer no-undo.
    define variable lsub as decimal no-undo.
    define variable ltax as decimal no-undo.
    define variable lstat as character no-undo.

    assign ptotal = 0.
    do li = 1 to pnum:
        create tt-order.
        assign
            tt-order.order-num   = li + 35 * 1000
            tt-order.cust-num    = li modulo 25 + 1
            tt-order.order-date  = today
            tt-order.order-status      = "O"
            tt-order.order-total = 0.
        assign lsub = 0.
        do lj = 1 to 6:
            create tt-line.
            assign
                tt-line.order-num = tt-order.order-num
                tt-line.line-num  = lj
                tt-line.item-num  = lj * 100 + li
                tt-line.qty       = lj + 35 modulo 5
                tt-line.price     = lj * 2.5 + 35
                tt-line.extended  = calc-extended(tt-line.qty, tt-line.price).
            assign lsub = lsub + tt-line.extended.
        end.
        assign
            ltax  = calc-tax(lsub, gv-tax-rate)
            lstat = format-status(tt-order.order-status)
            tt-order.order-total = lsub + ltax
            ptotal = ptotal + tt-order.order-total.
        if gv-verbose then
            message "Batch 35 order" tt-order.order-num "total" tt-order.order-total lstat.
    end.
end procedure.

procedure process-batch-036:
    define input  parameter pnum   as integer no-undo.
    define output parameter ptotal as decimal no-undo.
    define variable li  as integer no-undo.
    define variable lj  as integer no-undo.
    define variable lsub as decimal no-undo.
    define variable ltax as decimal no-undo.
    define variable lstat as character no-undo.

    assign ptotal = 0.
    do li = 1 to pnum:
        create tt-order.
        assign
            tt-order.order-num   = li + 36 * 1000
            tt-order.cust-num    = li modulo 25 + 1
            tt-order.order-date  = today
            tt-order.order-status      = "O"
            tt-order.order-total = 0.
        assign lsub = 0.
        do lj = 1 to 6:
            create tt-line.
            assign
                tt-line.order-num = tt-order.order-num
                tt-line.line-num  = lj
                tt-line.item-num  = lj * 100 + li
                tt-line.qty       = lj + 36 modulo 5
                tt-line.price     = lj * 2.5 + 36
                tt-line.extended  = calc-extended(tt-line.qty, tt-line.price).
            assign lsub = lsub + tt-line.extended.
        end.
        assign
            ltax  = calc-tax(lsub, gv-tax-rate)
            lstat = format-status(tt-order.order-status)
            tt-order.order-total = lsub + ltax
            ptotal = ptotal + tt-order.order-total.
        if gv-verbose then
            message "Batch 36 order" tt-order.order-num "total" tt-order.order-total lstat.
    end.
end procedure.

procedure process-batch-037:
    define input  parameter pnum   as integer no-undo.
    define output parameter ptotal as decimal no-undo.
    define variable li  as integer no-undo.
    define variable lj  as integer no-undo.
    define variable lsub as decimal no-undo.
    define variable ltax as decimal no-undo.
    define variable lstat as character no-undo.

    assign ptotal = 0.
    do li = 1 to pnum:
        create tt-order.
        assign
            tt-order.order-num   = li + 37 * 1000
            tt-order.cust-num    = li modulo 25 + 1
            tt-order.order-date  = today
            tt-order.order-status      = "O"
            tt-order.order-total = 0.
        assign lsub = 0.
        do lj = 1 to 6:
            create tt-line.
            assign
                tt-line.order-num = tt-order.order-num
                tt-line.line-num  = lj
                tt-line.item-num  = lj * 100 + li
                tt-line.qty       = lj + 37 modulo 5
                tt-line.price     = lj * 2.5 + 37
                tt-line.extended  = calc-extended(tt-line.qty, tt-line.price).
            assign lsub = lsub + tt-line.extended.
        end.
        assign
            ltax  = calc-tax(lsub, gv-tax-rate)
            lstat = format-status(tt-order.order-status)
            tt-order.order-total = lsub + ltax
            ptotal = ptotal + tt-order.order-total.
        if gv-verbose then
            message "Batch 37 order" tt-order.order-num "total" tt-order.order-total lstat.
    end.
end procedure.

procedure process-batch-038:
    define input  parameter pnum   as integer no-undo.
    define output parameter ptotal as decimal no-undo.
    define variable li  as integer no-undo.
    define variable lj  as integer no-undo.
    define variable lsub as decimal no-undo.
    define variable ltax as decimal no-undo.
    define variable lstat as character no-undo.

    assign ptotal = 0.
    do li = 1 to pnum:
        create tt-order.
        assign
            tt-order.order-num   = li + 38 * 1000
            tt-order.cust-num    = li modulo 25 + 1
            tt-order.order-date  = today
            tt-order.order-status      = "O"
            tt-order.order-total = 0.
        assign lsub = 0.
        do lj = 1 to 6:
            create tt-line.
            assign
                tt-line.order-num = tt-order.order-num
                tt-line.line-num  = lj
                tt-line.item-num  = lj * 100 + li
                tt-line.qty       = lj + 38 modulo 5
                tt-line.price     = lj * 2.5 + 38
                tt-line.extended  = calc-extended(tt-line.qty, tt-line.price).
            assign lsub = lsub + tt-line.extended.
        end.
        assign
            ltax  = calc-tax(lsub, gv-tax-rate)
            lstat = format-status(tt-order.order-status)
            tt-order.order-total = lsub + ltax
            ptotal = ptotal + tt-order.order-total.
        if gv-verbose then
            message "Batch 38 order" tt-order.order-num "total" tt-order.order-total lstat.
    end.
end procedure.

procedure process-batch-039:
    define input  parameter pnum   as integer no-undo.
    define output parameter ptotal as decimal no-undo.
    define variable li  as integer no-undo.
    define variable lj  as integer no-undo.
    define variable lsub as decimal no-undo.
    define variable ltax as decimal no-undo.
    define variable lstat as character no-undo.

    assign ptotal = 0.
    do li = 1 to pnum:
        create tt-order.
        assign
            tt-order.order-num   = li + 39 * 1000
            tt-order.cust-num    = li modulo 25 + 1
            tt-order.order-date  = today
            tt-order.order-status      = "O"
            tt-order.order-total = 0.
        assign lsub = 0.
        do lj = 1 to 6:
            create tt-line.
            assign
                tt-line.order-num = tt-order.order-num
                tt-line.line-num  = lj
                tt-line.item-num  = lj * 100 + li
                tt-line.qty       = lj + 39 modulo 5
                tt-line.price     = lj * 2.5 + 39
                tt-line.extended  = calc-extended(tt-line.qty, tt-line.price).
            assign lsub = lsub + tt-line.extended.
        end.
        assign
            ltax  = calc-tax(lsub, gv-tax-rate)
            lstat = format-status(tt-order.order-status)
            tt-order.order-total = lsub + ltax
            ptotal = ptotal + tt-order.order-total.
        if gv-verbose then
            message "Batch 39 order" tt-order.order-num "total" tt-order.order-total lstat.
    end.
end procedure.

procedure process-batch-040:
    define input  parameter pnum   as integer no-undo.
    define output parameter ptotal as decimal no-undo.
    define variable li  as integer no-undo.
    define variable lj  as integer no-undo.
    define variable lsub as decimal no-undo.
    define variable ltax as decimal no-undo.
    define variable lstat as character no-undo.

    assign ptotal = 0.
    do li = 1 to pnum:
        create tt-order.
        assign
            tt-order.order-num   = li + 40 * 1000
            tt-order.cust-num    = li modulo 25 + 1
            tt-order.order-date  = today
            tt-order.order-status      = "O"
            tt-order.order-total = 0.
        assign lsub = 0.
        do lj = 1 to 6:
            create tt-line.
            assign
                tt-line.order-num = tt-order.order-num
                tt-line.line-num  = lj
                tt-line.item-num  = lj * 100 + li
                tt-line.qty       = lj + 40 modulo 5
                tt-line.price     = lj * 2.5 + 40
                tt-line.extended  = calc-extended(tt-line.qty, tt-line.price).
            assign lsub = lsub + tt-line.extended.
        end.
        assign
            ltax  = calc-tax(lsub, gv-tax-rate)
            lstat = format-status(tt-order.order-status)
            tt-order.order-total = lsub + ltax
            ptotal = ptotal + tt-order.order-total.
        if gv-verbose then
            message "Batch 40 order" tt-order.order-num "total" tt-order.order-total lstat.
    end.
end procedure.

procedure process-batch-041:
    define input  parameter pnum   as integer no-undo.
    define output parameter ptotal as decimal no-undo.
    define variable li  as integer no-undo.
    define variable lj  as integer no-undo.
    define variable lsub as decimal no-undo.
    define variable ltax as decimal no-undo.
    define variable lstat as character no-undo.

    assign ptotal = 0.
    do li = 1 to pnum:
        create tt-order.
        assign
            tt-order.order-num   = li + 41 * 1000
            tt-order.cust-num    = li modulo 25 + 1
            tt-order.order-date  = today
            tt-order.order-status      = "O"
            tt-order.order-total = 0.
        assign lsub = 0.
        do lj = 1 to 6:
            create tt-line.
            assign
                tt-line.order-num = tt-order.order-num
                tt-line.line-num  = lj
                tt-line.item-num  = lj * 100 + li
                tt-line.qty       = lj + 41 modulo 5
                tt-line.price     = lj * 2.5 + 41
                tt-line.extended  = calc-extended(tt-line.qty, tt-line.price).
            assign lsub = lsub + tt-line.extended.
        end.
        assign
            ltax  = calc-tax(lsub, gv-tax-rate)
            lstat = format-status(tt-order.order-status)
            tt-order.order-total = lsub + ltax
            ptotal = ptotal + tt-order.order-total.
        if gv-verbose then
            message "Batch 41 order" tt-order.order-num "total" tt-order.order-total lstat.
    end.
end procedure.

procedure process-batch-042:
    define input  parameter pnum   as integer no-undo.
    define output parameter ptotal as decimal no-undo.
    define variable li  as integer no-undo.
    define variable lj  as integer no-undo.
    define variable lsub as decimal no-undo.
    define variable ltax as decimal no-undo.
    define variable lstat as character no-undo.

    assign ptotal = 0.
    do li = 1 to pnum:
        create tt-order.
        assign
            tt-order.order-num   = li + 42 * 1000
            tt-order.cust-num    = li modulo 25 + 1
            tt-order.order-date  = today
            tt-order.order-status      = "O"
            tt-order.order-total = 0.
        assign lsub = 0.
        do lj = 1 to 6:
            create tt-line.
            assign
                tt-line.order-num = tt-order.order-num
                tt-line.line-num  = lj
                tt-line.item-num  = lj * 100 + li
                tt-line.qty       = lj + 42 modulo 5
                tt-line.price     = lj * 2.5 + 42
                tt-line.extended  = calc-extended(tt-line.qty, tt-line.price).
            assign lsub = lsub + tt-line.extended.
        end.
        assign
            ltax  = calc-tax(lsub, gv-tax-rate)
            lstat = format-status(tt-order.order-status)
            tt-order.order-total = lsub + ltax
            ptotal = ptotal + tt-order.order-total.
        if gv-verbose then
            message "Batch 42 order" tt-order.order-num "total" tt-order.order-total lstat.
    end.
end procedure.

procedure process-batch-043:
    define input  parameter pnum   as integer no-undo.
    define output parameter ptotal as decimal no-undo.
    define variable li  as integer no-undo.
    define variable lj  as integer no-undo.
    define variable lsub as decimal no-undo.
    define variable ltax as decimal no-undo.
    define variable lstat as character no-undo.

    assign ptotal = 0.
    do li = 1 to pnum:
        create tt-order.
        assign
            tt-order.order-num   = li + 43 * 1000
            tt-order.cust-num    = li modulo 25 + 1
            tt-order.order-date  = today
            tt-order.order-status      = "O"
            tt-order.order-total = 0.
        assign lsub = 0.
        do lj = 1 to 6:
            create tt-line.
            assign
                tt-line.order-num = tt-order.order-num
                tt-line.line-num  = lj
                tt-line.item-num  = lj * 100 + li
                tt-line.qty       = lj + 43 modulo 5
                tt-line.price     = lj * 2.5 + 43
                tt-line.extended  = calc-extended(tt-line.qty, tt-line.price).
            assign lsub = lsub + tt-line.extended.
        end.
        assign
            ltax  = calc-tax(lsub, gv-tax-rate)
            lstat = format-status(tt-order.order-status)
            tt-order.order-total = lsub + ltax
            ptotal = ptotal + tt-order.order-total.
        if gv-verbose then
            message "Batch 43 order" tt-order.order-num "total" tt-order.order-total lstat.
    end.
end procedure.

procedure process-batch-044:
    define input  parameter pnum   as integer no-undo.
    define output parameter ptotal as decimal no-undo.
    define variable li  as integer no-undo.
    define variable lj  as integer no-undo.
    define variable lsub as decimal no-undo.
    define variable ltax as decimal no-undo.
    define variable lstat as character no-undo.

    assign ptotal = 0.
    do li = 1 to pnum:
        create tt-order.
        assign
            tt-order.order-num   = li + 44 * 1000
            tt-order.cust-num    = li modulo 25 + 1
            tt-order.order-date  = today
            tt-order.order-status      = "O"
            tt-order.order-total = 0.
        assign lsub = 0.
        do lj = 1 to 6:
            create tt-line.
            assign
                tt-line.order-num = tt-order.order-num
                tt-line.line-num  = lj
                tt-line.item-num  = lj * 100 + li
                tt-line.qty       = lj + 44 modulo 5
                tt-line.price     = lj * 2.5 + 44
                tt-line.extended  = calc-extended(tt-line.qty, tt-line.price).
            assign lsub = lsub + tt-line.extended.
        end.
        assign
            ltax  = calc-tax(lsub, gv-tax-rate)
            lstat = format-status(tt-order.order-status)
            tt-order.order-total = lsub + ltax
            ptotal = ptotal + tt-order.order-total.
        if gv-verbose then
            message "Batch 44 order" tt-order.order-num "total" tt-order.order-total lstat.
    end.
end procedure.

procedure process-batch-045:
    define input  parameter pnum   as integer no-undo.
    define output parameter ptotal as decimal no-undo.
    define variable li  as integer no-undo.
    define variable lj  as integer no-undo.
    define variable lsub as decimal no-undo.
    define variable ltax as decimal no-undo.
    define variable lstat as character no-undo.

    assign ptotal = 0.
    do li = 1 to pnum:
        create tt-order.
        assign
            tt-order.order-num   = li + 45 * 1000
            tt-order.cust-num    = li modulo 25 + 1
            tt-order.order-date  = today
            tt-order.order-status      = "O"
            tt-order.order-total = 0.
        assign lsub = 0.
        do lj = 1 to 6:
            create tt-line.
            assign
                tt-line.order-num = tt-order.order-num
                tt-line.line-num  = lj
                tt-line.item-num  = lj * 100 + li
                tt-line.qty       = lj + 45 modulo 5
                tt-line.price     = lj * 2.5 + 45
                tt-line.extended  = calc-extended(tt-line.qty, tt-line.price).
            assign lsub = lsub + tt-line.extended.
        end.
        assign
            ltax  = calc-tax(lsub, gv-tax-rate)
            lstat = format-status(tt-order.order-status)
            tt-order.order-total = lsub + ltax
            ptotal = ptotal + tt-order.order-total.
        if gv-verbose then
            message "Batch 45 order" tt-order.order-num "total" tt-order.order-total lstat.
    end.
end procedure.

procedure process-batch-046:
    define input  parameter pnum   as integer no-undo.
    define output parameter ptotal as decimal no-undo.
    define variable li  as integer no-undo.
    define variable lj  as integer no-undo.
    define variable lsub as decimal no-undo.
    define variable ltax as decimal no-undo.
    define variable lstat as character no-undo.

    assign ptotal = 0.
    do li = 1 to pnum:
        create tt-order.
        assign
            tt-order.order-num   = li + 46 * 1000
            tt-order.cust-num    = li modulo 25 + 1
            tt-order.order-date  = today
            tt-order.order-status      = "O"
            tt-order.order-total = 0.
        assign lsub = 0.
        do lj = 1 to 6:
            create tt-line.
            assign
                tt-line.order-num = tt-order.order-num
                tt-line.line-num  = lj
                tt-line.item-num  = lj * 100 + li
                tt-line.qty       = lj + 46 modulo 5
                tt-line.price     = lj * 2.5 + 46
                tt-line.extended  = calc-extended(tt-line.qty, tt-line.price).
            assign lsub = lsub + tt-line.extended.
        end.
        assign
            ltax  = calc-tax(lsub, gv-tax-rate)
            lstat = format-status(tt-order.order-status)
            tt-order.order-total = lsub + ltax
            ptotal = ptotal + tt-order.order-total.
        if gv-verbose then
            message "Batch 46 order" tt-order.order-num "total" tt-order.order-total lstat.
    end.
end procedure.

procedure process-batch-047:
    define input  parameter pnum   as integer no-undo.
    define output parameter ptotal as decimal no-undo.
    define variable li  as integer no-undo.
    define variable lj  as integer no-undo.
    define variable lsub as decimal no-undo.
    define variable ltax as decimal no-undo.
    define variable lstat as character no-undo.

    assign ptotal = 0.
    do li = 1 to pnum:
        create tt-order.
        assign
            tt-order.order-num   = li + 47 * 1000
            tt-order.cust-num    = li modulo 25 + 1
            tt-order.order-date  = today
            tt-order.order-status      = "O"
            tt-order.order-total = 0.
        assign lsub = 0.
        do lj = 1 to 6:
            create tt-line.
            assign
                tt-line.order-num = tt-order.order-num
                tt-line.line-num  = lj
                tt-line.item-num  = lj * 100 + li
                tt-line.qty       = lj + 47 modulo 5
                tt-line.price     = lj * 2.5 + 47
                tt-line.extended  = calc-extended(tt-line.qty, tt-line.price).
            assign lsub = lsub + tt-line.extended.
        end.
        assign
            ltax  = calc-tax(lsub, gv-tax-rate)
            lstat = format-status(tt-order.order-status)
            tt-order.order-total = lsub + ltax
            ptotal = ptotal + tt-order.order-total.
        if gv-verbose then
            message "Batch 47 order" tt-order.order-num "total" tt-order.order-total lstat.
    end.
end procedure.

procedure process-batch-048:
    define input  parameter pnum   as integer no-undo.
    define output parameter ptotal as decimal no-undo.
    define variable li  as integer no-undo.
    define variable lj  as integer no-undo.
    define variable lsub as decimal no-undo.
    define variable ltax as decimal no-undo.
    define variable lstat as character no-undo.

    assign ptotal = 0.
    do li = 1 to pnum:
        create tt-order.
        assign
            tt-order.order-num   = li + 48 * 1000
            tt-order.cust-num    = li modulo 25 + 1
            tt-order.order-date  = today
            tt-order.order-status      = "O"
            tt-order.order-total = 0.
        assign lsub = 0.
        do lj = 1 to 6:
            create tt-line.
            assign
                tt-line.order-num = tt-order.order-num
                tt-line.line-num  = lj
                tt-line.item-num  = lj * 100 + li
                tt-line.qty       = lj + 48 modulo 5
                tt-line.price     = lj * 2.5 + 48
                tt-line.extended  = calc-extended(tt-line.qty, tt-line.price).
            assign lsub = lsub + tt-line.extended.
        end.
        assign
            ltax  = calc-tax(lsub, gv-tax-rate)
            lstat = format-status(tt-order.order-status)
            tt-order.order-total = lsub + ltax
            ptotal = ptotal + tt-order.order-total.
        if gv-verbose then
            message "Batch 48 order" tt-order.order-num "total" tt-order.order-total lstat.
    end.
end procedure.

procedure process-batch-049:
    define input  parameter pnum   as integer no-undo.
    define output parameter ptotal as decimal no-undo.
    define variable li  as integer no-undo.
    define variable lj  as integer no-undo.
    define variable lsub as decimal no-undo.
    define variable ltax as decimal no-undo.
    define variable lstat as character no-undo.

    assign ptotal = 0.
    do li = 1 to pnum:
        create tt-order.
        assign
            tt-order.order-num   = li + 49 * 1000
            tt-order.cust-num    = li modulo 25 + 1
            tt-order.order-date  = today
            tt-order.order-status      = "O"
            tt-order.order-total = 0.
        assign lsub = 0.
        do lj = 1 to 6:
            create tt-line.
            assign
                tt-line.order-num = tt-order.order-num
                tt-line.line-num  = lj
                tt-line.item-num  = lj * 100 + li
                tt-line.qty       = lj + 49 modulo 5
                tt-line.price     = lj * 2.5 + 49
                tt-line.extended  = calc-extended(tt-line.qty, tt-line.price).
            assign lsub = lsub + tt-line.extended.
        end.
        assign
            ltax  = calc-tax(lsub, gv-tax-rate)
            lstat = format-status(tt-order.order-status)
            tt-order.order-total = lsub + ltax
            ptotal = ptotal + tt-order.order-total.
        if gv-verbose then
            message "Batch 49 order" tt-order.order-num "total" tt-order.order-total lstat.
    end.
end procedure.

procedure process-batch-050:
    define input  parameter pnum   as integer no-undo.
    define output parameter ptotal as decimal no-undo.
    define variable li  as integer no-undo.
    define variable lj  as integer no-undo.
    define variable lsub as decimal no-undo.
    define variable ltax as decimal no-undo.
    define variable lstat as character no-undo.

    assign ptotal = 0.
    do li = 1 to pnum:
        create tt-order.
        assign
            tt-order.order-num   = li + 50 * 1000
            tt-order.cust-num    = li modulo 25 + 1
            tt-order.order-date  = today
            tt-order.order-status      = "O"
            tt-order.order-total = 0.
        assign lsub = 0.
        do lj = 1 to 6:
            create tt-line.
            assign
                tt-line.order-num = tt-order.order-num
                tt-line.line-num  = lj
                tt-line.item-num  = lj * 100 + li
                tt-line.qty       = lj + 50 modulo 5
                tt-line.price     = lj * 2.5 + 50
                tt-line.extended  = calc-extended(tt-line.qty, tt-line.price).
            assign lsub = lsub + tt-line.extended.
        end.
        assign
            ltax  = calc-tax(lsub, gv-tax-rate)
            lstat = format-status(tt-order.order-status)
            tt-order.order-total = lsub + ltax
            ptotal = ptotal + tt-order.order-total.
        if gv-verbose then
            message "Batch 50 order" tt-order.order-num "total" tt-order.order-total lstat.
    end.
end procedure.

procedure process-batch-051:
    define input  parameter pnum   as integer no-undo.
    define output parameter ptotal as decimal no-undo.
    define variable li  as integer no-undo.
    define variable lj  as integer no-undo.
    define variable lsub as decimal no-undo.
    define variable ltax as decimal no-undo.
    define variable lstat as character no-undo.

    assign ptotal = 0.
    do li = 1 to pnum:
        create tt-order.
        assign
            tt-order.order-num   = li + 51 * 1000
            tt-order.cust-num    = li modulo 25 + 1
            tt-order.order-date  = today
            tt-order.order-status      = "O"
            tt-order.order-total = 0.
        assign lsub = 0.
        do lj = 1 to 6:
            create tt-line.
            assign
                tt-line.order-num = tt-order.order-num
                tt-line.line-num  = lj
                tt-line.item-num  = lj * 100 + li
                tt-line.qty       = lj + 51 modulo 5
                tt-line.price     = lj * 2.5 + 51
                tt-line.extended  = calc-extended(tt-line.qty, tt-line.price).
            assign lsub = lsub + tt-line.extended.
        end.
        assign
            ltax  = calc-tax(lsub, gv-tax-rate)
            lstat = format-status(tt-order.order-status)
            tt-order.order-total = lsub + ltax
            ptotal = ptotal + tt-order.order-total.
        if gv-verbose then
            message "Batch 51 order" tt-order.order-num "total" tt-order.order-total lstat.
    end.
end procedure.

procedure process-batch-052:
    define input  parameter pnum   as integer no-undo.
    define output parameter ptotal as decimal no-undo.
    define variable li  as integer no-undo.
    define variable lj  as integer no-undo.
    define variable lsub as decimal no-undo.
    define variable ltax as decimal no-undo.
    define variable lstat as character no-undo.

    assign ptotal = 0.
    do li = 1 to pnum:
        create tt-order.
        assign
            tt-order.order-num   = li + 52 * 1000
            tt-order.cust-num    = li modulo 25 + 1
            tt-order.order-date  = today
            tt-order.order-status      = "O"
            tt-order.order-total = 0.
        assign lsub = 0.
        do lj = 1 to 6:
            create tt-line.
            assign
                tt-line.order-num = tt-order.order-num
                tt-line.line-num  = lj
                tt-line.item-num  = lj * 100 + li
                tt-line.qty       = lj + 52 modulo 5
                tt-line.price     = lj * 2.5 + 52
                tt-line.extended  = calc-extended(tt-line.qty, tt-line.price).
            assign lsub = lsub + tt-line.extended.
        end.
        assign
            ltax  = calc-tax(lsub, gv-tax-rate)
            lstat = format-status(tt-order.order-status)
            tt-order.order-total = lsub + ltax
            ptotal = ptotal + tt-order.order-total.
        if gv-verbose then
            message "Batch 52 order" tt-order.order-num "total" tt-order.order-total lstat.
    end.
end procedure.

procedure process-batch-053:
    define input  parameter pnum   as integer no-undo.
    define output parameter ptotal as decimal no-undo.
    define variable li  as integer no-undo.
    define variable lj  as integer no-undo.
    define variable lsub as decimal no-undo.
    define variable ltax as decimal no-undo.
    define variable lstat as character no-undo.

    assign ptotal = 0.
    do li = 1 to pnum:
        create tt-order.
        assign
            tt-order.order-num   = li + 53 * 1000
            tt-order.cust-num    = li modulo 25 + 1
            tt-order.order-date  = today
            tt-order.order-status      = "O"
            tt-order.order-total = 0.
        assign lsub = 0.
        do lj = 1 to 6:
            create tt-line.
            assign
                tt-line.order-num = tt-order.order-num
                tt-line.line-num  = lj
                tt-line.item-num  = lj * 100 + li
                tt-line.qty       = lj + 53 modulo 5
                tt-line.price     = lj * 2.5 + 53
                tt-line.extended  = calc-extended(tt-line.qty, tt-line.price).
            assign lsub = lsub + tt-line.extended.
        end.
        assign
            ltax  = calc-tax(lsub, gv-tax-rate)
            lstat = format-status(tt-order.order-status)
            tt-order.order-total = lsub + ltax
            ptotal = ptotal + tt-order.order-total.
        if gv-verbose then
            message "Batch 53 order" tt-order.order-num "total" tt-order.order-total lstat.
    end.
end procedure.

procedure process-batch-054:
    define input  parameter pnum   as integer no-undo.
    define output parameter ptotal as decimal no-undo.
    define variable li  as integer no-undo.
    define variable lj  as integer no-undo.
    define variable lsub as decimal no-undo.
    define variable ltax as decimal no-undo.
    define variable lstat as character no-undo.

    assign ptotal = 0.
    do li = 1 to pnum:
        create tt-order.
        assign
            tt-order.order-num   = li + 54 * 1000
            tt-order.cust-num    = li modulo 25 + 1
            tt-order.order-date  = today
            tt-order.order-status      = "O"
            tt-order.order-total = 0.
        assign lsub = 0.
        do lj = 1 to 6:
            create tt-line.
            assign
                tt-line.order-num = tt-order.order-num
                tt-line.line-num  = lj
                tt-line.item-num  = lj * 100 + li
                tt-line.qty       = lj + 54 modulo 5
                tt-line.price     = lj * 2.5 + 54
                tt-line.extended  = calc-extended(tt-line.qty, tt-line.price).
            assign lsub = lsub + tt-line.extended.
        end.
        assign
            ltax  = calc-tax(lsub, gv-tax-rate)
            lstat = format-status(tt-order.order-status)
            tt-order.order-total = lsub + ltax
            ptotal = ptotal + tt-order.order-total.
        if gv-verbose then
            message "Batch 54 order" tt-order.order-num "total" tt-order.order-total lstat.
    end.
end procedure.

procedure process-batch-055:
    define input  parameter pnum   as integer no-undo.
    define output parameter ptotal as decimal no-undo.
    define variable li  as integer no-undo.
    define variable lj  as integer no-undo.
    define variable lsub as decimal no-undo.
    define variable ltax as decimal no-undo.
    define variable lstat as character no-undo.

    assign ptotal = 0.
    do li = 1 to pnum:
        create tt-order.
        assign
            tt-order.order-num   = li + 55 * 1000
            tt-order.cust-num    = li modulo 25 + 1
            tt-order.order-date  = today
            tt-order.order-status      = "O"
            tt-order.order-total = 0.
        assign lsub = 0.
        do lj = 1 to 6:
            create tt-line.
            assign
                tt-line.order-num = tt-order.order-num
                tt-line.line-num  = lj
                tt-line.item-num  = lj * 100 + li
                tt-line.qty       = lj + 55 modulo 5
                tt-line.price     = lj * 2.5 + 55
                tt-line.extended  = calc-extended(tt-line.qty, tt-line.price).
            assign lsub = lsub + tt-line.extended.
        end.
        assign
            ltax  = calc-tax(lsub, gv-tax-rate)
            lstat = format-status(tt-order.order-status)
            tt-order.order-total = lsub + ltax
            ptotal = ptotal + tt-order.order-total.
        if gv-verbose then
            message "Batch 55 order" tt-order.order-num "total" tt-order.order-total lstat.
    end.
end procedure.

procedure process-batch-056:
    define input  parameter pnum   as integer no-undo.
    define output parameter ptotal as decimal no-undo.
    define variable li  as integer no-undo.
    define variable lj  as integer no-undo.
    define variable lsub as decimal no-undo.
    define variable ltax as decimal no-undo.
    define variable lstat as character no-undo.

    assign ptotal = 0.
    do li = 1 to pnum:
        create tt-order.
        assign
            tt-order.order-num   = li + 56 * 1000
            tt-order.cust-num    = li modulo 25 + 1
            tt-order.order-date  = today
            tt-order.order-status      = "O"
            tt-order.order-total = 0.
        assign lsub = 0.
        do lj = 1 to 6:
            create tt-line.
            assign
                tt-line.order-num = tt-order.order-num
                tt-line.line-num  = lj
                tt-line.item-num  = lj * 100 + li
                tt-line.qty       = lj + 56 modulo 5
                tt-line.price     = lj * 2.5 + 56
                tt-line.extended  = calc-extended(tt-line.qty, tt-line.price).
            assign lsub = lsub + tt-line.extended.
        end.
        assign
            ltax  = calc-tax(lsub, gv-tax-rate)
            lstat = format-status(tt-order.order-status)
            tt-order.order-total = lsub + ltax
            ptotal = ptotal + tt-order.order-total.
        if gv-verbose then
            message "Batch 56 order" tt-order.order-num "total" tt-order.order-total lstat.
    end.
end procedure.

procedure process-batch-057:
    define input  parameter pnum   as integer no-undo.
    define output parameter ptotal as decimal no-undo.
    define variable li  as integer no-undo.
    define variable lj  as integer no-undo.
    define variable lsub as decimal no-undo.
    define variable ltax as decimal no-undo.
    define variable lstat as character no-undo.

    assign ptotal = 0.
    do li = 1 to pnum:
        create tt-order.
        assign
            tt-order.order-num   = li + 57 * 1000
            tt-order.cust-num    = li modulo 25 + 1
            tt-order.order-date  = today
            tt-order.order-status      = "O"
            tt-order.order-total = 0.
        assign lsub = 0.
        do lj = 1 to 6:
            create tt-line.
            assign
                tt-line.order-num = tt-order.order-num
                tt-line.line-num  = lj
                tt-line.item-num  = lj * 100 + li
                tt-line.qty       = lj + 57 modulo 5
                tt-line.price     = lj * 2.5 + 57
                tt-line.extended  = calc-extended(tt-line.qty, tt-line.price).
            assign lsub = lsub + tt-line.extended.
        end.
        assign
            ltax  = calc-tax(lsub, gv-tax-rate)
            lstat = format-status(tt-order.order-status)
            tt-order.order-total = lsub + ltax
            ptotal = ptotal + tt-order.order-total.
        if gv-verbose then
            message "Batch 57 order" tt-order.order-num "total" tt-order.order-total lstat.
    end.
end procedure.

procedure process-batch-058:
    define input  parameter pnum   as integer no-undo.
    define output parameter ptotal as decimal no-undo.
    define variable li  as integer no-undo.
    define variable lj  as integer no-undo.
    define variable lsub as decimal no-undo.
    define variable ltax as decimal no-undo.
    define variable lstat as character no-undo.

    assign ptotal = 0.
    do li = 1 to pnum:
        create tt-order.
        assign
            tt-order.order-num   = li + 58 * 1000
            tt-order.cust-num    = li modulo 25 + 1
            tt-order.order-date  = today
            tt-order.order-status      = "O"
            tt-order.order-total = 0.
        assign lsub = 0.
        do lj = 1 to 6:
            create tt-line.
            assign
                tt-line.order-num = tt-order.order-num
                tt-line.line-num  = lj
                tt-line.item-num  = lj * 100 + li
                tt-line.qty       = lj + 58 modulo 5
                tt-line.price     = lj * 2.5 + 58
                tt-line.extended  = calc-extended(tt-line.qty, tt-line.price).
            assign lsub = lsub + tt-line.extended.
        end.
        assign
            ltax  = calc-tax(lsub, gv-tax-rate)
            lstat = format-status(tt-order.order-status)
            tt-order.order-total = lsub + ltax
            ptotal = ptotal + tt-order.order-total.
        if gv-verbose then
            message "Batch 58 order" tt-order.order-num "total" tt-order.order-total lstat.
    end.
end procedure.

procedure process-batch-059:
    define input  parameter pnum   as integer no-undo.
    define output parameter ptotal as decimal no-undo.
    define variable li  as integer no-undo.
    define variable lj  as integer no-undo.
    define variable lsub as decimal no-undo.
    define variable ltax as decimal no-undo.
    define variable lstat as character no-undo.

    assign ptotal = 0.
    do li = 1 to pnum:
        create tt-order.
        assign
            tt-order.order-num   = li + 59 * 1000
            tt-order.cust-num    = li modulo 25 + 1
            tt-order.order-date  = today
            tt-order.order-status      = "O"
            tt-order.order-total = 0.
        assign lsub = 0.
        do lj = 1 to 6:
            create tt-line.
            assign
                tt-line.order-num = tt-order.order-num
                tt-line.line-num  = lj
                tt-line.item-num  = lj * 100 + li
                tt-line.qty       = lj + 59 modulo 5
                tt-line.price     = lj * 2.5 + 59
                tt-line.extended  = calc-extended(tt-line.qty, tt-line.price).
            assign lsub = lsub + tt-line.extended.
        end.
        assign
            ltax  = calc-tax(lsub, gv-tax-rate)
            lstat = format-status(tt-order.order-status)
            tt-order.order-total = lsub + ltax
            ptotal = ptotal + tt-order.order-total.
        if gv-verbose then
            message "Batch 59 order" tt-order.order-num "total" tt-order.order-total lstat.
    end.
end procedure.

procedure process-batch-060:
    define input  parameter pnum   as integer no-undo.
    define output parameter ptotal as decimal no-undo.
    define variable li  as integer no-undo.
    define variable lj  as integer no-undo.
    define variable lsub as decimal no-undo.
    define variable ltax as decimal no-undo.
    define variable lstat as character no-undo.

    assign ptotal = 0.
    do li = 1 to pnum:
        create tt-order.
        assign
            tt-order.order-num   = li + 60 * 1000
            tt-order.cust-num    = li modulo 25 + 1
            tt-order.order-date  = today
            tt-order.order-status      = "O"
            tt-order.order-total = 0.
        assign lsub = 0.
        do lj = 1 to 6:
            create tt-line.
            assign
                tt-line.order-num = tt-order.order-num
                tt-line.line-num  = lj
                tt-line.item-num  = lj * 100 + li
                tt-line.qty       = lj + 60 modulo 5
                tt-line.price     = lj * 2.5 + 60
                tt-line.extended  = calc-extended(tt-line.qty, tt-line.price).
            assign lsub = lsub + tt-line.extended.
        end.
        assign
            ltax  = calc-tax(lsub, gv-tax-rate)
            lstat = format-status(tt-order.order-status)
            tt-order.order-total = lsub + ltax
            ptotal = ptotal + tt-order.order-total.
        if gv-verbose then
            message "Batch 60 order" tt-order.order-num "total" tt-order.order-total lstat.
    end.
end procedure.

procedure process-batch-061:
    define input  parameter pnum   as integer no-undo.
    define output parameter ptotal as decimal no-undo.
    define variable li  as integer no-undo.
    define variable lj  as integer no-undo.
    define variable lsub as decimal no-undo.
    define variable ltax as decimal no-undo.
    define variable lstat as character no-undo.

    assign ptotal = 0.
    do li = 1 to pnum:
        create tt-order.
        assign
            tt-order.order-num   = li + 61 * 1000
            tt-order.cust-num    = li modulo 25 + 1
            tt-order.order-date  = today
            tt-order.order-status      = "O"
            tt-order.order-total = 0.
        assign lsub = 0.
        do lj = 1 to 6:
            create tt-line.
            assign
                tt-line.order-num = tt-order.order-num
                tt-line.line-num  = lj
                tt-line.item-num  = lj * 100 + li
                tt-line.qty       = lj + 61 modulo 5
                tt-line.price     = lj * 2.5 + 61
                tt-line.extended  = calc-extended(tt-line.qty, tt-line.price).
            assign lsub = lsub + tt-line.extended.
        end.
        assign
            ltax  = calc-tax(lsub, gv-tax-rate)
            lstat = format-status(tt-order.order-status)
            tt-order.order-total = lsub + ltax
            ptotal = ptotal + tt-order.order-total.
        if gv-verbose then
            message "Batch 61 order" tt-order.order-num "total" tt-order.order-total lstat.
    end.
end procedure.

procedure process-batch-062:
    define input  parameter pnum   as integer no-undo.
    define output parameter ptotal as decimal no-undo.
    define variable li  as integer no-undo.
    define variable lj  as integer no-undo.
    define variable lsub as decimal no-undo.
    define variable ltax as decimal no-undo.
    define variable lstat as character no-undo.

    assign ptotal = 0.
    do li = 1 to pnum:
        create tt-order.
        assign
            tt-order.order-num   = li + 62 * 1000
            tt-order.cust-num    = li modulo 25 + 1
            tt-order.order-date  = today
            tt-order.order-status      = "O"
            tt-order.order-total = 0.
        assign lsub = 0.
        do lj = 1 to 6:
            create tt-line.
            assign
                tt-line.order-num = tt-order.order-num
                tt-line.line-num  = lj
                tt-line.item-num  = lj * 100 + li
                tt-line.qty       = lj + 62 modulo 5
                tt-line.price     = lj * 2.5 + 62
                tt-line.extended  = calc-extended(tt-line.qty, tt-line.price).
            assign lsub = lsub + tt-line.extended.
        end.
        assign
            ltax  = calc-tax(lsub, gv-tax-rate)
            lstat = format-status(tt-order.order-status)
            tt-order.order-total = lsub + ltax
            ptotal = ptotal + tt-order.order-total.
        if gv-verbose then
            message "Batch 62 order" tt-order.order-num "total" tt-order.order-total lstat.
    end.
end procedure.

procedure process-batch-063:
    define input  parameter pnum   as integer no-undo.
    define output parameter ptotal as decimal no-undo.
    define variable li  as integer no-undo.
    define variable lj  as integer no-undo.
    define variable lsub as decimal no-undo.
    define variable ltax as decimal no-undo.
    define variable lstat as character no-undo.

    assign ptotal = 0.
    do li = 1 to pnum:
        create tt-order.
        assign
            tt-order.order-num   = li + 63 * 1000
            tt-order.cust-num    = li modulo 25 + 1
            tt-order.order-date  = today
            tt-order.order-status      = "O"
            tt-order.order-total = 0.
        assign lsub = 0.
        do lj = 1 to 6:
            create tt-line.
            assign
                tt-line.order-num = tt-order.order-num
                tt-line.line-num  = lj
                tt-line.item-num  = lj * 100 + li
                tt-line.qty       = lj + 63 modulo 5
                tt-line.price     = lj * 2.5 + 63
                tt-line.extended  = calc-extended(tt-line.qty, tt-line.price).
            assign lsub = lsub + tt-line.extended.
        end.
        assign
            ltax  = calc-tax(lsub, gv-tax-rate)
            lstat = format-status(tt-order.order-status)
            tt-order.order-total = lsub + ltax
            ptotal = ptotal + tt-order.order-total.
        if gv-verbose then
            message "Batch 63 order" tt-order.order-num "total" tt-order.order-total lstat.
    end.
end procedure.

procedure process-batch-064:
    define input  parameter pnum   as integer no-undo.
    define output parameter ptotal as decimal no-undo.
    define variable li  as integer no-undo.
    define variable lj  as integer no-undo.
    define variable lsub as decimal no-undo.
    define variable ltax as decimal no-undo.
    define variable lstat as character no-undo.

    assign ptotal = 0.
    do li = 1 to pnum:
        create tt-order.
        assign
            tt-order.order-num   = li + 64 * 1000
            tt-order.cust-num    = li modulo 25 + 1
            tt-order.order-date  = today
            tt-order.order-status      = "O"
            tt-order.order-total = 0.
        assign lsub = 0.
        do lj = 1 to 6:
            create tt-line.
            assign
                tt-line.order-num = tt-order.order-num
                tt-line.line-num  = lj
                tt-line.item-num  = lj * 100 + li
                tt-line.qty       = lj + 64 modulo 5
                tt-line.price     = lj * 2.5 + 64
                tt-line.extended  = calc-extended(tt-line.qty, tt-line.price).
            assign lsub = lsub + tt-line.extended.
        end.
        assign
            ltax  = calc-tax(lsub, gv-tax-rate)
            lstat = format-status(tt-order.order-status)
            tt-order.order-total = lsub + ltax
            ptotal = ptotal + tt-order.order-total.
        if gv-verbose then
            message "Batch 64 order" tt-order.order-num "total" tt-order.order-total lstat.
    end.
end procedure.

procedure process-batch-065:
    define input  parameter pnum   as integer no-undo.
    define output parameter ptotal as decimal no-undo.
    define variable li  as integer no-undo.
    define variable lj  as integer no-undo.
    define variable lsub as decimal no-undo.
    define variable ltax as decimal no-undo.
    define variable lstat as character no-undo.

    assign ptotal = 0.
    do li = 1 to pnum:
        create tt-order.
        assign
            tt-order.order-num   = li + 65 * 1000
            tt-order.cust-num    = li modulo 25 + 1
            tt-order.order-date  = today
            tt-order.order-status      = "O"
            tt-order.order-total = 0.
        assign lsub = 0.
        do lj = 1 to 6:
            create tt-line.
            assign
                tt-line.order-num = tt-order.order-num
                tt-line.line-num  = lj
                tt-line.item-num  = lj * 100 + li
                tt-line.qty       = lj + 65 modulo 5
                tt-line.price     = lj * 2.5 + 65
                tt-line.extended  = calc-extended(tt-line.qty, tt-line.price).
            assign lsub = lsub + tt-line.extended.
        end.
        assign
            ltax  = calc-tax(lsub, gv-tax-rate)
            lstat = format-status(tt-order.order-status)
            tt-order.order-total = lsub + ltax
            ptotal = ptotal + tt-order.order-total.
        if gv-verbose then
            message "Batch 65 order" tt-order.order-num "total" tt-order.order-total lstat.
    end.
end procedure.

procedure process-batch-066:
    define input  parameter pnum   as integer no-undo.
    define output parameter ptotal as decimal no-undo.
    define variable li  as integer no-undo.
    define variable lj  as integer no-undo.
    define variable lsub as decimal no-undo.
    define variable ltax as decimal no-undo.
    define variable lstat as character no-undo.

    assign ptotal = 0.
    do li = 1 to pnum:
        create tt-order.
        assign
            tt-order.order-num   = li + 66 * 1000
            tt-order.cust-num    = li modulo 25 + 1
            tt-order.order-date  = today
            tt-order.order-status      = "O"
            tt-order.order-total = 0.
        assign lsub = 0.
        do lj = 1 to 6:
            create tt-line.
            assign
                tt-line.order-num = tt-order.order-num
                tt-line.line-num  = lj
                tt-line.item-num  = lj * 100 + li
                tt-line.qty       = lj + 66 modulo 5
                tt-line.price     = lj * 2.5 + 66
                tt-line.extended  = calc-extended(tt-line.qty, tt-line.price).
            assign lsub = lsub + tt-line.extended.
        end.
        assign
            ltax  = calc-tax(lsub, gv-tax-rate)
            lstat = format-status(tt-order.order-status)
            tt-order.order-total = lsub + ltax
            ptotal = ptotal + tt-order.order-total.
        if gv-verbose then
            message "Batch 66 order" tt-order.order-num "total" tt-order.order-total lstat.
    end.
end procedure.

procedure process-batch-067:
    define input  parameter pnum   as integer no-undo.
    define output parameter ptotal as decimal no-undo.
    define variable li  as integer no-undo.
    define variable lj  as integer no-undo.
    define variable lsub as decimal no-undo.
    define variable ltax as decimal no-undo.
    define variable lstat as character no-undo.

    assign ptotal = 0.
    do li = 1 to pnum:
        create tt-order.
        assign
            tt-order.order-num   = li + 67 * 1000
            tt-order.cust-num    = li modulo 25 + 1
            tt-order.order-date  = today
            tt-order.order-status      = "O"
            tt-order.order-total = 0.
        assign lsub = 0.
        do lj = 1 to 6:
            create tt-line.
            assign
                tt-line.order-num = tt-order.order-num
                tt-line.line-num  = lj
                tt-line.item-num  = lj * 100 + li
                tt-line.qty       = lj + 67 modulo 5
                tt-line.price     = lj * 2.5 + 67
                tt-line.extended  = calc-extended(tt-line.qty, tt-line.price).
            assign lsub = lsub + tt-line.extended.
        end.
        assign
            ltax  = calc-tax(lsub, gv-tax-rate)
            lstat = format-status(tt-order.order-status)
            tt-order.order-total = lsub + ltax
            ptotal = ptotal + tt-order.order-total.
        if gv-verbose then
            message "Batch 67 order" tt-order.order-num "total" tt-order.order-total lstat.
    end.
end procedure.

procedure process-batch-068:
    define input  parameter pnum   as integer no-undo.
    define output parameter ptotal as decimal no-undo.
    define variable li  as integer no-undo.
    define variable lj  as integer no-undo.
    define variable lsub as decimal no-undo.
    define variable ltax as decimal no-undo.
    define variable lstat as character no-undo.

    assign ptotal = 0.
    do li = 1 to pnum:
        create tt-order.
        assign
            tt-order.order-num   = li + 68 * 1000
            tt-order.cust-num    = li modulo 25 + 1
            tt-order.order-date  = today
            tt-order.order-status      = "O"
            tt-order.order-total = 0.
        assign lsub = 0.
        do lj = 1 to 6:
            create tt-line.
            assign
                tt-line.order-num = tt-order.order-num
                tt-line.line-num  = lj
                tt-line.item-num  = lj * 100 + li
                tt-line.qty       = lj + 68 modulo 5
                tt-line.price     = lj * 2.5 + 68
                tt-line.extended  = calc-extended(tt-line.qty, tt-line.price).
            assign lsub = lsub + tt-line.extended.
        end.
        assign
            ltax  = calc-tax(lsub, gv-tax-rate)
            lstat = format-status(tt-order.order-status)
            tt-order.order-total = lsub + ltax
            ptotal = ptotal + tt-order.order-total.
        if gv-verbose then
            message "Batch 68 order" tt-order.order-num "total" tt-order.order-total lstat.
    end.
end procedure.

procedure process-batch-069:
    define input  parameter pnum   as integer no-undo.
    define output parameter ptotal as decimal no-undo.
    define variable li  as integer no-undo.
    define variable lj  as integer no-undo.
    define variable lsub as decimal no-undo.
    define variable ltax as decimal no-undo.
    define variable lstat as character no-undo.

    assign ptotal = 0.
    do li = 1 to pnum:
        create tt-order.
        assign
            tt-order.order-num   = li + 69 * 1000
            tt-order.cust-num    = li modulo 25 + 1
            tt-order.order-date  = today
            tt-order.order-status      = "O"
            tt-order.order-total = 0.
        assign lsub = 0.
        do lj = 1 to 6:
            create tt-line.
            assign
                tt-line.order-num = tt-order.order-num
                tt-line.line-num  = lj
                tt-line.item-num  = lj * 100 + li
                tt-line.qty       = lj + 69 modulo 5
                tt-line.price     = lj * 2.5 + 69
                tt-line.extended  = calc-extended(tt-line.qty, tt-line.price).
            assign lsub = lsub + tt-line.extended.
        end.
        assign
            ltax  = calc-tax(lsub, gv-tax-rate)
            lstat = format-status(tt-order.order-status)
            tt-order.order-total = lsub + ltax
            ptotal = ptotal + tt-order.order-total.
        if gv-verbose then
            message "Batch 69 order" tt-order.order-num "total" tt-order.order-total lstat.
    end.
end procedure.

procedure process-batch-070:
    define input  parameter pnum   as integer no-undo.
    define output parameter ptotal as decimal no-undo.
    define variable li  as integer no-undo.
    define variable lj  as integer no-undo.
    define variable lsub as decimal no-undo.
    define variable ltax as decimal no-undo.
    define variable lstat as character no-undo.

    assign ptotal = 0.
    do li = 1 to pnum:
        create tt-order.
        assign
            tt-order.order-num   = li + 70 * 1000
            tt-order.cust-num    = li modulo 25 + 1
            tt-order.order-date  = today
            tt-order.order-status      = "O"
            tt-order.order-total = 0.
        assign lsub = 0.
        do lj = 1 to 6:
            create tt-line.
            assign
                tt-line.order-num = tt-order.order-num
                tt-line.line-num  = lj
                tt-line.item-num  = lj * 100 + li
                tt-line.qty       = lj + 70 modulo 5
                tt-line.price     = lj * 2.5 + 70
                tt-line.extended  = calc-extended(tt-line.qty, tt-line.price).
            assign lsub = lsub + tt-line.extended.
        end.
        assign
            ltax  = calc-tax(lsub, gv-tax-rate)
            lstat = format-status(tt-order.order-status)
            tt-order.order-total = lsub + ltax
            ptotal = ptotal + tt-order.order-total.
        if gv-verbose then
            message "Batch 70 order" tt-order.order-num "total" tt-order.order-total lstat.
    end.
end procedure.

procedure process-batch-071:
    define input  parameter pnum   as integer no-undo.
    define output parameter ptotal as decimal no-undo.
    define variable li  as integer no-undo.
    define variable lj  as integer no-undo.
    define variable lsub as decimal no-undo.
    define variable ltax as decimal no-undo.
    define variable lstat as character no-undo.

    assign ptotal = 0.
    do li = 1 to pnum:
        create tt-order.
        assign
            tt-order.order-num   = li + 71 * 1000
            tt-order.cust-num    = li modulo 25 + 1
            tt-order.order-date  = today
            tt-order.order-status      = "O"
            tt-order.order-total = 0.
        assign lsub = 0.
        do lj = 1 to 6:
            create tt-line.
            assign
                tt-line.order-num = tt-order.order-num
                tt-line.line-num  = lj
                tt-line.item-num  = lj * 100 + li
                tt-line.qty       = lj + 71 modulo 5
                tt-line.price     = lj * 2.5 + 71
                tt-line.extended  = calc-extended(tt-line.qty, tt-line.price).
            assign lsub = lsub + tt-line.extended.
        end.
        assign
            ltax  = calc-tax(lsub, gv-tax-rate)
            lstat = format-status(tt-order.order-status)
            tt-order.order-total = lsub + ltax
            ptotal = ptotal + tt-order.order-total.
        if gv-verbose then
            message "Batch 71 order" tt-order.order-num "total" tt-order.order-total lstat.
    end.
end procedure.

procedure process-batch-072:
    define input  parameter pnum   as integer no-undo.
    define output parameter ptotal as decimal no-undo.
    define variable li  as integer no-undo.
    define variable lj  as integer no-undo.
    define variable lsub as decimal no-undo.
    define variable ltax as decimal no-undo.
    define variable lstat as character no-undo.

    assign ptotal = 0.
    do li = 1 to pnum:
        create tt-order.
        assign
            tt-order.order-num   = li + 72 * 1000
            tt-order.cust-num    = li modulo 25 + 1
            tt-order.order-date  = today
            tt-order.order-status      = "O"
            tt-order.order-total = 0.
        assign lsub = 0.
        do lj = 1 to 6:
            create tt-line.
            assign
                tt-line.order-num = tt-order.order-num
                tt-line.line-num  = lj
                tt-line.item-num  = lj * 100 + li
                tt-line.qty       = lj + 72 modulo 5
                tt-line.price     = lj * 2.5 + 72
                tt-line.extended  = calc-extended(tt-line.qty, tt-line.price).
            assign lsub = lsub + tt-line.extended.
        end.
        assign
            ltax  = calc-tax(lsub, gv-tax-rate)
            lstat = format-status(tt-order.order-status)
            tt-order.order-total = lsub + ltax
            ptotal = ptotal + tt-order.order-total.
        if gv-verbose then
            message "Batch 72 order" tt-order.order-num "total" tt-order.order-total lstat.
    end.
end procedure.

procedure process-batch-073:
    define input  parameter pnum   as integer no-undo.
    define output parameter ptotal as decimal no-undo.
    define variable li  as integer no-undo.
    define variable lj  as integer no-undo.
    define variable lsub as decimal no-undo.
    define variable ltax as decimal no-undo.
    define variable lstat as character no-undo.

    assign ptotal = 0.
    do li = 1 to pnum:
        create tt-order.
        assign
            tt-order.order-num   = li + 73 * 1000
            tt-order.cust-num    = li modulo 25 + 1
            tt-order.order-date  = today
            tt-order.order-status      = "O"
            tt-order.order-total = 0.
        assign lsub = 0.
        do lj = 1 to 6:
            create tt-line.
            assign
                tt-line.order-num = tt-order.order-num
                tt-line.line-num  = lj
                tt-line.item-num  = lj * 100 + li
                tt-line.qty       = lj + 73 modulo 5
                tt-line.price     = lj * 2.5 + 73
                tt-line.extended  = calc-extended(tt-line.qty, tt-line.price).
            assign lsub = lsub + tt-line.extended.
        end.
        assign
            ltax  = calc-tax(lsub, gv-tax-rate)
            lstat = format-status(tt-order.order-status)
            tt-order.order-total = lsub + ltax
            ptotal = ptotal + tt-order.order-total.
        if gv-verbose then
            message "Batch 73 order" tt-order.order-num "total" tt-order.order-total lstat.
    end.
end procedure.

procedure process-batch-074:
    define input  parameter pnum   as integer no-undo.
    define output parameter ptotal as decimal no-undo.
    define variable li  as integer no-undo.
    define variable lj  as integer no-undo.
    define variable lsub as decimal no-undo.
    define variable ltax as decimal no-undo.
    define variable lstat as character no-undo.

    assign ptotal = 0.
    do li = 1 to pnum:
        create tt-order.
        assign
            tt-order.order-num   = li + 74 * 1000
            tt-order.cust-num    = li modulo 25 + 1
            tt-order.order-date  = today
            tt-order.order-status      = "O"
            tt-order.order-total = 0.
        assign lsub = 0.
        do lj = 1 to 6:
            create tt-line.
            assign
                tt-line.order-num = tt-order.order-num
                tt-line.line-num  = lj
                tt-line.item-num  = lj * 100 + li
                tt-line.qty       = lj + 74 modulo 5
                tt-line.price     = lj * 2.5 + 74
                tt-line.extended  = calc-extended(tt-line.qty, tt-line.price).
            assign lsub = lsub + tt-line.extended.
        end.
        assign
            ltax  = calc-tax(lsub, gv-tax-rate)
            lstat = format-status(tt-order.order-status)
            tt-order.order-total = lsub + ltax
            ptotal = ptotal + tt-order.order-total.
        if gv-verbose then
            message "Batch 74 order" tt-order.order-num "total" tt-order.order-total lstat.
    end.
end procedure.

procedure process-batch-075:
    define input  parameter pnum   as integer no-undo.
    define output parameter ptotal as decimal no-undo.
    define variable li  as integer no-undo.
    define variable lj  as integer no-undo.
    define variable lsub as decimal no-undo.
    define variable ltax as decimal no-undo.
    define variable lstat as character no-undo.

    assign ptotal = 0.
    do li = 1 to pnum:
        create tt-order.
        assign
            tt-order.order-num   = li + 75 * 1000
            tt-order.cust-num    = li modulo 25 + 1
            tt-order.order-date  = today
            tt-order.order-status      = "O"
            tt-order.order-total = 0.
        assign lsub = 0.
        do lj = 1 to 6:
            create tt-line.
            assign
                tt-line.order-num = tt-order.order-num
                tt-line.line-num  = lj
                tt-line.item-num  = lj * 100 + li
                tt-line.qty       = lj + 75 modulo 5
                tt-line.price     = lj * 2.5 + 75
                tt-line.extended  = calc-extended(tt-line.qty, tt-line.price).
            assign lsub = lsub + tt-line.extended.
        end.
        assign
            ltax  = calc-tax(lsub, gv-tax-rate)
            lstat = format-status(tt-order.order-status)
            tt-order.order-total = lsub + ltax
            ptotal = ptotal + tt-order.order-total.
        if gv-verbose then
            message "Batch 75 order" tt-order.order-num "total" tt-order.order-total lstat.
    end.
end procedure.

procedure process-batch-076:
    define input  parameter pnum   as integer no-undo.
    define output parameter ptotal as decimal no-undo.
    define variable li  as integer no-undo.
    define variable lj  as integer no-undo.
    define variable lsub as decimal no-undo.
    define variable ltax as decimal no-undo.
    define variable lstat as character no-undo.

    assign ptotal = 0.
    do li = 1 to pnum:
        create tt-order.
        assign
            tt-order.order-num   = li + 76 * 1000
            tt-order.cust-num    = li modulo 25 + 1
            tt-order.order-date  = today
            tt-order.order-status      = "O"
            tt-order.order-total = 0.
        assign lsub = 0.
        do lj = 1 to 6:
            create tt-line.
            assign
                tt-line.order-num = tt-order.order-num
                tt-line.line-num  = lj
                tt-line.item-num  = lj * 100 + li
                tt-line.qty       = lj + 76 modulo 5
                tt-line.price     = lj * 2.5 + 76
                tt-line.extended  = calc-extended(tt-line.qty, tt-line.price).
            assign lsub = lsub + tt-line.extended.
        end.
        assign
            ltax  = calc-tax(lsub, gv-tax-rate)
            lstat = format-status(tt-order.order-status)
            tt-order.order-total = lsub + ltax
            ptotal = ptotal + tt-order.order-total.
        if gv-verbose then
            message "Batch 76 order" tt-order.order-num "total" tt-order.order-total lstat.
    end.
end procedure.

procedure process-batch-077:
    define input  parameter pnum   as integer no-undo.
    define output parameter ptotal as decimal no-undo.
    define variable li  as integer no-undo.
    define variable lj  as integer no-undo.
    define variable lsub as decimal no-undo.
    define variable ltax as decimal no-undo.
    define variable lstat as character no-undo.

    assign ptotal = 0.
    do li = 1 to pnum:
        create tt-order.
        assign
            tt-order.order-num   = li + 77 * 1000
            tt-order.cust-num    = li modulo 25 + 1
            tt-order.order-date  = today
            tt-order.order-status      = "O"
            tt-order.order-total = 0.
        assign lsub = 0.
        do lj = 1 to 6:
            create tt-line.
            assign
                tt-line.order-num = tt-order.order-num
                tt-line.line-num  = lj
                tt-line.item-num  = lj * 100 + li
                tt-line.qty       = lj + 77 modulo 5
                tt-line.price     = lj * 2.5 + 77
                tt-line.extended  = calc-extended(tt-line.qty, tt-line.price).
            assign lsub = lsub + tt-line.extended.
        end.
        assign
            ltax  = calc-tax(lsub, gv-tax-rate)
            lstat = format-status(tt-order.order-status)
            tt-order.order-total = lsub + ltax
            ptotal = ptotal + tt-order.order-total.
        if gv-verbose then
            message "Batch 77 order" tt-order.order-num "total" tt-order.order-total lstat.
    end.
end procedure.

procedure process-batch-078:
    define input  parameter pnum   as integer no-undo.
    define output parameter ptotal as decimal no-undo.
    define variable li  as integer no-undo.
    define variable lj  as integer no-undo.
    define variable lsub as decimal no-undo.
    define variable ltax as decimal no-undo.
    define variable lstat as character no-undo.

    assign ptotal = 0.
    do li = 1 to pnum:
        create tt-order.
        assign
            tt-order.order-num   = li + 78 * 1000
            tt-order.cust-num    = li modulo 25 + 1
            tt-order.order-date  = today
            tt-order.order-status      = "O"
            tt-order.order-total = 0.
        assign lsub = 0.
        do lj = 1 to 6:
            create tt-line.
            assign
                tt-line.order-num = tt-order.order-num
                tt-line.line-num  = lj
                tt-line.item-num  = lj * 100 + li
                tt-line.qty       = lj + 78 modulo 5
                tt-line.price     = lj * 2.5 + 78
                tt-line.extended  = calc-extended(tt-line.qty, tt-line.price).
            assign lsub = lsub + tt-line.extended.
        end.
        assign
            ltax  = calc-tax(lsub, gv-tax-rate)
            lstat = format-status(tt-order.order-status)
            tt-order.order-total = lsub + ltax
            ptotal = ptotal + tt-order.order-total.
        if gv-verbose then
            message "Batch 78 order" tt-order.order-num "total" tt-order.order-total lstat.
    end.
end procedure.

procedure process-batch-079:
    define input  parameter pnum   as integer no-undo.
    define output parameter ptotal as decimal no-undo.
    define variable li  as integer no-undo.
    define variable lj  as integer no-undo.
    define variable lsub as decimal no-undo.
    define variable ltax as decimal no-undo.
    define variable lstat as character no-undo.

    assign ptotal = 0.
    do li = 1 to pnum:
        create tt-order.
        assign
            tt-order.order-num   = li + 79 * 1000
            tt-order.cust-num    = li modulo 25 + 1
            tt-order.order-date  = today
            tt-order.order-status      = "O"
            tt-order.order-total = 0.
        assign lsub = 0.
        do lj = 1 to 6:
            create tt-line.
            assign
                tt-line.order-num = tt-order.order-num
                tt-line.line-num  = lj
                tt-line.item-num  = lj * 100 + li
                tt-line.qty       = lj + 79 modulo 5
                tt-line.price     = lj * 2.5 + 79
                tt-line.extended  = calc-extended(tt-line.qty, tt-line.price).
            assign lsub = lsub + tt-line.extended.
        end.
        assign
            ltax  = calc-tax(lsub, gv-tax-rate)
            lstat = format-status(tt-order.order-status)
            tt-order.order-total = lsub + ltax
            ptotal = ptotal + tt-order.order-total.
        if gv-verbose then
            message "Batch 79 order" tt-order.order-num "total" tt-order.order-total lstat.
    end.
end procedure.

procedure process-batch-080:
    define input  parameter pnum   as integer no-undo.
    define output parameter ptotal as decimal no-undo.
    define variable li  as integer no-undo.
    define variable lj  as integer no-undo.
    define variable lsub as decimal no-undo.
    define variable ltax as decimal no-undo.
    define variable lstat as character no-undo.

    assign ptotal = 0.
    do li = 1 to pnum:
        create tt-order.
        assign
            tt-order.order-num   = li + 80 * 1000
            tt-order.cust-num    = li modulo 25 + 1
            tt-order.order-date  = today
            tt-order.order-status      = "O"
            tt-order.order-total = 0.
        assign lsub = 0.
        do lj = 1 to 6:
            create tt-line.
            assign
                tt-line.order-num = tt-order.order-num
                tt-line.line-num  = lj
                tt-line.item-num  = lj * 100 + li
                tt-line.qty       = lj + 80 modulo 5
                tt-line.price     = lj * 2.5 + 80
                tt-line.extended  = calc-extended(tt-line.qty, tt-line.price).
            assign lsub = lsub + tt-line.extended.
        end.
        assign
            ltax  = calc-tax(lsub, gv-tax-rate)
            lstat = format-status(tt-order.order-status)
            tt-order.order-total = lsub + ltax
            ptotal = ptotal + tt-order.order-total.
        if gv-verbose then
            message "Batch 80 order" tt-order.order-num "total" tt-order.order-total lstat.
    end.
end procedure.

/* Main block */
assign gv-user = "batch" gv-verbose = false.

run process-batch-001(input 8, output gv-acc).
assign gv-count = gv-count + 1.
run process-batch-002(input 8, output gv-acc).
assign gv-count = gv-count + 1.
run process-batch-003(input 8, output gv-acc).
assign gv-count = gv-count + 1.
run process-batch-004(input 8, output gv-acc).
assign gv-count = gv-count + 1.
run process-batch-005(input 8, output gv-acc).
assign gv-count = gv-count + 1.
run process-batch-006(input 8, output gv-acc).
assign gv-count = gv-count + 1.
run process-batch-007(input 8, output gv-acc).
assign gv-count = gv-count + 1.
run process-batch-008(input 8, output gv-acc).
assign gv-count = gv-count + 1.
run process-batch-009(input 8, output gv-acc).
assign gv-count = gv-count + 1.
run process-batch-010(input 8, output gv-acc).
assign gv-count = gv-count + 1.
run process-batch-011(input 8, output gv-acc).
assign gv-count = gv-count + 1.
run process-batch-012(input 8, output gv-acc).
assign gv-count = gv-count + 1.
run process-batch-013(input 8, output gv-acc).
assign gv-count = gv-count + 1.
run process-batch-014(input 8, output gv-acc).
assign gv-count = gv-count + 1.
run process-batch-015(input 8, output gv-acc).
assign gv-count = gv-count + 1.
run process-batch-016(input 8, output gv-acc).
assign gv-count = gv-count + 1.
run process-batch-017(input 8, output gv-acc).
assign gv-count = gv-count + 1.
run process-batch-018(input 8, output gv-acc).
assign gv-count = gv-count + 1.
run process-batch-019(input 8, output gv-acc).
assign gv-count = gv-count + 1.
run process-batch-020(input 8, output gv-acc).
assign gv-count = gv-count + 1.
run process-batch-021(input 8, output gv-acc).
assign gv-count = gv-count + 1.
run process-batch-022(input 8, output gv-acc).
assign gv-count = gv-count + 1.
run process-batch-023(input 8, output gv-acc).
assign gv-count = gv-count + 1.
run process-batch-024(input 8, output gv-acc).
assign gv-count = gv-count + 1.
run process-batch-025(input 8, output gv-acc).
assign gv-count = gv-count + 1.
run process-batch-026(input 8, output gv-acc).
assign gv-count = gv-count + 1.
run process-batch-027(input 8, output gv-acc).
assign gv-count = gv-count + 1.
run process-batch-028(input 8, output gv-acc).
assign gv-count = gv-count + 1.
run process-batch-029(input 8, output gv-acc).
assign gv-count = gv-count + 1.
run process-batch-030(input 8, output gv-acc).
assign gv-count = gv-count + 1.
run process-batch-031(input 8, output gv-acc).
assign gv-count = gv-count + 1.
run process-batch-032(input 8, output gv-acc).
assign gv-count = gv-count + 1.
run process-batch-033(input 8, output gv-acc).
assign gv-count = gv-count + 1.
run process-batch-034(input 8, output gv-acc).
assign gv-count = gv-count + 1.
run process-batch-035(input 8, output gv-acc).
assign gv-count = gv-count + 1.
run process-batch-036(input 8, output gv-acc).
assign gv-count = gv-count + 1.
run process-batch-037(input 8, output gv-acc).
assign gv-count = gv-count + 1.
run process-batch-038(input 8, output gv-acc).
assign gv-count = gv-count + 1.
run process-batch-039(input 8, output gv-acc).
assign gv-count = gv-count + 1.
run process-batch-040(input 8, output gv-acc).
assign gv-count = gv-count + 1.
run process-batch-041(input 8, output gv-acc).
assign gv-count = gv-count + 1.
run process-batch-042(input 8, output gv-acc).
assign gv-count = gv-count + 1.
run process-batch-043(input 8, output gv-acc).
assign gv-count = gv-count + 1.
run process-batch-044(input 8, output gv-acc).
assign gv-count = gv-count + 1.
run process-batch-045(input 8, output gv-acc).
assign gv-count = gv-count + 1.
run process-batch-046(input 8, output gv-acc).
assign gv-count = gv-count + 1.
run process-batch-047(input 8, output gv-acc).
assign gv-count = gv-count + 1.
run process-batch-048(input 8, output gv-acc).
assign gv-count = gv-count + 1.
run process-batch-049(input 8, output gv-acc).
assign gv-count = gv-count + 1.
run process-batch-050(input 8, output gv-acc).
assign gv-count = gv-count + 1.
run process-batch-051(input 8, output gv-acc).
assign gv-count = gv-count + 1.
run process-batch-052(input 8, output gv-acc).
assign gv-count = gv-count + 1.
run process-batch-053(input 8, output gv-acc).
assign gv-count = gv-count + 1.
run process-batch-054(input 8, output gv-acc).
assign gv-count = gv-count + 1.
run process-batch-055(input 8, output gv-acc).
assign gv-count = gv-count + 1.
run process-batch-056(input 8, output gv-acc).
assign gv-count = gv-count + 1.
run process-batch-057(input 8, output gv-acc).
assign gv-count = gv-count + 1.
run process-batch-058(input 8, output gv-acc).
assign gv-count = gv-count + 1.
run process-batch-059(input 8, output gv-acc).
assign gv-count = gv-count + 1.
run process-batch-060(input 8, output gv-acc).
assign gv-count = gv-count + 1.
run process-batch-061(input 8, output gv-acc).
assign gv-count = gv-count + 1.
run process-batch-062(input 8, output gv-acc).
assign gv-count = gv-count + 1.
run process-batch-063(input 8, output gv-acc).
assign gv-count = gv-count + 1.
run process-batch-064(input 8, output gv-acc).
assign gv-count = gv-count + 1.
run process-batch-065(input 8, output gv-acc).
assign gv-count = gv-count + 1.
run process-batch-066(input 8, output gv-acc).
assign gv-count = gv-count + 1.
run process-batch-067(input 8, output gv-acc).
assign gv-count = gv-count + 1.
run process-batch-068(input 8, output gv-acc).
assign gv-count = gv-count + 1.
run process-batch-069(input 8, output gv-acc).
assign gv-count = gv-count + 1.
run process-batch-070(input 8, output gv-acc).
assign gv-count = gv-count + 1.
run process-batch-071(input 8, output gv-acc).
assign gv-count = gv-count + 1.
run process-batch-072(input 8, output gv-acc).
assign gv-count = gv-count + 1.
run process-batch-073(input 8, output gv-acc).
assign gv-count = gv-count + 1.
run process-batch-074(input 8, output gv-acc).
assign gv-count = gv-count + 1.
run process-batch-075(input 8, output gv-acc).
assign gv-count = gv-count + 1.
run process-batch-076(input 8, output gv-acc).
assign gv-count = gv-count + 1.
run process-batch-077(input 8, output gv-acc).
assign gv-count = gv-count + 1.
run process-batch-078(input 8, output gv-acc).
assign gv-count = gv-count + 1.
run process-batch-079(input 8, output gv-acc).
assign gv-count = gv-count + 1.
run process-batch-080(input 8, output gv-acc).
assign gv-count = gv-count + 1.

for each tt-order where tt-order.order-total > 0:
    assign gv-acc = gv-acc + tt-order.order-total.
end.
message "Processed" gv-count "batches for module" gv-module "acc" gv-acc.
