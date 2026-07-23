/* Synthetic include: utility functions used by the main program. */

function calc-extended returns decimal
    (input pq as integer, input pp as decimal):
    return pq * pp.
end function.

function calc-tax returns decimal
    (input pamount as decimal, input prate as decimal):
    return round(pamount * prate, 2).
end function.

function format-status returns character
    (input pcode as character):
    define variable lresult as character no-undo.
    case pcode:
        when "O" then lresult = "Open".
        when "S" then lresult = "Shipped".
        when "C" then lresult = "Closed".
        otherwise lresult = "Unknown".
    end case.
    return lresult.
end function.
