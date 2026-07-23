/* Synthetic include: shared temp-table and variable definitions.
   Models the include-resident declarations a real editing buffer relies on. */

define temp-table tt-order no-undo
    field order-num   as integer
    field cust-num    as integer
    field order-date  as date
    field ship-date   as date
    field order-total  as decimal
    field order-status as character
    index idx-order is primary unique order-num
    index idx-cust cust-num.

define temp-table tt-line no-undo
    field order-num as integer
    field line-num  as integer
    field item-num  as integer
    field qty       as integer
    field price     as decimal
    field extended  as decimal
    index idx-line is primary unique order-num line-num.

define variable gv-module   as character no-undo initial "order-entry".
define variable gv-user     as character no-undo.
define variable gv-tax-rate as decimal   no-undo initial 0.08.
define variable gv-verbose  as logical   no-undo initial false.
