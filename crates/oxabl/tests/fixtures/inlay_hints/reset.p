/* A synthetic program whose main block is the transaction. */
FIND FIRST invoice WHERE invoice.invnum = 1 EXCLUSIVE-LOCK.
invoice.amount = 0.
MESSAGE "done".
