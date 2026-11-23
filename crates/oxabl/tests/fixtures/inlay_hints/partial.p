/* A synthetic program with text the preprocessor did not expand. */
FIND FIRST customer WHERE customer.custnum = 1 {&lock-phrase}.
DISPLAY customer.name.
FOR EACH invoice WHERE invoice.custnum = 5 {missing.i}:
    invoice.amount = 0.
END.
