/* Request values that are not an identity decide paging and sorting. */
IF get-value("page") = "2" THEN
    RUN next-page.
CASE get-field("sort"):
    WHEN "name" THEN RUN sort-by-name.
END CASE.
