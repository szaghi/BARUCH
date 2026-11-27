call doc%load_file('notes-1252.md')
print '(A,I0)', 'read as UTF-8, replacement characters (U+FFFD): ', count_replacements(doc%source())
