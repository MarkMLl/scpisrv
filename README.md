# scpisrv
This is a minimal SCPI server, intended to be embedded in a program that reads from or controls a bench instrument. See e.g. https://github.com/MarkMLl/Mastech_ms2115b for a slightly more comprehensive example.

This is definitely not comprehensive, but is able to parse SCPI commands (Program Messages), expand short to long node names, fill in omitted root references, and then pass each Program Message Unit to a registered handler.
