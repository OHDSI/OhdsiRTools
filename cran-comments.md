Resubmission: 
- Removed quotes around 'WebAPI'.
- Changed directional quotation marks to undirected ones.
- Added URL to WebAPI in the description.
- Added multiple executable examples.
- I did not replace \dontrun with \donttest, because these examples either modify package files, or require access to an active WebAPI server or mail server. Setting them to \donttest would mean they are executed when calling devtools:release(), causing the process to stop. I did delete some of the \dontrun examples.
- Vignette no long writes to home space.
- Using 'snow' instead of 'parallel' because we need several low-level functions that are exported by 'snow' but not by 'parallel' (e.g. 'sendCall' and 'recvOneResult'). Note that 'parallel' itself also calls the 'snow' package.

---

## Test environments
* Ubuntu 14.04.5 LTS (Travis), R 3.5.0
* Windows 7, R 3.4.4

## R CMD check results

There were no ERRORs or WARNINGs. 

## Downstream dependencies

There are no downstream dependencies.