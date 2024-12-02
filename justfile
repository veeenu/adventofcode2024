today:
  just r $(date +%d)

input:
  mkdir -p _input
  dune exec ./input.exe
  ls _input

r day:
  dune exec ./day{{day}}.exe 
