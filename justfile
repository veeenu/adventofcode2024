@today:
  just r $(date +%d)

@input:
  mkdir -p _input
  dune exec ./input.exe
  ls _input
  grep day$(date +%d) dune || echo -e "\n(executable\n  (name day$(date +%d)))" >> dune
  touch day$(date +%d).ml
  dune build

@r day:
  dune exec ./day{{day}}.exe 
