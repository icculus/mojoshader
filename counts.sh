#!/bin/sh

for feh in ??_?_?/*.bytecode ; do
    echo -n "$feh "
    echo -n `./testparse glsl $feh |grep "INSTRUCTION COUNT: " |perl -w -p -e 's/\AINSTRUCTION COUNT://;'`
    echo -n " vs. "
    grep "instruction slots used" `echo $feh |perl -w -p -e 's/bytecode\Z/disasm/;'` |perl -w -p -e 's#\A// approximately (\d+) instruction slots used .*?\Z#$1#;'
done

