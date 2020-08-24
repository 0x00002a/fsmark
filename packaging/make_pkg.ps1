mkdir pkg_out
stack build --test --copy-bins --local-bin-path pkg_out
cd pkg_out

cp ../packaging/fsm.nsi .
makensis ./fsm.nsi
cp fsm-installer.exe ..
cp fsm.exe ..
cd ..
rm -r pkg_out

