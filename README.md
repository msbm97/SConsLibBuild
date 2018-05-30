# SConsLibBuild
A test code to demonstrate how to build a shared library with absolute paths on MacOSX using scons

The input to these scripts are:
scons fc=gfortran cc=gcc install install_dir=lib

See: 

https://wincent.com/wiki/@executable_path,_@load_path_and_@rpath

https://developer.apple.com/library/content/documentation/DeveloperTools/Conceptual/DynamicLibraries/100-Articles/RunpathDependentLibraries.html

https://github.com/conda/conda-build/issues/279

https://stackoverflow.com/questions/10021428/macos-how-to-link-a-dynamic-library-with-a-relative-path-using-gcc-ld

https://stackoverflow.com/questions/9263256/why-is-install-name-tool-and-otool-necessary-for-mach-o-libraries-in-mac-os-x

https://blogs.oracle.com/dipol/dynamic-libraries,-rpath,-and-mac-os

Basically on mac the -o <path>/libabc.dylib gets written into the file as it's install name.
You can check this using otool -D <path>/libabc.dylib

You can use -install_name "@rpath/libabc.dylib" to change the name to that, and then (according to docs) the linker will use @rpath plus the rpath specified when you link an exe to get the file from the right path.

Can't test yet as no program in example.
