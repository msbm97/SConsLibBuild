#!/bin/python
""" Top level build script for code """

#####################################
#  Import necessary python modules  #
#####################################
import os
import sys

import atexit

# Skip a line to make standard output a little easier on the eyes
print ""

#################################################
#  Make sure the correct SCons version is used  #
#################################################
EnsureSConsVersion(0,96)

#################################################
#  Supported build platforms and compilers      #
#################################################

#################################################
#  Establish allowed command line options       #
#################################################

#  Append an option to this list to add a new option to the build script
optionList = [
               BoolVariable('release', 'Set to 1 to build optimized code versions', 0),
               BoolVariable('verbose', 'Dump the construction environment to the screen for debug purposes', 0),
               BoolVariable('libBuild', 'Build Trace as a static library for integration into MOOSE', 0),
               BoolVariable('dynamicLibBuild', 'Build Trace library as a dynamic library for integration into MOOSE', 0),
               PathVariable('install_dir', 'Location where the TRACE executable is installed', 'bin', PathVariable.PathIsDirCreate),
               EnumVariable('fc', 'Fortran compiler to use. Supported compilers are', '', ['',
                                                                                           'lahey',
                                                                                           'nag',
                                                                                           'ifort12',
                                                                                           'ifort16',
                                                                                           'ifort17',
                                                                                           'intel',
                                                                                           'gfortran',
                                                                                           'g95']),
               EnumVariable('cc', 'C compiler to use. Supported compilers are', '', ['',
                                                                                     'msvc10',
                                                                                     'msvc14',
                                                                                     'gcc',
                                                                                     'clang',
                                                                                    ]),
               EnumVariable('arch', 'Architecture of the build.  Supported architecture types are', '', ['',
                                                                                                         'x86',
                                                                                                         'x64'
                                                                                                        ])
             ]


#  Register the command line options allowed by this script
opts = Variables()
for option in optionList:
  opts.Add(option)

#################################################
#  Establish a default contruction environment  #
#################################################

env = Environment(options=opts)              # Propagate options through to environment
                                             # as construction variables


####################################
#  Process command line arguments  #
####################################

# What platform are we building on?
platform = env['PLATFORM']



# What compiler environment and architecture should we use to build the code.
fcompiler = env['fc']
ccompiler = env['cc']
target_arch = env['arch']

# Determine if the code should be compiled in optimized or debug mode
if env['release'] == 1:
   buildType = 'release'
else:
   buildType = 'debug'

# Determine if the code should compile as a static library
if env['libBuild'] == 1:
   if env['dynamicLibBuild']==1:
      projectType='shared'
   else:
      projectType = 'static'
else:
   projectType = 'exe'


#  Establish the default target to be built, if the user does not specify it on the command line
Default(['src',])

env = Environment(options = opts, tools=['fortran', 'f90', 'default'])
env.Append(ENV = {'PATH' : os.environ['PATH']})
env['F90'] = 'gfortran'
env.Tool('gnulink')
env['F90FLAGS'] = '-fno-underscoring'
env['FORTRANMODDIRPREFIX'] = '-J'
fortranFlags = ['-g']
env['LINKFLAGS'] = '-ISystemStubs'

##################################################
#  Establish the names of the build directories  #
##################################################

build_root = os.getcwd()+ os.sep+ 'build' + os.sep + platform +os.sep
common_build_dir = build_root + 'common'
trace_build_dir = build_root + 'src'

#  Make these directory names available to all other SConscript files
Export([
        'fcompiler',
        'ccompiler',
        'opts',
        'trace_build_dir',
        'common_build_dir',
      ])

####################
#  Build the code  #
####################

common_target  = env.SConscript('common/SConscript',  variant_dir=common_build_dir,  exports='env', duplicate=1)
trace_target   = env.SConscript('src/SConscript',   variant_dir=trace_build_dir,   exports=['env', 'common_target'],
                                duplicate=1)


################################################################
#  Install the executable in a directory that is easier to     #
#  get at than the deeply-nested build directories             #
################################################################
install_dir = env['install_dir']    # User can override the default install directory
if projectType == 'exe':
   env.Install(install_dir, [trace_target])
else:
   env.Install(install_dir, [trace_target, common_target])
env.Alias('install', install_dir)
