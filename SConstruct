#!/bin/python
""" Top level build script for code """

#####################################
#  Import necessary python modules  #
#####################################
import os
import sys


# Skip a line to make standard output a little easier on the eyes
print ""

#  Append an option to this list to add a new option to the build script
optionList = [
               PathVariable('install_dir', 'Location where the TRACE executable is installed', 'bin', PathVariable.PathIsDirCreate),
               EnumVariable('fc', 'Fortran compiler to use. Supported compilers are', '', ['gfortran']),
               EnumVariable('cc', 'C compiler to use. Supported compilers are', '', ['gcc']),
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

# What platform are we building on?
platform = 'darwin'
fcompiler = env['fc']
ccompiler = env['cc']
target_arch = 'x64'
buildType = 'debug'
projectType='shared'


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

build_root = os.getcwd()+os.sep+'build' + os.sep + platform +os.sep
common_build_dir = build_root + 'common'
trace_build_dir = build_root + 'src'
absPath = os.getcwd()+os.sep

#  Make these directory names available to all other SConscript files
Export([
        'fcompiler',
        'opts',
        'trace_build_dir',
        'common_build_dir',
        'fortranFlags',
        'absPath'
      ])

####################
#  Build the code  #
####################

common_target  = env.SConscript('common/SConscript',  variant_dir=common_build_dir,  exports='env', duplicate=1)
trace_target   = env.SConscript('src/SConscript',   variant_dir=trace_build_dir,   exports='env',
                                duplicate=1)

################################################################
#  Install the library in a directory that is easier to query  #
#  get at than the deeply-nested build directories             #
################################################################
install_dir = env['install_dir']    # User can override the default install directory
env.Install(install_dir, [trace_target, common_target])
env.Alias('install', install_dir)
