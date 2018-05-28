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
#
# linux2 is x86_64 linux
# darwin is MacOS X bsd layer
#
supported_fortran_compilers = {
                                'cygwin'   : ['lahey', 'g95', 'gfortran', 'ifort12', 'ifort16', 'ifort17'],
                                'win32'    : ['lahey', 'g95', 'ifort12', 'ifort16', 'ifort17'],
                                'linux'    : ['lahey', 'nag', 'intel', 'g95', 'gfortran'],
                                'linux2'   : ['lahey', 'nag', 'intel', 'g95', 'gfortran'],
                                'darwin'   : ['gfortran','g95', 'intel']
                              }

supported_c_compilers = {
                          'cygwin'  : ['gcc', 'msvc10', 'msvc14'],
                          'win32'   : ['gcc', 'msvc10', 'msvc14'],
                          'linux'   : ['gcc'],
                          'linux2'  : ['gcc'],
                          'darwin'  : ['gcc', 'clang']
                        }

default_fortran_compilers = {
                              'cygwin'   : 'ifort17',
                              'win32'    : 'ifort17',
                              'linux'    : 'lahey',
                              'linux2'   : 'lahey',
                              'darwin'   : 'intel'
                            }

default_c_compilers = {
                        'cygwin'   : 'msvc14',
                        'win32'    : 'msvc14',
                        'linux'    : 'gcc',
                        'linux2'   : 'gcc',
                        'darwin'   : 'clang'
                      }

supported_fortan_archs = {
                           'ifort12'    : ['x86','x64'],
                           'ifort16'    : ['x86','x64'],
                           'ifort17'    : ['x86','x64'],
                           'lahey'      : ['x86','x64'],
                           'nag'        : ['x86'],
                           'intel'      : ['x86', 'x86_64'],
                           'g95'        : ['x86'],
                           'gfortran'   : ['x86','x64'],
                         }

default_fortran_archs = {
                          'ifort12'    : 'x64',
                          'ifort16'    : 'x64',
                          'ifort17'    : 'x64',
                          'lahey'      : 'x64',
                          'nag'        : 'x86',
                          'intel'      : 'x86_64',
                          'g95'        : 'x86',
                          'gfortran'   : 'x64',
                        }


# Not using these right now - C architecture type is forced to be the same as
# that used for the Fortran - this makes the most sense
supported_c_archs = {
                      'msvc10'   : ['x86','x64'],
                      'msvc14'   : ['x86','x64'],
                      'gcc'      : ['x86','x64'],
                      'clang'    : ['x86','x64'],
                    }

default_c_archs = {
                    'msvc10'   : 'x64',
                    'msvc14'   : 'x64',
                    'gcc'      : 'x64',
                    'clang'    : 'x64'
                  }


# Configuration dictionaries for storing compiler version-dependent properties.
cpp_arch_strings = {
                     'msvc10' : {'x86' : ['WIN32'],
                                 'x64' : ['WIN32','WIN64'],
                                },
                     'msvc14' : {'x86' : ['WIN32'],
                                 'x64' : ['WIN32','WIN64'],
                                },
                   }

#  Rather than call os.environ directly in the statements below, we need to use
#  our own custom function to retrieve the environment variables in order to
#  guard against and trap situations where an environment variable does not
#  exist on the host computer. Otherwise, os.environ throws a KeyError

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

###########################
#  Establish help system  #
###########################

help_text = "\n" + "The following options are available when building the code (in addition to the scons built-in options)\n" + opts.GenerateHelpText(env)
Help(help_text)


####################################
#  Process command line arguments  #
####################################

# What platform are we building on?
platform = env['PLATFORM']
if platform == 'posix':
   platform = sys.platform


if env['verbose'] == 1:
   build_utils.DumpEnv(env)

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

#  Make sure the current platform is supported in the build scripts
if not supported_fortran_compilers.has_key(platform) or not supported_c_compilers.has_key(platform):
   print "SCons Programming Error: The platform you are attempting to build on is unknwon"
   Exit(1)

else:
   #  Make sure the requested fortran compiler is supported in the build scripts.  If not, use the default
   if not supported_fortran_compilers[platform].count(fcompiler):
      print "The Fortran compiler you want to use is unknown to this build system.  Using the default for this platform...."
      fcompiler = default_fortran_compilers[platform]

   #  Make sure the requested c compiler is supported in the build scripts.  If not, use the default
   if not supported_c_compilers[platform].count(ccompiler):
      print "The C compiler you want to use is unknown to this build system.  Using the default for this platform...."
      ccompiler = default_c_compilers[platform]

#  Make sure the requested target architecture is supported in the build scripts
if not supported_fortan_archs[fcompiler].count(target_arch):
   if not target_arch == '':
      print 'Requested architecture '+ target_arch + ' not available for the ' + fcompiler + ' compiler'
      Exit(1)
   target_arch = default_fortran_archs[fcompiler]


# Error checking to make sure the configuration of the cygwin platform is consistent with the chosen compilers
if platform == 'cygwin':

   # Check to make sure the path names contained in the
   # relevant environment variables actually exist for the chosen compilers
   if fcompiler == 'ifort12':
      envPath = build_utils.get_env('IFORT_COMPILER12')
      if envPath == '':
         print 'Error - you have chosen the ' + fcompiler + ' compiler, but the environment variable IFORT_COMPILER12 is not set'
         Exit(1)
      elif not os.path.isdir(envPath):
         print 'Error - you have chosen to build TRACE using version 12 of the Intel compiler, but the path name contained'
         print 'in the IFORT_COMPILER12 environment variable does not exist.  The build will not succeed.  Shutting down...'
         Exit(1)
   elif fcompiler == 'ifort16':
      envPath = build_utils.get_env('IFORT_COMPILER16')
      if envPath == '':
         print 'Error - you have chosen the ' + fcompiler + ' compiler, but the environment variable IFORT_COMPILER16 is not set'
         Exit(1)
      elif not os.path.isdir(envPath):
         print 'Error - you have chosen to build TRACE using version 16 of the Intel compiler, but the path name contained'
         print 'in the IFORT_COMPILER16 environment variable does not exist.  The build will not succeed.  Shutting down...'
         Exit(1)
   elif fcompiler == 'ifort17':
      envPath = build_utils.get_env('IFORT_COMPILER17')
      if envPath == '':
         print 'Error - you have chosen the ' + fcompiler + ' compiler, but the environment variable IFORT_COMPILER17 is not set'
         Exit(1)
      elif not os.path.isdir(envPath):
         print 'Error - you have chosen to build TRACE using version 17 of the Intel compiler, but the path name contained'
         print 'in the IFORT_COMPILER17 environment variable does not exist.  The build will not succeed.  Shutting down...'
         Exit(1)
   else:
      print 'Error - Unkown Fortran compiler string requested for the cygwin platform'
      Exit(1)

   if ccompiler == 'msvc10':
      envPath = build_utils.get_env('VS10INSTALLDIR')
      if envPath == '':
         print 'Error - you have chosen the ' + ccompiler + ' compiler, but the environment variable VS10INSTALLDIR is not set'
         Exit(1)
      elif not os.path.isdir(envPath):
         print 'Error - you have chosen to build TRACE using Microsoft Visual Studio 2010, but the path name contained'
         print 'in the VS10INSTALLDIR environment variable does not exist.  The build will not succeed.  Shutting down...'
         Exit(1)

      envPath = build_utils.get_env('VC10INSTALLDIR')
      if envPath == '':
         print 'Error - you have chosen the ' + ccompiler + ' compiler, but the environment variable VC10INSTALLDIR is not set'
         Exit(1)
      elif not os.path.isdir(envPath):
         print 'Error - you have chosen to build TRACE using Microsoft Visual Studio 2010, but the path name contained'
         print 'in the VC10INSTALLDIR environment variable does not exist.  The build will not succeed.  Shutting down...'
         Exit(1)

      envPath = build_utils.get_env('WINDOWSSDK10DIR')
      if envPath == '':
         print 'Error - you have chosen the ' + ccompiler + ' compiler, but the environment variable WINDOWSSDK10DIR is not set'
         Exit(1)
      elif not os.path.isdir(envPath):
         print 'Error - you have chosen to build TRACE using Microsoft Visual Studio 2010, but the path name contained'
         print 'in the WINDOWSSDK10DIR environment variable does not exist.  The build will not succeed.  Shutting down...'
         Exit(1)

   elif ccompiler == 'msvc14':
      envPath = build_utils.get_env('VS14INSTALLDIR')
      if envPath == '':
         print 'Error - you have chosen the ' + ccompiler + ' compiler, but the environment variable VS14INSTALLDIR is not set'
         Exit(1)
      elif not os.path.isdir(envPath):
         print 'Error - you have chosen to build TRACE using Microsoft Visual Studio 2015, but the path name contained'
         print 'in the VS14INSTALLDIR environment variable does not exist.  The build will not succeed.  Shutting down...'
         Exit(1)

      envPath = build_utils.get_env('VC14INSTALLDIR')
      if envPath == '':
         print 'Error - you have chosen the ' + ccompiler + ' compiler, but the environment variable VC14INSTALLDIR is not set'
         Exit(1)
      elif not os.path.isdir(envPath):
         print 'Error - you have chosen to build TRACE using Microsoft Visual Studio 2015, but the path name contained'
         print 'in the VC14INSTALLDIR environment variable does not exist.  The build will not succeed.  Shutting down...'
         Exit(1)

      envPath = build_utils.get_env('WINDOWSSDK14DIR')
      if envPath == '':
         print 'Error - you have chosen the ' + ccompiler + ' compiler, but the environment variable WINDOWSSDK14DIR is not set'
         Exit(1)
      elif not os.path.isdir(envPath):
         print 'Error - you have chosen to build TRACE using Microsoft Visual Studio 2015, but the path name contained'
         print 'in the WINDOWSSDK14DIR environment variable does not exist.  The build will not succeed.  Shutting down...'
         Exit(1)

      envPath = build_utils.get_env('WINDOWSKITDIR')
      if envPath == '':
         print 'Error - you have chosen the ' + ccompiler + ' compiler, but the environment variable WINDOWSKITDIR is not set'
         Exit(1)
      elif not os.path.isdir(envPath):
         print 'Error - you have chosen to build TRACE using Microsoft Visual Studio 2015, but the path name contained'
         print 'in the WINDOWSKITDIR environment variable does not exist.  The build will not succeed.  Shutting down...'
         Exit(1)

   else:
      print 'Error - Unkown C compiler string requested for the cygwin platform'
      Exit(1)

   # Make sure the correct compiler combinations are in use
   if ccompiler == 'msvc14' and fcompiler == 'ifort12':
      print 'Error - Cannot couple the MS VC++ 14.0 compiler with the Intel Fortran 2011 compiler'
      Exit(1)
   elif ccompiler == 'msvc10' and not fcompiler == 'ifort12':
      print 'Error - Cannot couple the MS VC++ 10.0 compiler with the Intel Fortran 2016 or 2017 compiler'
      Exit(1)

#  Establish the default target to be built, if the user does not specify it on the command line
Default(['src',])


##################################################
#  Establish the names of the build directories  #
##################################################

build_root = os.getcwd()+ os.sep+ 'build' + os.sep + platform + os.sep + fcompiler + os.sep + target_arch + os.sep + buildType + os.sep
tpr_build_dir = build_root + 'TPRLib'
common_build_dir = build_root + 'common'
trace_build_dir = build_root + 'src'

#  Make these directory names available to all other SConscript files
Export([
        'fcompiler',
        'ccompiler',
        'platform',
        'target_arch',
        'buildType',
        'projectType',
        'cpp_arch_strings',
        'opts',
        'tpr_build_dir',
        'trace_build_dir',
        'common_build_dir',
      ])

####################
#  Build the code  #
####################

common_target  = env.SConscript('common/SConscript',  variant_dir=common_build_dir,  exports='env', duplicate=1)
tpr_target     = env.SConscript('TPRLib/f90/SConscript',  variant_dir=tpr_build_dir, exports=['env', 'common_target'], duplicate=1)
trace_target   = env.SConscript('src/SConscript',   variant_dir=trace_build_dir,   exports=['env', 'common_target',
                                 'tpr_target'], duplicate=1)


################################################################
#  Install the executable in a directory that is easier to     #
#  get at than the deeply-nested build directories             #
################################################################
install_dir = env['install_dir']    # User can override the default install directory
if projectType == 'exe':
   env.Install(install_dir, [trace_target])
else:
   env.Install(install_dir, [trace_target, common_target, tpr_target])
env.Alias('install', install_dir)
