sqstdlib_srcs = files(
  'sqstdblob.cpp',
  'sqstdio.cpp',
  'sqstdstream.cpp',
  'sqstdmath.cpp',
  'sqstdsystem.cpp',
  'sqstdstring.cpp',
  'sqstdaux.cpp',
  'sqstdrex.cpp',
)

sqstdlib_cpp_args = []

if get_option('sq64')
  sqstdlib_cpp_args += '-D_SQ64'
endif

lib_sqstdlib = static_library('sqstdlib',
  sqstdlib_srcs,
  cpp_args : sqstdlib_cpp_args,
  include_directories : include_directories('.', '../include'),
  dependencies : squirrel,
  install : false,
)

sqstdlib = declare_dependency(
  link_with : lib_sqstdlib,
  include_directories : include_directories('.'),
)
