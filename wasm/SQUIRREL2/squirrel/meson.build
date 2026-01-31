squirrel_srcs = files(
  'sqapi.cpp',
  'sqbaselib.cpp',
  'sqfuncstate.cpp',
  'sqdebug.cpp',
  'sqlexer.cpp',
  'sqobject.cpp',
  'sqcompiler.cpp',
  'sqstate.cpp',
  'sqtable.cpp',
  'sqmem.cpp',
  'sqvm.cpp',
  'sqclass.cpp',
)

squirrel_cpp_args = []

if get_option('sq64')
  squirrel_cpp_args += '-D_SQ64'
endif

lib_squirrel = static_library('squirrel',
  squirrel_srcs,
  cpp_args : squirrel_cpp_args,
  include_directories : include_directories('.', '../include'),
  install : false,
)

squirrel = declare_dependency(
  link_with : lib_squirrel,
  include_directories : include_directories('../include'),
)
