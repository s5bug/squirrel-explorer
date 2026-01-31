sq_srcs = files(
  'sq.c',
)

sq_cpp_args = []

if get_option('sq64')
  sq_cpp_args += '-D_SQ64'
endif

executable('sq',
  sq_srcs,
  dependencies : [squirrel, sqstdlib],
  cpp_args : sq_cpp_args,
  install : true,
  link_language : 'cpp' ,
)
