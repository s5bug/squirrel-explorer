project('squirrelexplorer', ['c', 'cpp'],
  default_options : [
    'c_std=gnu23',
    'cpp_std=gnu++23',
	'cpp_rtti=false',
    'warning_level=1',
  ],
  meson_version : '>= 1.9.0'
)

subdir('SQUIRREL2')

if host_machine.system() == 'emscripten'
  additional_c_args = []
  if get_option('c_std') == 'none'
    additional_c_args += '-std=gnu23'
  else
    additional_c_args += ('-std=' + get_option('c_std'))
  endif

  additional_cpp_args = []
  if get_option('cpp_std') == 'none'
    additional_cpp_args += '-std=gnu++23'
  else
    additional_cpp_args += ('-std=' + get_option('cpp_std'))
  endif
  if get_option('cpp_rtti') == false
    additional_cpp_args += '-fno-rtti'
  endif

  em_common_args = [
    '-O2',
    '-s', 'WASM=1',
    '-s', 'MODULARIZE=1',
    '-s', 'EXPORT_ES6=1',
  ]
  api_link_args = [
    '-s', 'EXPORTED_FUNCTIONS=_sq_open,_sq_close,_compile_and_serialize_buffer,_out_buffer_init,_out_buffer_close,_out_buffer_content,_free',
    '-s', 'EXPORTED_RUNTIME_METHODS=stringToNewUTF8,lengthBytesUTF8,UTF8ToString,HEAPU8'
  ]
  dragonbox_link_args = [
    '-s', 'EXPORTED_FUNCTIONS=_realloc,_free,_bulk_dragonbox',
    '-s', 'EXPORTED_RUNTIME_METHODS=HEAP32,HEAPU32,HEAPF32'
  ]
  
  # depending on squirrel, because Meson doesn't support depending on a declare_dependency in a custom_target:
  # - lib_squirrel in input
  # - SQUIRREL2/include in -I
  api_js = custom_target('api',
    input : ['api.c', lib_squirrel],
    output : ['api.js', 'api.wasm'],
    command : [
      meson.get_compiler('c').cmd_array(),
      '@INPUT@',
	  additional_c_args,
      get_option('c_args'),
      get_option('c_link_args'),
      em_common_args,
      api_link_args,
	  '-I' + (meson.current_source_dir() / 'SQUIRREL2/include'),
      '-o', '@OUTPUT0@',
    ],
    install : true,
    install_dir : get_option('bindir'),
	install_tag : 'wasm',
	console : true,
  )
  
  dragonbox_js = custom_target('dragonbox',
    input : 'dragonbox.cpp',
	output : ['dragonbox.js', 'dragonbox.wasm'],
	command : [
      meson.get_compiler('c').cmd_array(),
      '@INPUT@',
	  additional_cpp_args,
      get_option('cpp_args'),
      get_option('cpp_link_args'),
      em_common_args,
      dragonbox_link_args,
      '-o', '@OUTPUT0@',
    ],
    install : true,
    install_dir : get_option('bindir'),
	install_tag : 'wasm',
	console : true,
  )
endif
