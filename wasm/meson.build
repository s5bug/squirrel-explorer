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
  em_common_args = [
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

  api_js = executable('api', 'api.c',
    name_suffix: 'js',
    dependencies: [
      squirrel
    ],
    link_args: [em_common_args, api_link_args]
  )
  
  dragonbox_js = executable('dragonbox', 'dragonbox.cpp',
    name_suffix: 'js',
    link_args: [em_common_args, dragonbox_link_args]
  )
endif
