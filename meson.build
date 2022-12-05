project('ferror', 'fortran')

sources = files(
  'src/ferror_c_binding.f90',
  'src/ferror_c_implementation.f90',
  'src/ferror_implementation.f90',
  'src/ferror.f90'
)

ferror_lib = static_library('ferror', sources)
ferror_dep = declare_dependency(link_with: ferror_lib)
