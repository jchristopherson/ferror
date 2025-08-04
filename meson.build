project('ferror', 'fortran', 'c')

sources = files(
  'src/ferror_c_binding.f90',
  'src/ferror.f90'
)

ferror_lib = static_library('ferror', sources)
ferror_dep = declare_dependency(link_with: ferror_lib)

fortran_test = executable('ferror_test', 'test/ferror_test.f90', link_with: ferror_lib)
test('ferror_test', fortran_test)

inc = include_directories('include')
c_test = executable('ferror_test_c', 'test/ferror_test_c.c', include_directories: inc, link_with: ferror_lib)
test('ferror_test_c', c_test)