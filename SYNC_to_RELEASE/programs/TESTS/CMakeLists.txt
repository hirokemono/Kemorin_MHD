SET(COMPARE_PSF Compare/compare_psf_files.f90 Compare/t_ctl_param_psf_compares.f90 Compare/t_ctl_data_psf_compares.f90 Compare/t_ctl_data_psf_compare.f90)
ADD_EXECUTABLE(tests/compare_psf ${COMPARE_PSF})
TARGET_LINK_LIBRARIES(calypso calypso_c)


SET(FFTPACK_TEST FFT_test/t_fft_test_data.f90 FFT_test/test_FFTPACK5.f90)
ADD_EXECUTABLE(tests/test_FFTPACK ${FFTPACK_TEST})
TARGET_LINK_LIBRARIES(calypso calypso_c)

SET(FFW3_F_TEST FFT_test/t_fft_test_data.f90 FFT_test/test_FFTW3.f90)
ADD_EXECUTABLE(tests/test_FFTW3_f ${FFW3_F_TEST})
TARGET_LINK_LIBRARIES(calypso calypso_c)

SET(SGL_FFW3_TEST FFT_test/t_fft_test_data.f90 FFT_test/test_single_FFTW3.f90)
ADD_EXECUTABLE(tests/test_single_FFTW3 ${SGL_FFW3_TEST})
TARGET_LINK_LIBRARIES(calypso calypso_c)

SET(COMPARE_FFT FFT_test/t_fft_test_data.f90 FFT_test/compare_FFT_test.f90)
ADD_EXECUTABLE(tests/compare_fft_test ${COMPARE_FFT})
TARGET_LINK_LIBRARIES(calypso calypso_c)


SET(GAUSS_TEST Gauss_point_test/gauss_points_IO.f90 Gauss_point_test/test_gauss_points.f90)
ADD_EXECUTABLE(tests/Gauss_point_test ${GAUSS_TEST})
TARGET_LINK_LIBRARIES(calypso calypso_c)

SET(CMP_GAUSS_TEST Gauss_point_test/gauss_points_IO.f90 Gauss_point_test/compare_gauss_points.f90)
ADD_EXECUTABLE(tests/compare_gauss_points ${CMP_GAUSS_TEST})
TARGET_LINK_LIBRARIES(calypso calypso_c)


SET(SMDT_POLY_TEST Legendre_test/t_spherical_harmonics.f90 Legendre_test/schmidt_poly_on_meridian.f90 Legendre_test/t_spherical_harmonics.f90)
ADD_EXECUTABLE(tests/Legendre_test ${SMDT_POLY_TEST})
TARGET_LINK_LIBRARIES(calypso calypso_c)

