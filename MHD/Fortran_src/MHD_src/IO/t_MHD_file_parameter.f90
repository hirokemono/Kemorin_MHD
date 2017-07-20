!> @file  t_MHD_file_parameter.f90
!!      module t_MHD_file_parameter
!!
!! @author  H. Matsui
!! @date Programmed in June, 2013
!
!> @brief Structure for file information (prefix, format, IO flag)
!!
!!@verbatim
!!@endverbatim
!
      module t_MHD_file_parameter
!
      use m_precision
      use t_file_IO_parameter
!
      implicit none
!
!
!>      Structure of file name and format for MHD
      type MHD_file_IO_params
!>        Structure of file name and format for restart file
        type(field_IO_params) :: fst_file_IO
!>        Structure of file name and format for model coefficients
        type(field_IO_params) :: Csim_file_IO
!>        Structure of file name and format for commutation coefficients
        type(field_IO_params) :: Cdiff_file_IO
!
!>        Structure of file name and format for restart file
        type(field_IO_params) :: ucd_file_IO
      end type MHD_file_IO_params
!
      end module t_MHD_file_parameter
