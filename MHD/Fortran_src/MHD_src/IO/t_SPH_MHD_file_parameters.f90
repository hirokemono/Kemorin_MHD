!>@file   t_SPH_MHD_file_parameters.f90
!!@brief  module t_SPH_MHD_file_parameters
!!
!!@author H. Matsui
!!@date Programmed in Nov., 2009
!
!>@brief Array for file parameters for spherical shell dynamo
!!
!!@verbatim
!!      subroutine set_control_org_sph_files(MHD_files)
!!@endverbatim
!!
      module t_SPH_MHD_file_parameters
!
      use m_precision
      use m_constants
      use t_field_data_IO
!
      implicit  none
!
      type file_params_4_sph_mhd
!>        Structure for field data IO paramters
        type(field_IO_params) :: rj_file_param
!>        Structure for field data IO paramters
        type(field_IO_params) :: udt_file_param
!>        Structure for original restart file  paramters
        type(field_IO_params) :: rst_file_param
      end type file_params_4_sph_mhd
!
!------------------------------------------------------------------
!
       contains
!
!------------------------------------------------------------------
!
      subroutine set_control_org_sph_files(MHD_files)
!
      use set_ctl_params_2nd_files
!
      type(file_params_4_sph_mhd), intent(inout) :: MHD_files
!
!
      call set_control_org_sph_mesh(MHD_files%rj_file_param)
      call set_control_org_rst_file_def(MHD_files%rst_file_param)
      call set_control_org_udt_file_def(MHD_files%udt_file_param)
!
      end subroutine set_control_org_sph_files
!
!------------------------------------------------------------------
!
      end module t_SPH_MHD_file_parameters
