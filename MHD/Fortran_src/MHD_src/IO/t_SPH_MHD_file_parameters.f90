!>@file   t_SPH_MHD_file_parameters.f90
!!@brief  module t_SPH_MHD_file_parameters
!!
!!@author H. Matsui
!!@date Programmed in Nov., 2009
!
!>@brief Array for file parameters for spherical shell dynamo
!!
!!@verbatim
!!      subroutine set_control_org_sph_files(org_plt, MHD_files)
!!        type(platform_data_control), intent(in) :: org_plt
!!@endverbatim
!!
      module t_SPH_MHD_file_parameters
!
      use m_precision
      use m_constants
      use t_ctl_data_4_platforms
      use t_file_IO_parameter
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
      subroutine set_control_org_sph_files(org_plt, MHD_files)
!
      use m_default_file_prefix
      use set_control_platform_data
!
      type(platform_data_control), intent(in) :: org_plt
      type(file_params_4_sph_mhd), intent(inout) :: MHD_files
!
!
      call set_control_mesh_file_def                                    &
     &   (def_org_sph_rj_head, org_plt, MHD_files%rj_file_param)
      call set_control_mesh_file_def                                    &
     &   (def_org_rst_header, org_plt, MHD_files%rst_file_param)
      call set_control_mesh_file_def                                    &
     &   (def_org_ucd_header, org_plt, MHD_files%udt_file_param)
!
      end subroutine set_control_org_sph_files
!
!------------------------------------------------------------------
!
      end module t_SPH_MHD_file_parameters
