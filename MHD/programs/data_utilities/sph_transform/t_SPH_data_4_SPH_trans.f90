!>@file   t_SPH_data_4_SPH_trans.f90
!!@brief  module t_SPH_data_4_SPH_trans
!!
!!@author H. Matsui
!!@date Programmed in June, 2006
!
!>@brief Arrays for Field data IO for spherical transform utilities
!!
!!@verbatim
!!@endverbatim
!
      module t_SPH_data_4_SPH_trans
!
      use m_precision
      use m_machine_parameter
!
      use t_FEM_data_4_SPH_trans
      use t_step_parameter
      use t_time_data
      use t_SGS_model_addresses
      use t_schmidt_poly_on_rtm
      use t_work_4_sph_trans
      use t_global_gauss_coefs
      use t_legendre_trans_select
      use t_sph_FFT_selector
      use t_phys_name_4_sph_trans
      use t_SPH_mesh_field_data
      use t_field_data_IO
      use t_file_IO_parameter
      use t_sph_radial_interpolate
!
      implicit none
!
!>      Structure of spectr structures
      type SPH_for_SPH_transforms
!>        FEM mesh IO flags
        type(FEM_file_IO_flags) :: FEM_mesh_flags
!
!>        Structure of mesh file IO paramters
        type(field_IO_params) :: sph_file_param
!>        Structure of file name and format for restart file
        type(field_IO_params) :: fst_file_IO
!>        Structure for original restart file  paramters
        type(field_IO_params) :: org_rst_file_IO
!>        Structure of file name and format for spectr data file
        type(field_IO_params) :: sph_file_IO
!>        Structure of old spherical shell mesh file
        type(field_IO_params) :: org_rj_file_IO
!
        character(len = kchara) :: cmb_radial_grp =     'CMB'
        character(len = kchara) :: icb_radial_grp =     'ICB'
!
!>        Structure for spectr data
        type(field_name_4_sph_trans) :: fld_rtp
        type(SGS_model_addresses) :: ipol_LES
!>        Structures of Gauss points
        type(global_gauss_points) :: d_gauss
!
!>        Structure for field data IO
        type(field_IO) :: fld_IO
!
!>        Structures of parameters for spherical transform
        type(parameters_4_sph_trans) :: trans_p
!>        Work structures for various Legendre trasform
        type(legendre_trns_works) :: WK_leg
!>        Structure for work area of FFTs
        type(work_for_FFTs) :: WK_FFTs
!
!>      Interpolation tsble in radial direction
        type(sph_radial_interpolate) :: rj_itp
      end type SPH_for_SPH_transforms
!
      end module t_SPH_data_4_SPH_trans
