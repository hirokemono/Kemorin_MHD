!>@file   m_SPH_transforms.f90
!!@brief  module m_SPH_transforms
!!
!!@author H. Matsui
!!@date Programmed in June, 2006
!
!>@brief Arrays for Field data IO for spherical transform utilities
!!
!!@verbatim
!!@endverbatim
!
      module m_SPH_transforms
!
      use m_precision
      use m_machine_parameter
!
      use t_FEM_data_4_SPH_trans
      use t_SPH_data_4_SPH_trans
      use t_SPH_mesh_field_data
      use t_step_parameter
      use t_file_IO_parameter
      use t_ctl_data_4_sph_trans
!
      implicit none
!
      type(FEM_for_SPH_transforms) :: FEM_STR1
      type(SPH_for_SPH_transforms) :: SPH_STR1
!
!
!       Structure for time stepping parameters
      type(time_step_param), save :: t_STR
!
      type(spherical_transform_util_ctl), save :: spt_ctl1
!
!>  Structure of grid and spectr data for spherical spectr method
      type(SPH_mesh_field_data), save :: SPH_TRNS
!
      end module m_SPH_transforms
