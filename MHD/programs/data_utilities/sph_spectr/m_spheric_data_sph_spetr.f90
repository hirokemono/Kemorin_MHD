!>@file   m_spheric_data_sph_spetr.f90
!!@brief  module m_spheric_data_sph_spetr
!!
!!@author H. Matsui
!!@date Programmed on July, 2007
!!
!!@brief  indexing table of speherical harmonics transform
!!
!!@verbatim
!!      subroutine allocate_spheric_parameter
!!      subroutine deallocate_spheric_parameter
!!      subroutine deallocate_sph_param_smp
!!
!!      subroutine check_global_spheric_parameter
!!@endverbatim
!!
!
      module m_spheric_data_sph_spetr
!
      use m_precision
      use t_spheric_mesh
      use t_phys_data
      use t_field_data_IO
!
      implicit none
!
!>  Structure of grid and spectr data for spherical spectr method
      type(sph_mesh_data), save :: sph_mesh_spec
!
!>  Structure for field data
      type(phys_data), save :: rj_fld_spec
!
!>  Structure for field data IO
     type(field_IO), save :: sph_spec_IO
!
      end module m_spheric_data_sph_spetr
