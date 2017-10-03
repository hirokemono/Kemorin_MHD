!>@file   m_spheric_data_transform.f90
!!@brief  module m_spheric_data_transform
!!
!!@author H. Matsui
!!@date Programmed on July, 2007
!!
!!@brief  indexing table of speherical harmonics transform
!!
!
      module m_spheric_data_transform
!
      use m_precision
      use t_SPH_mesh_field_data
      use t_time_data
      use t_field_data_IO
      use t_file_IO_parameter
!
      implicit none
!
!>  Structure of grid and spectr data for spherical spectr method
      type(SPH_mesh_field_data), save :: SPH_TRNS
!
!>  Structure for field data IO
     type(field_IO), save :: sph_trns_IO
!
      end module m_spheric_data_transform
