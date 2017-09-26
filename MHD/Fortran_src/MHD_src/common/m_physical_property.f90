!>@file   m_physical_property.f90
!!@brief  module m_physical_property
!!
!!@author H. Matsui
!!@date Programmed in 2001
!!@date Modified in Jan., 2007
!
!>@brief  Coeffiecients of each term
!
      module m_physical_property
!
      use m_precision
      use t_control_parameter
      use t_boundary_data_sph_MHD
!
      implicit  none
!
!
      type(MHD_evolution_param), save :: MHD_prop1
!
      type(sph_MHD_boundary_data), save :: sph_MHD_bc1
!
      end module m_physical_property
