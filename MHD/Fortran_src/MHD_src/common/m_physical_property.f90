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
!
      implicit  none
!
!
      type(MHD_evolution_param), save :: MHD_prop1
!MHD_prop1%iflag_all_scheme
!
      integer (kind=kint) :: iflag_scheme = id_Crank_nicolson
!
      end module m_physical_property
