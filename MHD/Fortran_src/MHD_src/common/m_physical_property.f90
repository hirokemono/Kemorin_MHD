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
      use t_physical_property
!
      implicit  none
!
!
!>      Structure for fluid property
      type(fluid_property), save :: fl_prop1
!>      Structure for manetic property
      type(conductive_property), save :: cd_prop1
!
!>      Structure for thermal property
      type(scalar_property), save :: ht_prop1
!>      Structure for compositon property
      type(scalar_property), save :: cp_prop1
!
!>     Parameter for stratified layer (amplitude)
      real  (kind=kreal) :: stratified_sigma
!>     Parameter for stratified layer (thckness)
      real  (kind=kreal) :: stratified_width
!>     Parameter for stratified layer (radius)
      real  (kind=kreal) :: stratified_outer_r
!
      end module m_physical_property
