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
!ht_prop1%coef_nega_adv
!
!>     coefficient for time evolution of temperature and heat flux
!      real  (kind=kreal) :: coef_temp
!>     coefficient for heat flux (-coef_temp)
!      real  (kind=kreal) :: coef_nega_t
!>     coefficient for time evolution of composition and composition flux
      real  (kind=kreal) :: coef_light
!>     coefficient for composition flux (-coef_light)
      real  (kind=kreal) :: coef_nega_c
!
!>     coefficient for thermal diffusion
      real  (kind=kreal) :: coef_d_temp
!>     coefficient for chemical diffusion
      real  (kind=kreal) :: coef_d_light
!
!>     coefficient for heat source term
      real  (kind=kreal) :: coef_h_src
!>     coefficient for compositional source term
      real  (kind=kreal) :: coef_c_src
!
!>     Parameter for stratified layer (amplitude)
      real  (kind=kreal) :: stratified_sigma
!>     Parameter for stratified layer (thckness)
      real  (kind=kreal) :: stratified_width
!>     Parameter for stratified layer (radius)
      real  (kind=kreal) :: stratified_outer_r
!
      end module m_physical_property
