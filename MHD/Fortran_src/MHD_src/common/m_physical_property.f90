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
!>     external magnetic field (Constant)
      real (kind=kreal) :: ex_magne(3)
!
!   Coefficients
!
!>      Structure for fluid property
      type(fluid_property), save :: fl_prop1
!>      Structure for manetic property
      type(conductive_property), save :: cd_prop1
!cd_prop1%coef_diffuse
!
!>     coefficient for time evolution of temperature and heat flux
      real  (kind=kreal) :: coef_temp
!>     coefficient for heat flux (-coef_temp)
      real  (kind=kreal) :: coef_nega_t
!>     coefficient for time evolution of magnetic field
!      real  (kind=kreal) :: coef_magne
!>     coefficient for time electric potentia
!      real  (kind=kreal) :: coef_mag_p
!>     coefficient for time evolution of composition and composition flux
      real  (kind=kreal) :: coef_light
!>     coefficient for composition flux (-coef_light)
      real  (kind=kreal) :: coef_nega_c
!
!>     coefficient for thermal diffusion
      real  (kind=kreal) :: coef_d_temp
!>     coefficient for magnetic diffusion
!      real  (kind=kreal) :: coef_d_magne
!>     coefficient for chemical diffusion
      real  (kind=kreal) :: coef_d_light
!
!>     coefficient for magnetic induction
!      real  (kind=kreal) :: coef_induct
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
