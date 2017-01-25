!>@file   t_physical_property.f90
!!@brief  module t_physical_property
!!
!!@author H. Matsui
!!@date Programmed in 2001
!!@date Modified in Jan., 2007
!
!>@brief  Coeffiecients of each term
!
      module t_physical_property
!
      use m_precision
!
      implicit  none
!
!>      Turn OFF flag
      integer (kind=kint), parameter, private :: id_turn_OFF = 0
!
!>     flag for no gravity
      integer (kind = kint), parameter :: iflag_no_gravity =  -1
!>     flag for constant gravity
      integer (kind = kint), parameter :: iflag_const_g =      0
!>     flag for radial gravity (amplitude is fixed)
      integer (kind = kint), parameter :: iflag_radial_g =     1
!>     flag for self radial gravity
      integer (kind = kint), parameter :: iflag_self_r_g =     2
!
!   Coefficients
!
!>      Structure for fluid property
      type fluid_property
!>        coefficient for time evolution of velocity and advection
        real  (kind=kreal) :: coef_velo
!>       coefficient for advection (-coef_velo)
        real  (kind=kreal) :: coef_nega_v
!>       coefficient for time pressure gradient
        real  (kind=kreal) :: coef_press
!>       1 / coef_press
        real  (kind=kreal) :: acoef_press
!
!>       coefficient for viscous diffusion
        real  (kind=kreal) :: coef_diffuse
!
!>       coefficient for thermal buoyancy
        real  (kind=kreal) :: coef_buo
!>       coefficient for Coriolis force
        real  (kind=kreal) :: coef_cor
!>       coefficient for chemical Lorentz force
        real  (kind=kreal) :: coef_lor
!>       coefficient for chemical buoyancy
        real  (kind=kreal) :: coef_comp_buo
!
!>       flag for gravity type
        integer (kind=kint) :: i_grav
!>       gravity direction for constant gravity
        real (kind=kreal) :: grav(3)
!>       rotation vector for Coriolis force
        real (kind=kreal) :: sys_rot(3)
      end type fluid_property
!
!>      Structure for manetic property
      type conductive_property
!>       coefficient for time evolution of magnetic field
        real  (kind=kreal) :: coef_magne
!>       coefficient for time electric potentia
        real  (kind=kreal) :: coef_mag_p
!>       1 / coef_mag_p
        real  (kind=kreal) :: acoef_mag_p
!
!>       coefficient for magnetic diffusion
        real  (kind=kreal) :: coef_diffuse
!>       coefficient for magnetic induction
        real  (kind=kreal) :: coef_induct
!
!>        Magneto convectio flag
        integer (kind=kint) :: iflag_magneto_cv = id_turn_OFF
!>       external magnetic field (Constant)
        real (kind=kreal) :: ex_magne(3)
      end type conductive_property
!
!>      Structure for thermal property
      type scalar_property
!>       coefficient for time evolution of temperature and heat flux
        real  (kind=kreal) :: coef_advect
!>       coefficient for heat flux (-coef_advect)
        real  (kind=kreal) :: coef_nega_adv
!
!>       coefficient for thermal diffusion
        real  (kind=kreal) :: coef_diffuse
!>       coefficient for heat source term
        real  (kind=kreal) :: coef_source
      end type scalar_property
!
      end module t_physical_property
