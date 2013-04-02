!
!     module   m_normalize_parameter
!.......................................................................
!
!      subroutine allocate_coef_4_termal
!      subroutine allocate_coef_4_velocity
!      subroutine allocate_coef_4_press
!      subroutine allocate_coef_4_magne
!      subroutine allocate_coef_4_mag_p
!
!      subroutine allocate_coef_4_t_diffuse
!      subroutine allocate_coef_4_v_diffuse
!      subroutine allocate_coef_4_m_diffuse
!
!      subroutine allocate_coef_4_buoyancy
!      subroutine allocate_coef_4_comp_buo
!      subroutine allocate_coef_4_coriolis
!      subroutine allocate_coef_4_lorentz
!      subroutine allocate_coef_4_induction
!
!      subroutine allocate_coef_4_composition
!      subroutine allocate_coef_4_c_diffuse
!
      module m_normalize_parameter
!
      use m_precision
!
      implicit  none
!
!
      integer(kind=kint) :: num_coef_4_termal
      integer(kind=kint) :: num_coef_4_velocity
      integer(kind=kint) :: num_coef_4_press
      integer(kind=kint) :: num_coef_4_magnetic
      integer(kind=kint) :: num_coef_4_mag_p
      integer(kind=kint) :: num_coef_4_composition
      integer(kind=kint) :: num_coef_4_t_diffuse
      integer(kind=kint) :: num_coef_4_v_diffuse
      integer(kind=kint) :: num_coef_4_m_diffuse
      integer(kind=kint) :: num_coef_4_c_diffuse
      integer(kind=kint) :: num_coef_4_buoyancy
      integer(kind=kint) :: num_coef_4_comp_buo
      integer(kind=kint) :: num_coef_4_Coriolis
      integer(kind=kint) :: num_coef_4_Lorentz
      integer(kind=kint) :: num_coef_4_induction
!
      character (len=kchara), allocatable :: coef_4_termal_name(:)
      character (len=kchara), allocatable :: coef_4_velocity_name(:)
      character (len=kchara), allocatable :: coef_4_press_name(:)
      character (len=kchara), allocatable :: coef_4_magnetic_name(:)
      character (len=kchara), allocatable :: coef_4_mag_p_name(:)
      character (len=kchara), allocatable :: coef_4_composit_name(:)
      character (len=kchara), allocatable :: coef_4_t_diffuse_name(:)
      character (len=kchara), allocatable :: coef_4_v_diffuse_name(:)
      character (len=kchara), allocatable :: coef_4_m_diffuse_name(:)
      character (len=kchara), allocatable :: coef_4_c_diffuse_name(:)
      character (len=kchara), allocatable :: coef_4_buoyancy_name(:)
      character (len=kchara), allocatable :: coef_4_comp_buo_name(:)
      character (len=kchara), allocatable :: coef_4_Coriolis_name(:)
      character (len=kchara), allocatable :: coef_4_Lorentz_name(:)
      character (len=kchara), allocatable :: coef_4_induction_name(:)
!
      real (kind = kreal), allocatable :: coef_4_termal_power(:)
      real (kind = kreal), allocatable :: coef_4_velocity_power(:)
      real (kind = kreal), allocatable :: coef_4_press_power(:)
      real (kind = kreal), allocatable :: coef_4_magnetic_power(:)
      real (kind = kreal), allocatable :: coef_4_mag_p_power(:)
      real (kind = kreal), allocatable :: coef_4_t_diffuse_power(:)
      real (kind = kreal), allocatable :: coef_4_v_diffuse_power(:)
      real (kind = kreal), allocatable :: coef_4_m_diffuse_power(:)
      real (kind = kreal), allocatable :: coef_4_c_diffuse_power(:)
      real (kind = kreal), allocatable :: coef_4_buoyancy_power(:)
      real (kind = kreal), allocatable :: coef_4_comp_buo_power(:)
      real (kind = kreal), allocatable :: coef_4_Coriolis_power(:)
      real (kind = kreal), allocatable :: coef_4_Lorentz_power(:)
      real (kind = kreal), allocatable :: coef_4_induction_power(:)
      real (kind = kreal), allocatable :: coef_4_composit_power(:)
!
!  ---------------------------------------------------------------------
!
      contains
!
!  ---------------------------------------------------------------------
!
      subroutine allocate_coef_4_termal
!
      allocate(coef_4_termal_name(num_coef_4_termal))
      allocate(coef_4_termal_power(num_coef_4_termal))
      coef_4_termal_power = 0.0d0
!
      end subroutine allocate_coef_4_termal
!
! -----------------------------------------------------------------------
!
      subroutine allocate_coef_4_velocity
!
      allocate(coef_4_velocity_name(num_coef_4_velocity))
      allocate(coef_4_velocity_power(num_coef_4_velocity))
      coef_4_velocity_power = 0.0d0
!
      end subroutine allocate_coef_4_velocity
!
! -----------------------------------------------------------------------
!
      subroutine allocate_coef_4_press
!
      allocate(coef_4_press_name(num_coef_4_press))
      allocate(coef_4_press_power(num_coef_4_press))
      coef_4_press_power = 0.0d0
!
      end subroutine allocate_coef_4_press
!
! -----------------------------------------------------------------------
!
      subroutine allocate_coef_4_magne
!
      allocate(coef_4_magnetic_name(num_coef_4_magnetic))
      allocate(coef_4_magnetic_power(num_coef_4_magnetic))
      coef_4_magnetic_power = 0.0d0
!
      end subroutine allocate_coef_4_magne
!
! -----------------------------------------------------------------------
!
      subroutine allocate_coef_4_mag_p
!
      allocate(coef_4_mag_p_name(num_coef_4_mag_p))
      allocate(coef_4_mag_p_power(num_coef_4_mag_p))
      coef_4_mag_p_power = 0.0d0
!
      end subroutine allocate_coef_4_mag_p
!
! -----------------------------------------------------------------------
!
      subroutine allocate_coef_4_t_diffuse
!
      allocate(coef_4_t_diffuse_name(num_coef_4_t_diffuse))
      allocate(coef_4_t_diffuse_power(num_coef_4_t_diffuse))
      coef_4_t_diffuse_power = 0.0d0
!
      end subroutine allocate_coef_4_t_diffuse
!
! -----------------------------------------------------------------------
!
      subroutine allocate_coef_4_v_diffuse
!
      allocate(coef_4_v_diffuse_name(num_coef_4_v_diffuse))
      allocate(coef_4_v_diffuse_power(num_coef_4_v_diffuse))
      coef_4_v_diffuse_power = 0.0d0
!
      end subroutine allocate_coef_4_v_diffuse
!
! -----------------------------------------------------------------------
!
      subroutine allocate_coef_4_m_diffuse
!
      allocate(coef_4_m_diffuse_name(num_coef_4_m_diffuse))
      allocate(coef_4_m_diffuse_power(num_coef_4_m_diffuse))
      coef_4_m_diffuse_power = 0.0d0
!
      end subroutine allocate_coef_4_m_diffuse
!
! -----------------------------------------------------------------------
!
      subroutine allocate_coef_4_buoyancy
!
      allocate(coef_4_buoyancy_name(num_coef_4_buoyancy))
      allocate(coef_4_buoyancy_power(num_coef_4_buoyancy))
      coef_4_buoyancy_power = 0.0d0
!
      end subroutine allocate_coef_4_buoyancy
!
! -----------------------------------------------------------------------
!
      subroutine allocate_coef_4_comp_buo
!
      allocate(coef_4_comp_buo_name(num_coef_4_comp_buo))
      allocate(coef_4_comp_buo_power(num_coef_4_comp_buo))
      coef_4_comp_buo_power = 0.0d0
!
      end subroutine allocate_coef_4_comp_buo
!
! -----------------------------------------------------------------------
!
      subroutine allocate_coef_4_coriolis
!
      allocate(coef_4_Coriolis_name(num_coef_4_Coriolis))
      allocate(coef_4_Coriolis_power(num_coef_4_Coriolis))
      coef_4_Coriolis_power = 0.0d0
!
      end subroutine allocate_coef_4_coriolis
!
! -----------------------------------------------------------------------
!
      subroutine allocate_coef_4_lorentz
!
      allocate(coef_4_Lorentz_name(num_coef_4_Lorentz))
      allocate(coef_4_Lorentz_power(num_coef_4_Lorentz))
      coef_4_Lorentz_power = 0.0d0
!
      end subroutine allocate_coef_4_lorentz
!
! -----------------------------------------------------------------------
!
      subroutine allocate_coef_4_induction
!
      allocate(coef_4_induction_name(num_coef_4_induction))
      allocate(coef_4_induction_power(num_coef_4_induction))
      coef_4_induction_power = 0.0d0
!
      end subroutine allocate_coef_4_induction
!
! -----------------------------------------------------------------------
!
      subroutine allocate_coef_4_composition
!
      allocate(coef_4_composit_name(num_coef_4_composition))
      allocate(coef_4_composit_power(num_coef_4_composition))
      coef_4_composit_power = 0.0d0
!
      end subroutine allocate_coef_4_composition
!
! -----------------------------------------------------------------------
!
      subroutine allocate_coef_4_c_diffuse
!
      allocate(coef_4_c_diffuse_name(num_coef_4_c_diffuse))
      allocate(coef_4_c_diffuse_power(num_coef_4_c_diffuse))
      coef_4_c_diffuse_power = 0.0d0
!
      end subroutine allocate_coef_4_c_diffuse
!
! -----------------------------------------------------------------------
!
      end module m_normalize_parameter
