!m_ctl_data_mhd_normalize.f90
!      module m_ctl_data_mhd_normalize
!
!        programmed by H.Matsui on March. 2006
!
!      subroutine read_dimless_control
!      subroutine read_coef_term_control
!
!   --------------------------------------------------------------------
!    example
!!
!!!!!!  dimensionless numbers !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!  available numbers
!!     Prandtl_number, magnetic_Prandtl_number
!!     Rayleigh_number, modified_Rayleigh_number
!!     Composit_Rayleigh_number
!!     Reynords_number
!!     Taylor_number, Ekman_number
!!     Elsasser_number
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!    begin dimensionless_ctl
!!      array dimless_ctl 6
!!        dimless_ctl  Prandtl_number                   1.0e-0
!!        dimless_ctl  modified_Rayleigh_number         1.0E+2
!!        dimless_ctl  Ekman_number                     1.0e-3
!!        dimless_ctl  magnetic_Prandtl_number          5.0e+0
!!        dimless_ctl  Composite_Rayleigh_number        1.0E+2
!!        dimless_ctl  Composite_Prandtl_number         1.0E+2
!!      end array
!!    end  dimensionless_ctl
!!
!!!!!! Normalization settings  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!
!!    define
!!     coef_4_termal_ctl:      time integration and advection of temp
!!     coef_4_velocity_ctl:    time integration and advection of temperature
!!     coef_4_magnetic_ctl:    time integration and advection of magnetic f.
!!     coef_4_composition_ctl: time integration and advection of composition
!!
!!     coef_4_press_ctl:   coefficients for pressure gradient
!!     coef_4_mag_p_ctl:   coefficients for potential electric field
!!
!!     coef_4_t_diffuse_ctl:   coefficients for thermal diffusion
!!     coef_4_v_diffuse_ctl:   coefficients for viscous diffusion
!!     coef_4_m_diffuse_ctl:   coefficients for magnetic diffusion
!!     coef_4_c_diffuse_ctl:   coefficients for compositional diffusion
!!
!!     coef_4_buoyancy_ctl:   coefficients for buoyancy
!!     coef_4_Coriolis_ctl:   coefficients for Coriolis force
!!     coef_4_Lorentz_ctl:    coefficients for Lorantz force
!!     coef_4_composit_buoyancy_ctl: coefficients for compositional buoyancy
!!
!!     One:  1, Zero (Ignore), Two:  2,   Radial_parameter: (1-ri/ro)
!!     Radial_35: (1-0.35)
!!
!!     Real number.... Power of each numbers
!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!
!    begin coefficients_ctl
!      begin thermal
!        ...
!      end  thermal
!!
!      begin momentum
!        ...
!      end  momentum
!!
!      begin induction
!        ...
!      end  induction
!!
!      begin composition
!        ...
!      end  composition
!!
!    end  coefficients_ctl
!!
!!   --------------------------------------------------------------------
!
      module m_ctl_data_mhd_normalize
!
      use m_precision
!
      use m_machine_parameter
      use m_read_control_elements
      use skip_comment_f
      use t_read_control_arrays
      use t_ctl_data_mhd_normalize
!
      implicit  none
!
!
!>        Structure for list of dimensionless numbers
      type(dimless_control), save :: dless_ctl1
!
!>      Structure for coefficients of governing equations
      type(equations_control) :: eqs_ctl1
!
!   entry label
!
      character(len=kchara), parameter                                  &
     &      :: hd_dimless_ctl =  'dimensionless_ctl'
      integer (kind=kint) :: i_dimless_ctl =   0
!
      character(len=kchara), parameter                                  &
     &      :: hd_coef_term_ctl ='coefficients_ctl'
      integer (kind=kint) :: i_coef_term_ctl = 0
!
      private :: hd_dimless_ctl, hd_coef_term_ctl
      private :: i_dimless_ctl,  i_coef_term_ctl
!
!
!   --------------------------------------------------------------------
!
      contains
!
!   --------------------------------------------------------------------
!
      subroutine read_dimless_control
!
!
      call read_dimless_ctl(hd_dimless_ctl, i_dimless_ctl, dless_ctl1)
!
      end subroutine read_dimless_control
!
!   --------------------------------------------------------------------
!
      subroutine read_coef_term_control
!
!
      call read_coef_term_ctl                                           &
     &   (hd_coef_term_ctl, i_coef_term_ctl, eqs_ctl1)
!
      end subroutine read_coef_term_control
!
!   --------------------------------------------------------------------
!
      end module m_ctl_data_mhd_normalize
