!set_bc_grad_light_comp.f90
!      module set_bc_grad_light_comp
!
!      Written by H. Matsui on Sep., 2005
!
!      subroutine set_bc_grad_composition
!
      module set_bc_grad_light_comp
!
      use m_precision
!
      use m_control_parameter
!
      implicit none
!
!-----------------------------------------------------------------------
!
      contains
!
!-----------------------------------------------------------------------
!
      subroutine set_bc_grad_composition
!
      use int_surf_fixed_gradients
!
!
      call int_sf_grad_composition(intg_point_t_evo)
!
      end subroutine set_bc_grad_composition
!
!-----------------------------------------------------------------------
!
      end module set_bc_grad_light_comp
