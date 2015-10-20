!>@file  m_jacobian_sf_grp.f90
!!       module m_jacobian_sf_grp
!!
!!@author H. Matsui
!!@date   Programmed on Nov., 2008
!!@n      Modified by H. Matsui on Feb., 2012
!
!> @brief  2D Jacobian and difference for surface groups
!!
!!@verbatim
!!      subroutine cal_jacobian_surf_grp(sf_grp)
!!
!!      subroutine allocate_jacobians_2d_l_quad(sf_grp, n_int)
!!
!!      subroutine deallocate_jacobians_2d_linear
!!      subroutine deallocate_jacobians_2d_quad
!!      subroutine deallocate_jacobians_2d_l_quad
!!@endverbatim
!
      module   m_jacobian_sf_grp
!
      use m_precision
      use t_jacobian_2d
!
      implicit  none
!
!>     Stracture of linear Jacobians for surafce group
      type(jacobians_2d), save :: jac1_sf_grp_2d_l
!>     Stracture of quadrature Jacobians for surafce group
      type(jacobians_2d), save :: jac1_sf_grp_2d_q
!>     Stracture of quadrature Jacobians for linear surafce group
      type(jacobians_2d), save :: jac1_sf_grp_2d_ql
!
! ----------------------------------------------------------------------
!
      contains
!
! ----------------------------------------------------------------------
!> Construct shape function, difference of shape function, and Jacobian
!> for surface group
!
      subroutine cal_jacobian_surf_grp(sf_grp)
!
      use m_machine_parameter
      use m_geometry_constants
      use m_geometry_data
      use t_group_data
!
      use const_jacobians_2d
!
      type(surface_group_data), intent(in) :: sf_grp
!
!
      if (sf_grp%num_grp .le. 0) return
      call alloc_2d_jac_type(sf_grp%num_item, num_linear_sf,            &
     &                       maxtot_int_2d, jac1_sf_grp_2d_l)
      if (iflag_debug.eq.1) write(*,*) 'cal_jacobian_sf_grp_linear'
      call cal_jacobian_sf_grp_linear(node1, ele1, sf_grp,              &
     &    jac1_sf_grp_2d_l)
!
      if (ele1%first_ele_type .eq. 332) then
        if (iflag_debug.eq.1)  write(*,*) 'cal_jacobian_sf_grp_quad'
        call alloc_2d_jac_type(sf_grp%num_item, surf1%nnod_4_surf,      &
     &      maxtot_int_2d, jac1_sf_grp_2d_q)
        call cal_jacobian_sf_grp_quad(node1, ele1, sf_grp,              &
     &      jac1_sf_grp_2d_q)
      else if (ele1%first_ele_type .eq. 333) then
        if (iflag_debug.eq.1) write(*,*) 'cal_jacobian_sf_grp_lag'
        call alloc_2d_jac_type(sf_grp%num_item, surf1%nnod_4_surf,      &
     &        maxtot_int_2d, jac1_sf_grp_2d_q)
        call cal_jacobian_sf_grp_lag(node1, ele1, sf_grp,               &
     &      jac1_sf_grp_2d_q)
      else
        if (iflag_debug.eq.1) write(*,*) 'copy_jacobians_2d_quad'
        call copy_jacobians_2d(sf_grp%num_item, surf1%nnod_4_surf,      &
     &        jac1_sf_grp_2d_l, jac1_sf_grp_2d_q)
      end if
!
      end subroutine cal_jacobian_surf_grp
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!
      subroutine allocate_jacobians_2d_l_quad(sf_grp, n_int)
!
       use m_geometry_constants
       use t_group_data
!
      type(surface_group_data), intent(in) :: sf_grp
      integer(kind = kint), intent(in) :: n_int
!
      call alloc_2d_jac_type(sf_grp%num_item, num_quad_sf, n_int,       &
     &    jac1_sf_grp_2d_ql)
!
       end subroutine allocate_jacobians_2d_l_quad
!
!  ------------------------------------------------------------------
!  ------------------------------------------------------------------
!
       subroutine deallocate_jacobians_2d_linear
!
!
      call dealloc_2d_jac_type(jac1_sf_grp_2d_l)
!
      end subroutine deallocate_jacobians_2d_linear
!
!  ------------------------------------------------------------------
!
      subroutine deallocate_jacobians_2d_quad
!
!
      call dealloc_2d_jac_type(jac1_sf_grp_2d_q)
!
      end subroutine deallocate_jacobians_2d_quad
!
!  ------------------------------------------------------------------
!
      subroutine deallocate_jacobians_2d_l_quad
!
!
      call dealloc_2d_jac_type(jac1_sf_grp_2d_ql)
!
      end subroutine deallocate_jacobians_2d_l_quad
!
!  ------------------------------------------------------------------
!
      end module   m_jacobian_sf_grp
