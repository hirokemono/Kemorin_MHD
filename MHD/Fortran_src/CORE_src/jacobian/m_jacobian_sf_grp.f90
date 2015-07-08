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
!!      subroutine cal_jacobian_surf_grp
!!
!!      subroutine allocate_jacobians_2d_l_quad(n_int)
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
      subroutine cal_jacobian_surf_grp
!
      use m_machine_parameter
      use m_geometry_constants
      use m_geometry_parameter
      use m_surface_group
!
      use cal_jacobians_linear
      use cal_jacobians_quad
      use cal_jacobians_lag
!
!
      if (num_surf .le. 0) return
      call alloc_2d_jac_type(num_surf_bc, num_linear_sf, maxtot_int_2d, &
     &                       jac1_sf_grp_2d_l)
      if (iflag_debug.eq.1) write(*,*) 'cal_jacobian_dylinear'
      call cal_jacobian_dylinear(jac1_sf_grp_2d_l)
!
      if (first_ele_type .eq. 332) then
        if (iflag_debug.eq.1)  write(*,*) 'cal_jacobian_dyquad'
        call alloc_2d_jac_type(num_surf_bc, nnod_4_surf,                &
     &        maxtot_int_2d, jac1_sf_grp_2d_q)
        call cal_jacobian_dyquad(jac1_sf_grp_2d_q)
      else if (first_ele_type .eq. 333) then
        if (iflag_debug.eq.1) write(*,*) 'cal_jacobian_dylag'
        call alloc_2d_jac_type(num_surf_bc, nnod_4_surf,                &
     &        maxtot_int_2d, jac1_sf_grp_2d_q)
        call cal_jacobian_dylag(jac1_sf_grp_2d_q)
      else
        if (iflag_debug.eq.1) write(*,*) 'copy_jacobians_2d_quad'
        call copy_jacobians_2d(num_surf_bc, nnod_4_surf,                &
     &        jac1_sf_grp_2d_l, jac1_sf_grp_2d_q)
      end if
!
      end subroutine cal_jacobian_surf_grp
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!
      subroutine allocate_jacobians_2d_l_quad(n_int)
!
       use m_geometry_constants
       use m_geometry_parameter
       use m_surface_group
!
      integer(kind = kint), intent(in) :: n_int
!
      call alloc_2d_jac_type(num_surf_bc, num_quad_sf, n_int,           &
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
