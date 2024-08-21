!>@file   coef_fdm3e_hdiv_CMB.f90
!!@brief  module coef_fdm3e_hdiv_CMB
!!
!!@author H. Matsui
!!@date Programmed in May., 2013
!
!>@brief Matrix to evaluate radial derivative for free-slip at CMB
!!
!!@verbatim
!!      subroutine cal_fdm3e_CMB_hdiv_vp(r_from_CMB)
!!        type(fdm3e_BC_hdiv), intent(inout) :: fdm3e_CMB
!!
!!   Matrix for poloidal velocity with horizontal divergence at CMB
!!      d_ele =     fdm3e_CMB%dmat_vp0(-2,1) * d_rj(CMB-2)
!!                + fdm3e_CMB%dmat_vp0(-1,1) * d_rj(CMB-1)
!!                + fdm3e_CMB%dmat_vp0( 0,1) * d_rj(CMB  )
!!                + fdm3e_CMB%dmat_vp0( 1,1) * dfdr(CMB  )
!!      dfdr =      fdm3e_CMB%dmat_vp0(-2,2) * d_rj(CMB-3)
!!                + fdm3e_CMB%dmat_vp0(-1,2) * d_rj(CMB-2)
!!                + fdm3e_CMB%dmat_vp0( 0,2) * d_rj(CMB-1)
!!                + fdm3e_CMB%dmat_vp0( 1,2) * dfdr(CMB  )
!!      d2fdr2 =    fdm3e_CMB%dmat_vp0(-2,3) * d_rj(CMB-3)
!!                + fdm3e_CMB%dmat_vp0(-1,3) * d_rj(CMB-2)
!!                + fdm3e_CMB%dmat_vp0( 0,3) * d_rj(CMB-1)
!!                + fdm3e_CMB%dmat_vp0( 1,3) * dfdr(CMB  )
!!      d3fdr3 =    fdm3e_CMB%dmat_vp0(-2,4) * d_rj(CMB-3)
!!                + fdm3e_CMB%dmat_vp0(-1,4) * d_rj(CMB-2)
!!                + fdm3e_CMB%dmat_vp0( 0,4) * d_rj(CMB-1)
!!                + fdm3e_CMB%dmat_vp0( 1,4) * dfdr(CMB  )
!!
!!      subroutine cal_third_fdm_CMB_ele(i_th, kr_out,                  &
!!     &          sph_rj, fdm3e_CMB, d_rj, dfdr_rj, dele_bc)
!!        type(sph_rj_grid), intent(in) ::  sph_rj
!!        type(fdm3e_BC_hdiv), intent(in) :: fdm3e_CMB
!!        integer(kind = kint), intent(in) :: i_th, kr_out
!!        real(kind = kreal), intent(in) :: d_rj(sph_rj%nnod_rj)
!!        real(kind = kreal), intent(in) :: dfdr_rj(sph_rj%nnod_rj)
!!        real(kind = kreal), intent(inout) :: dele_bc(sph_rj%nidx_rj(2))
!!@endverbatim
!!
      module coef_fdm3e_hdiv_CMB
!
      use m_precision
      use m_constants
!
      use t_spheric_rj_data
      use t_coef_fdm3e_MHD_boundaries
      use cal_inverse_small_matrix
!
      implicit none
!
!
!>      Work matrix to evaluate fdm3e_CMB%dmat_vp0
!!@verbatim
!!      d_rj(CMB  ) =  mat_taylor_3e(1,1) * d_ele
!!                   + mat_taylor_3e(1,2) * dfdr
!!                   + mat_taylor_3e(1,3) * d2fdr2
!!                   + mat_taylor_3e(1,4) * d3fdr3
!!      d_rj(CMB-1) =  mat_taylor_3e(2,1) * d_ele
!!                   + mat_taylor_3e(2,2) * dfdr
!!                   + mat_taylor_3e(2,3) * d2fdr2
!!                   + mat_taylor_3e(2,4) * d3fdr3
!!      d_rj(CMB-2) =  mat_taylor_3e(3,1) * d_ele
!!                   + mat_taylor_3e(3,2) * dfdr
!!                   + mat_taylor_3e(3,3) * d2fdr2
!!                   + mat_taylor_3e(3,4) * d3fdr3
!!      dfdr(CMB  ) =  mat_taylor_3e(4,1) * d_ele
!!                   + mat_taylor_3e(4,2) * dfdr
!!                   + mat_taylor_3e(4,3) * d2fdr2
!!                   + mat_taylor_3e(4,4) * d3fdr3
!!     mat_fdm4_CMB1_free_vp = (mat_taylor_3e)^-1
!!@endverbatim
      real(kind = kreal) :: mat_fdm3e_CMB_hdiv_vp(4,4)
!!@endverbatim
!
      private :: mat_fdm3e_CMB_hdiv_vp
!
! -----------------------------------------------------------------------
!
      contains
!
! -----------------------------------------------------------------------
!
      subroutine cal_fdm3e_CMB_hdiv_vp(r_from_CMB, fdm3e_CMB)
!
      real(kind = kreal), intent(in) :: r_from_CMB(-2:0)
      type(fdm3e_BC_hdiv), intent(inout) :: fdm3e_CMB
!
      integer(kind = kint) :: ierr
      real(kind = kreal) :: mat_taylor_3e(4,4)
      real(kind = kreal) :: dr_p1, dr_n1, dr_n2
!
!
      dr_p1 = half * (r_from_CMB(0) - r_from_CMB(-1))
      dr_n1 = half * (r_from_CMB(0) - r_from_CMB(-1))
      dr_n2 = r_from_CMB(0) - half * (r_from_CMB(-1) + r_from_CMB(-2))
!
      mat_taylor_3e(1,1) = one
      mat_taylor_3e(1,2) = dr_p1
      mat_taylor_3e(1,3) = half * dr_p1*dr_p1
      mat_taylor_3e(1,4) = dr_p1*dr_p1*dr_p1 / six
!
      mat_taylor_3e(2,1) =   one
      mat_taylor_3e(2,2) =  -dr_n1
      mat_taylor_3e(2,3) =   half * dr_n1*dr_n1
      mat_taylor_3e(2,4) =  -dr_n1*dr_n1*dr_n1 / six
!
      mat_taylor_3e(3,1) =   one
      mat_taylor_3e(3,2) =  -dr_n2
      mat_taylor_3e(3,3) =   half * dr_n2*dr_n2
      mat_taylor_3e(3,4) =  -dr_n2*dr_n2*dr_n2 / six
!
      mat_taylor_3e(4,1) =  zero
      mat_taylor_3e(4,2) =  one
      mat_taylor_3e(4,3) =  dr_p1
      mat_taylor_3e(4,4) =  half * dr_p1*dr_p1
!
      call cal_inverse_44_matrix(mat_taylor_3e,                         &
     &                           mat_fdm3e_CMB_hdiv_vp, ierr)
!
      if(ierr .eq. 1) then
        write(*,*) 'singular matrix cal_fdm3e_CMB_hdiv_vp ',            &
     &            r_from_CMB(0)
      end if
!
      fdm3e_CMB%dmat_vp0( 0,1:4) = mat_fdm3e_CMB_hdiv_vp(1:4,1)
      fdm3e_CMB%dmat_vp0(-1,1:4) = mat_fdm3e_CMB_hdiv_vp(1:4,2)
      fdm3e_CMB%dmat_vp0(-2,1:4) = mat_fdm3e_CMB_hdiv_vp(1:4,3)
      fdm3e_CMB%dmat_vp0( 1,1:4) = mat_fdm3e_CMB_hdiv_vp(1:4,4)
!
      end subroutine cal_fdm3e_CMB_hdiv_vp
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine cal_third_fdm_CMB_ele(i_th, kr_out,                    &
     &          sph_rj, fdm3e_CMB, d_rj, dfdr_rj, dele_bc)
!
      type(sph_rj_grid), intent(in) ::  sph_rj
      type(fdm3e_BC_hdiv), intent(in) :: fdm3e_CMB
      integer(kind = kint), intent(in) :: i_th, kr_out
      real(kind = kreal), intent(in) :: d_rj(sph_rj%nnod_rj)
      real(kind = kreal), intent(in) :: dfdr_rj(sph_rj%nnod_rj)
!
      real(kind = kreal), intent(inout) :: dele_bc(sph_rj%nidx_rj(2))
!
      integer(kind = kint) :: inod, i_n2, i_n1, j
!
!
!$omp parallel do private(inod,i_n2,i_n1,j)
      do j = 1, sph_rj%nidx_rj(2)
        inod = j + (kr_out-1) * sph_rj%nidx_rj(2)
        i_n1 = j + (kr_out-2) * sph_rj%nidx_rj(2)
        i_n2 = j + (kr_out-3) * sph_rj%nidx_rj(2)
!
        dele_bc(inod) =  fdm3e_CMB%dmat_vp0(-2,i_th+1) * d_rj(i_n2)     &
     &                 + fdm3e_CMB%dmat_vp0(-1,i_th+1) * d_rj(i_n1)     &
     &                 + fdm3e_CMB%dmat_vp0( 0,i_th+1) * d_rj(inod)     &
     &                 + fdm3e_CMB%dmat_vp0( 1,i_th+1) * dfdr_rj(inod)
      end do
!$omp end parallel do
!
      end subroutine cal_third_fdm_CMB_ele
!
! -----------------------------------------------------------------------
!
      end module coef_fdm3e_hdiv_CMB
