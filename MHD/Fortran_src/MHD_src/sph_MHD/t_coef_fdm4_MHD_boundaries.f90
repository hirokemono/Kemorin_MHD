!>@file   t_coef_fdm4_MHD_boundaries.f90
!!@brief  module t_coef_fdm4_MHD_boundaries
!!
!!@author H. Matsui
!!@date Programmed in May., 2013
!
!>@brief Matrix to evaluate radial derivative for non-slip at ICB
!!
!!@verbatim
!!      subroutine check_4th_ICB_vpol_fdm(fdm4_vpol_ICB)
!!        type(fdm4_ICB_vpol), intent(in) :: fdm4_vpol_ICB
!!
!!   Matrix for poloidal velocity with non-slip boundary at ICB
!!      d2fdr2 =    fdm4_noslip_ICB%dmat_vp0( 2,3) * d_rj(ICB+2)
!!                + fdm4_noslip_ICB%dmat_vp0( 1,3) * d_rj(ICB+1)
!!                + fdm4_noslip_ICB%dmat_vp0( 0,3) * d_rj(ICB  )
!!      d3fdr3 =    fdm4_noslip_ICB%dmat_vp0( 2,4) * d_rj(ICB+2)
!!                + fdm4_noslip_ICB%dmat_vp0( 1,4) * d_rj(ICB+1)
!!                + fdm4_noslip_ICB%dmat_vp0( 0,4) * d_rj(ICB  )
!!      d4fdr4 =    fdm4_noslip_ICB%dmat_vp0( 2,5) * d_rj(ICB+2)
!!                + fdm4_noslip_ICB%dmat_vp0( 1,5) * d_rj(ICB+1)
!!                + fdm4_noslip_ICB%dmat_vp0( 0,5) * d_rj(ICB  )
!!
!!   Matrix for poloidal velocity with non-slip boundary at next of ICB
!!      dfdr =      fdm4_noslip_ICB%dmat_vp1( 2,2) * d_rj(ICB+3)
!!                + fdm4_noslip_ICB%dmat_vp1( 1,2) * d_rj(ICB+2)
!!                + fdm4_noslip_ICB%dmat_vp1( 0,2) * d_rj(ICB+1)
!!                + fdm4_noslip_ICB%dmat_vp1(-1,2) * d_rj(ICB  )
!!      d2fdr2 =    fdm4_noslip_ICB%dmat_vp1( 2,3) * d_rj(ICB+3)
!!                + fdm4_noslip_ICB%dmat_vp1( 1,3) * d_rj(ICB+2)
!!                + fdm4_noslip_ICB%dmat_vp1( 0,3) * d_rj(ICB+1)
!!                + fdm4_noslip_ICB%dmat_vp1(-1,3) * d_rj(ICB  )
!!      d3fdr3 =    fdm4_noslip_ICB%dmat_vp1( 2,4) * d_rj(ICB+3)
!!                + fdm4_noslip_ICB%dmat_vp1( 1,4) * d_rj(ICB+2)
!!                + fdm4_noslip_ICB%dmat_vp1( 0,4) * d_rj(ICB+1)
!!                + fdm4_noslip_ICB%dmat_vp1(-1,4) * d_rj(ICB  )
!!      d4fdr4 =    fdm4_noslip_ICB%dmat_vp1( 2,5) * d_rj(ICB+3)
!!                + fdm4_noslip_ICB%dmat_vp1( 1,5) * d_rj(ICB+2)
!!                + fdm4_noslip_ICB%dmat_vp1( 0,5) * d_rj(ICB+1)
!!                + fdm4_noslip_ICB%dmat_vp1(-1,5) * d_rj(ICB  )
!!
!!
!!   Matrix for poloidal velocity with free-slip boundary at ICB
!!      dfdr =      fdm4_free_ICB%dmat_vp1( 2,2) * d_rj(3)
!!                + fdm4_free_ICB%dmat_vp1( 1,2) * d_rj(2)
!!                + fdm4_free_ICB%dmat_vp1( 0,2) * d_rj(1)
!!      d2fdr2 =    fdm4_free_ICB%dmat_vp1( 2,3) * d_rj(3)
!!                + fdm4_free_ICB%dmat_vp1( 1,3) * d_rj(2)
!!                + fdm4_free_ICB%dmat_vp1( 0,3) * d_rj(1)
!!      d3fdr3 =    fdm4_free_ICB%dmat_vp1( 2,4) * d_rj(3)
!!                + fdm4_free_ICB%dmat_vp1( 1,4) * d_rj(2)
!!                + fdm4_free_ICB%dmat_vp1( 0,4) * d_rj(1)
!!      d4fdr4 =    fdm4_free_ICB%dmat_vp1( 2,5) * d_rj(3)
!!                + fdm4_free_ICB%dmat_vp1( 1,5) * d_rj(2)
!!                + fdm4_free_ICB%dmat_vp1( 0,5) * d_rj(1)
!!
!!   Matrix for poloidal velocity with free-slip boundary at next of ICB
!!      dfdr =      fdm4_free_ICB%dmat_vp1( 2,2) * d_rj(ICB+3)
!!                + fdm4_free_ICB%dmat_vp1( 1,2) * d_rj(ICB+2)
!!                + fdm4_free_ICB%dmat_vp1( 0,2) * d_rj(ICB+1)
!!                + fdm4_free_ICB%dmat_vp1(-1,2) * d_rj(ICB  )
!!      d2fdr2 =    fdm4_free_ICB%dmat_vp1( 2,3) * d_rj(ICB+3)
!!                + fdm4_free_ICB%dmat_vp1( 1,3) * d_rj(ICB+2)
!!                + fdm4_free_ICB%dmat_vp1( 0,3) * d_rj(ICB+1)
!!                + fdm4_free_ICB%dmat_vp1(-1,3) * d_rj(ICB  )
!!      d3fdr3 =    fdm4_free_ICB%dmat_vp1( 2,4) * d_rj(ICB+3)
!!                + fdm4_free_ICB%dmat_vp1( 1,4) * d_rj(ICB+2)
!!                + fdm4_free_ICB%dmat_vp1( 0,4) * d_rj(ICB+1)
!!                + fdm4_free_ICB%dmat_vp1(-1,4) * d_rj(ICB  )
!!      d4fdr4 =    fdm4_free_ICB%dmat_vp1( 2,5) * d_rj(ICB+3)
!!                + fdm4_free_ICB%dmat_vp1( 1,5) * d_rj(ICB+2)
!!                + fdm4_free_ICB%dmat_vp1( 0,5) * d_rj(ICB+1)
!!                + fdm4_free_ICB%dmat_vp1(-1,5) * d_rj(ICB  )
!!@endverbatim
!!
!!@n @param r_from_ICB(0:3) radius to three next points of ICB
!!
      module t_coef_fdm4_MHD_boundaries
!
      use m_precision
      use m_constants
!
      implicit none
!
      type fdm4_ICB_vpol
!>        Matrix to evaluate radial derivative at ICB
        real(kind = kreal) :: dmat_vp0(0:2,2:5)
!>        Matrix to evaluate radial derivative at next of ICB
        real(kind = kreal) :: dmat_vp1(-1:2,5)
      end type fdm4_ICB_vpol
!
! -----------------------------------------------------------------------
!
      contains
!
! -----------------------------------------------------------------------
!
      subroutine check_4th_ICB_vpol_fdm(fdm4_vpol_ICB)
!
      type(fdm4_ICB_vpol), intent(in) :: fdm4_vpol_ICB
!
!
      write(50,*) ' fdm4_vpol_ICB%dmat_vp0'
      write(50,*) 'matrix for dfdr'
      write(50,'(1p9E25.15e3)') fdm4_vpol_ICB%dmat_vp0(0:2,2)
      write(50,*) 'matrix for d2fdr2'
      write(50,'(1p9E25.15e3)') fdm4_vpol_ICB%dmat_vp0(0:2,3)
      write(50,*) 'matrix for d3fdr3'
      write(50,'(1p9E25.15e3)') fdm4_vpol_ICB%dmat_vp0(0:2,4)
!
      write(50,*) ' fdm4_vpol_ICB%dmat_vp1'
      write(50,*) 'matrix for dfdr'
      write(50,'(1p9E25.15e3)') fdm4_vpol_ICB%dmat_vp1(-1:2,2)
      write(50,*) 'matrix for d2fdr2'
      write(50,'(1p9E25.15e3)') fdm4_vpol_ICB%dmat_vp1(-1:2,3)
      write(50,*) 'matrix for d3fdr3'
      write(50,'(1p9E25.15e3)') fdm4_vpol_ICB%dmat_vp1(-1:2,4)
      write(50,*) 'matrix for d4fdr4'
      write(50,'(1p9E25.15e3)') fdm4_vpol_ICB%dmat_vp1(-1:2,5)
!
      end subroutine check_4th_ICB_vpol_fdm
!
! -----------------------------------------------------------------------
!
      end module t_coef_fdm4_MHD_boundaries
