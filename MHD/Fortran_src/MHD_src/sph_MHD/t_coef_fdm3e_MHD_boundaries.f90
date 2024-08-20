!>@file   t_coef_fdm3e_MHD_boundaries.f90
!!@brief  module t_coef_fdm3e_MHD_boundaries
!!
!!@author H. Matsui
!!@date Programmed in May., 2013
!
!>@brief Matrix to evaluate radial derivative for non-slip at ICB
!!
!!@verbatim
!!      subroutine check_3rd_ele_ICB_vpol_fdm(fdm4_vpol_ICB)
!!        type(fdm3e_BC_hdiv), intent(in) :: fdm4_vpol_ICB
!!      subroutine check_3rd_ele_CMB_vpol_fdm(fdm4_vpol_CMB)
!!        type(fdm3e_BC_hdiv), intent(in) :: fdm4_vpol_CMB
!!
!!   Matrix for horizontal divergence of velocity at CMB element
!!      d_ele =     fdm4_noslip_CMB%dmat_vp0(-2,0) * d_rj(CMB-2)
!!                + fdm4_noslip_CMB%dmat_vp0(-1,0) * d_rj(CMB-1)
!!                + fdm4_noslip_CMB%dmat_vp0( 0,0) * d_rj(CMB  )
!!                + fdm4_noslip_CMB%dmat_vp0( 1,0) * dfdr(CMB  )
!!      dfdr =      fdm4_noslip_CMB%dmat_vp0(-2,1) * d_rj(CMB-2)
!!                + fdm4_noslip_CMB%dmat_vp0(-1,1) * d_rj(CMB-1)
!!                + fdm4_noslip_CMB%dmat_vp0( 0,1) * d_rj(CMB  )
!!                + fdm4_noslip_CMB%dmat_vp0( 1,1) * dfdr(CMB  )
!!      d2fdr2 =    fdm4_noslip_CMB%dmat_vp0(-2,2) * d_rj(CMB-2)
!!                + fdm4_noslip_CMB%dmat_vp0(-1,2) * d_rj(CMB-1)
!!                + fdm4_noslip_CMB%dmat_vp0( 0,2) * d_rj(CMB  )
!!                + fdm4_noslip_CMB%dmat_vp0( 1,2) * dfdr(CMB  )
!!      d3fdr3 =    fdm4_noslip_CMB%dmat_vp0(-2,3) * d_rj(CMB-2)
!!                + fdm4_noslip_CMB%dmat_vp0(-1,3) * d_rj(CMB-1)
!!                + fdm4_noslip_CMB%dmat_vp0( 0,3) * d_rj(CMB  )
!!                + fdm4_noslip_CMB%dmat_vp0( 1,3) * dfdr(CMB  )
!!
!!   Matrix for horizontal divergence of velocity at ICB element
!!      d_ele =     fdm4_noslip_CMB%dmat_vp0(-2,0) * dfdr(ICB  )
!!                + fdm4_noslip_CMB%dmat_vp0(-1,0) * d_rj(ICB+1)
!!                + fdm4_noslip_CMB%dmat_vp0( 0,0) * d_rj(ICB+2)
!!                + fdm4_noslip_CMB%dmat_vp0( 1,0) * d_rj(ICB+3)
!!      dfdr =      fdm4_noslip_CMB%dmat_vp0(-2,1) * dfdr(ICB  )
!!                + fdm4_noslip_CMB%dmat_vp0(-1,1) * d_rj(ICB  )
!!                + fdm4_noslip_CMB%dmat_vp0( 0,1) * d_rj(ICB+1)
!!                + fdm4_noslip_CMB%dmat_vp0( 1,1) * d_rj(ICB+2)
!!      d2fdr2 =    fdm4_noslip_CMB%dmat_vp0(-2,2) * dfdr(ICB  )
!!                + fdm4_noslip_CMB%dmat_vp0(-1,2) * d_rj(ICB  )
!!                + fdm4_noslip_CMB%dmat_vp0( 0,2) * d_rj(ICB+1)
!!                + fdm4_noslip_CMB%dmat_vp0( 1,2) * d_rj(ICB+2)
!!      d3fdr3 =    fdm4_noslip_CMB%dmat_vp0(-2,3) * dfdr(ICB  )
!!                + fdm4_noslip_CMB%dmat_vp0(-1,3) * d_rj(ICB  )
!!                + fdm4_noslip_CMB%dmat_vp0( 0,3) * d_rj(ICB+1)
!!                + fdm4_noslip_CMB%dmat_vp0( 1,3) * dfdr(ICB+2)
!!@endverbatim
!!
!!@n @param r_from_ICB(0:3) radius to three next points of ICB
!!
      module t_coef_fdm3e_MHD_boundaries
!
      use m_precision
!
      use m_constants
!
      implicit none
!
!
      type fdm3e_BC_hdiv
!>        Matrix to evaluate radial derivative at Boundary
        real(kind = kreal) :: dmat_vp0(-2:1,0:3)
      end type fdm3e_BC_hdiv
!
!
! -----------------------------------------------------------------------
!
      contains
!
! -----------------------------------------------------------------------
!
      subroutine check_3rd_ele_ICB_vpol_fdm(fdm4_vpol_ICB)
!
      type(fdm3e_BC_hdiv), intent(in) :: fdm4_vpol_ICB
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
      end subroutine check_3rd_ele_ICB_vpol_fdm
!
! -----------------------------------------------------------------------
!
      subroutine check_3rd_ele_CMB_vpol_fdm(fdm3e_free_CMB)
!
      type(fdm3e_BC_hdiv), intent(in) :: fdm3e_free_CMB
!
!
      write(50,*) ' fdm3e_free_CMB%dmat_vp0'
      write(50,*) 'matrix for Interpolation'
      write(50,'(1p9E25.15e3)') fdm3e_free_CMB%dmat_vp0(-2:1,0)
      write(50,*) 'matrix for dfdr'
      write(50,'(1p9E25.15e3)') fdm3e_free_CMB%dmat_vp0(-2:1,1)
      write(50,*) 'matrix for d3fdr3'
      write(50,'(1p9E25.15e3)') fdm3e_free_CMB%dmat_vp0(-2:1,2)
      write(50,*) 'matrix for d3fdr3'
      write(50,'(1p9E25.15e3)') fdm3e_free_CMB%dmat_vp0(-2:1,3)
!
      end subroutine check_3rd_ele_CMB_vpol_fdm
!
! -----------------------------------------------------------------------
!
      end module t_coef_fdm3e_MHD_boundaries
