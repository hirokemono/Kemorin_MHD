!>@file   t_coef_fdm3e_MHD_boundaries.f90
!!@brief  module t_coef_fdm3e_MHD_boundaries
!!
!!@author H. Matsui
!!@date Programmed in May., 2013
!
!>@brief Matrix to evaluate radial derivative for non-slip at ICB
!!
!!@verbatim
!!      subroutine check_3rd_ele_BC_vpol_fdm(id_file, fdm3e_BC0)
!!        integer(kind = kint), intent(in) :: id_file
!!        type(fdm3e_BC_hdiv), intent(in) :: fdm3e_BC0
!!
!!   Matrix for horizontal divergence of velocity at CMB element
!!      d_ele =     fdm3e_CMB%dmat_vp0(-2,1) * d_rj(CMB-2)
!!                + fdm3e_CMB%dmat_vp0(-1,1) * d_rj(CMB-1)
!!                + fdm3e_CMB%dmat_vp0( 0,1) * d_rj(CMB  )
!!                + fdm3e_CMB%dmat_vp0( 1,1) * dfdr(CMB  )
!!      dfdr =      fdm3e_CMB%dmat_vp0(-2,2) * d_rj(CMB-2)
!!                + fdm3e_CMB%dmat_vp0(-1,2) * d_rj(CMB-1)
!!                + fdm3e_CMB%dmat_vp0( 0,2) * d_rj(CMB  )
!!                + fdm3e_CMB%dmat_vp0( 1,2) * dfdr(CMB  )
!!      d2fdr2 =    fdm3e_CMB%dmat_vp0(-2,3) * d_rj(CMB-2)
!!                + fdm3e_CMB%dmat_vp0(-1,3) * d_rj(CMB-1)
!!                + fdm3e_CMB%dmat_vp0( 0,3) * d_rj(CMB  )
!!                + fdm3e_CMB%dmat_vp0( 1,3) * dfdr(CMB  )
!!      d3fdr3 =    fdm3e_CMB%dmat_vp0(-2,4) * d_rj(CMB-2)
!!                + fdm3e_CMB%dmat_vp0(-1,4) * d_rj(CMB-1)
!!                + fdm3e_CMB%dmat_vp0( 0,4) * d_rj(CMB  )
!!                + fdm3e_CMB%dmat_vp0( 1,4) * dfdr(CMB  )
!!
!!   Matrix for horizontal divergence of velocity at ICB element
!!      d_ele =     fdm3e_ICB%dmat_vp0(-2,0) * dfdr(ICB  )
!!                + fdm3e_ICB%dmat_vp0(-1,0) * d_rj(ICB  )
!!                + fdm3e_ICB%dmat_vp0( 0,0) * d_rj(ICB+1)
!!                + fdm3e_ICB%dmat_vp0( 1,0) * d_rj(ICB+2)
!!      dfdr =      fdm3e_ICB%dmat_vp0(-2,1) * dfdr(ICB  )
!!                + fdm3e_ICB%dmat_vp0(-1,1) * d_rj(ICB  )
!!                + fdm3e_ICB%dmat_vp0( 0,1) * d_rj(ICB+1)
!!                + fdm3e_ICB%dmat_vp0( 1,1) * d_rj(ICB+2)
!!      d2fdr2 =    fdm3e_ICB%dmat_vp0(-2,2) * dfdr(ICB  )
!!                + fdm3e_ICB%dmat_vp0(-1,2) * d_rj(ICB  )
!!                + fdm3e_ICB%dmat_vp0( 0,2) * d_rj(ICB+1)
!!                + fdm3e_ICB%dmat_vp0( 1,2) * d_rj(ICB+2)
!!      d3fdr3 =    fdm3e_ICB%dmat_vp0(-2,3) * dfdr(ICB  )
!!                + fdm3e_ICB%dmat_vp0(-1,3) * d_rj(ICB  )
!!                + fdm3e_ICB%dmat_vp0( 0,3) * d_rj(ICB+1)
!!                + fdm3e_ICB%dmat_vp0( 1,3) * dfdr(ICB+2)
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
        real(kind = kreal) :: dmat_vp0(-2:1,4)
      end type fdm3e_BC_hdiv
!
!
! -----------------------------------------------------------------------
!
      contains
!
! -----------------------------------------------------------------------
!
      subroutine check_3rd_ele_BC_vpol_fdm(id_file, fdm3e_BC0)
!
      integer(kind = kint), intent(in) :: id_file
      type(fdm3e_BC_hdiv), intent(in) :: fdm3e_BC0
!
!
      write(id_file,*) ' fdm3e_BC0%dmat_vp0'
      write(id_file,*) 'matrix for Interpolation'
      write(id_file,'(1p9E25.15e3)') fdm3e_BC0%dmat_vp0(-2:1,1)
      write(id_file,*) 'matrix for dfdr'
      write(id_file,'(1p9E25.15e3)') fdm3e_BC0%dmat_vp0(-2:1,2)
      write(id_file,*) 'matrix for d2fdr2'
      write(id_file,'(1p9E25.15e3)') fdm3e_BC0%dmat_vp0(-2:1,3)
      write(id_file,*) 'matrix for d3fdr3'
      write(id_file,'(1p9E25.15e3)') fdm3e_BC0%dmat_vp0(-2:1,4)
!
      end subroutine check_3rd_ele_BC_vpol_fdm
!
! -----------------------------------------------------------------------
!
      end module t_coef_fdm3e_MHD_boundaries
