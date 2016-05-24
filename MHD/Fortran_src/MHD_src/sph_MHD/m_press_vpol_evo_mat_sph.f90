!>@file   m_press_vpol_evo_mat_sph.f90
!!@brief  module m_press_vpol_evo_mat_sph
!!
!!@author H. Matsui
!!@date Programmed in May., 2013
!
!>@brief Radial matrix for time evolution
!!        of poloidal velocity and pressure
!!
!!@verbatim
!!      subroutine allocate_press_vpol_mat_sph(sph_rj)
!!      subroutine deallocate_press_vpol_mat_sph
!!      subroutine check_velocity_matrices_sph(my_rank, sph_rj)
!!        type(sph_rj_grid), intent(inout) :: sph_rj
!!@endverbatim
!
!
      module m_press_vpol_evo_mat_sph
!
      use m_precision
      use t_spheric_rj_data
!
      implicit none
!
!>      7-band matrix for time evlution of poloidal velocity and pressure
      real(kind = kreal), allocatable :: vsp_evo_mat(:,:,:)
!>      LU-decompositted matrix for time evlution
!!      of poloidal velocity and pressure
      real(kind = kreal), allocatable :: vsp_evo_lu(:,:,:)
!>      Determinant of time evlution of poloidal velocity and pressure
      real(kind = kreal), allocatable :: vsp_evo_det(:,:)
!>      Pivot information
!!       for time evlution matrix of poloidal velocity and pressure
      integer(kind = kint), allocatable :: i_vsp_pivot(:,:)
!
! -----------------------------------------------------------------------
!
      contains
!
! -----------------------------------------------------------------------
!
      subroutine allocate_press_vpol_mat_sph(sph_rj)
!
      type(sph_rj_grid), intent(inout) :: sph_rj
!
      integer(kind = kint) :: nri, jmax
!
      nri =  sph_rj%nidx_rj(1)
      jmax = sph_rj%nidx_rj(2)
!
!
      allocate( vsp_evo_mat(7,2*nri,jmax) )
      allocate( vsp_evo_lu(13,2*nri,jmax) )
      allocate( vsp_evo_det(2*nri,jmax) )
      allocate( i_vsp_pivot(2*nri,jmax) )
!
      vsp_evo_mat =   0.0d0
      vsp_evo_lu =    0.0d0
      vsp_evo_det =   0.0d0
      i_vsp_pivot =   0
!
      vsp_evo_mat(4,1:2*nri,1:jmax) = 1.0d0
!
      end subroutine allocate_press_vpol_mat_sph
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine deallocate_press_vpol_mat_sph
!
!
      deallocate( vsp_evo_mat, vsp_evo_lu )
      deallocate( vsp_evo_det, i_vsp_pivot )
!
      end subroutine deallocate_press_vpol_mat_sph
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine check_velocity_matrices_sph(my_rank, sph_rj)
!
      use m_radial_matrices_sph
      use check_sph_radial_mat
!
      integer(kind = kint), intent(in) :: my_rank
      type(sph_rj_grid), intent(inout) :: sph_rj
!
!
      write(50+my_rank,'(a)') 'evolution matrix for poloidal velocity'
      call check_radial_7band_mat                                       &
     &   (my_rank, sph_rj%nidx_rj(1), sph_rj%nidx_rj(2),                &
     &    sph_rj%idx_gl_1d_rj_j, sph_rj%radius_1d_rj_r, vsp_evo_mat)
!
      write(50+my_rank,'(a)') 'evolution matrix for toroidal velocity'
      call check_radial_3band_mat                                       &
     &   (my_rank, sph_rj%nidx_rj(1), sph_rj%nidx_rj(2),                &
     &    sph_rj%idx_gl_1d_rj_j, sph_rj%radius_1d_rj_r,                 &
     &    band_vt_evo%mat)
!
      end subroutine check_velocity_matrices_sph
!
! -----------------------------------------------------------------------
!
      end module m_press_vpol_evo_mat_sph
