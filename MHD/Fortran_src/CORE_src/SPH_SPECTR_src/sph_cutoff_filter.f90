!
!>@file   sph_cutoff_filter.f90
!!@brief  module sph_cutoff_filter
!!
!!@author H. Matsui
!!@date Programmed in Aug., 2013
!
!>@brief  Subroutines to reead control data
!!
!!@verbatim
!!      subroutine init_horizontal_cutoff_filter
!!      subroutine deallocate_sph_cutoff_filter
!!
!!      subroutine sph_horizontal_cutoff_scalar(nnod_rj, ntot_comp_rj,  &
!!     &          i_fld, i_filtered, d_rj)
!!      subroutine sph_horizontal_cutoff_vector(nnod_rj, ntot_comp_rj,  &
!!     &          i_fld, i_filtered, d_rj)
!!      subroutine sph_horizontal_cutoff_tensor(nnod_rj, ntot_comp_rj,  &
!!     &          i_fld, i_filtered, d_rj)
!!@endverbatim
!!
!!@n @param  nnod_rj         number of data points for spectr data
!!@n @param  ntot_comp_rj    total number of components for spectr data
!!@n @param  i_fld           start address for input field
!!@n @param  i_filtered      start address for filtered field
!!@n @param  d_rj            spectr data
!
      module sph_cutoff_filter
!
      use m_precision
      use m_constants
      use m_machine_parameter
      use m_spheric_parameter
!
      implicit none
!
!>      Cutoff degre for filtering
      integer(kind = kint) :: ltr_lowpass
!
!>      Filter coefficients for each spherical harmonics
      real(kind = kreal), allocatable :: c_filter(:)
!
      private :: c_filter
      private :: allocate_sph_cutoff_filter
!
!   --------------------------------------------------------------------
!
      contains
!
!   --------------------------------------------------------------------
!
      subroutine allocate_sph_cutoff_filter
!
      integer(kind = kint) :: num
!
!
      num = nidx_rj(2)
      allocate(c_filter(num))
      c_filter = zero
!
      end subroutine allocate_sph_cutoff_filter
!
!   --------------------------------------------------------------------
!
      subroutine deallocate_sph_cutoff_filter
!
!
      deallocate(c_filter)
!
      end subroutine deallocate_sph_cutoff_filter
!
!   --------------------------------------------------------------------
!   --------------------------------------------------------------------
!
      subroutine init_horizontal_cutoff_filter
!
      integer(kind = kint) :: j
!
!
      call allocate_sph_cutoff_filter
!
!$omp parallel do
      do j = 1, nidx_rj(2)
        if(idx_gl_1d_rj_j(j,2) .le. ltr_lowpass) c_filter(j) = one
      end do
!$omp end parallel do
!
      end subroutine init_horizontal_cutoff_filter
!
!   --------------------------------------------------------------------
!   --------------------------------------------------------------------
!
      subroutine sph_horizontal_cutoff_scalar(nnod_rj, ntot_comp_rj,    &
     &          i_fld, i_filtered, d_rj)
!
      integer(kind = kint), intent(in) :: i_filtered, i_fld
      integer(kind = kint), intent(in) :: nnod_rj, ntot_comp_rj
      real(kind= kreal), intent(inout) :: d_rj(nnod_rj, ntot_comp_rj)
!
      integer(kind = kint) :: inod, kr, j
!
!$omp parallel do private(kr,j,inod)
      do kr = 1, nidx_rj(1)
        do j = 1, nidx_rj(2)
          inod = j + (kr-1)*nidx_rj(2)
          d_rj(inod,i_filtered  ) = c_filter(j) * d_rj(inod,i_fld  )
        end do
      end do
!$omp end parallel do
!
      end subroutine sph_horizontal_cutoff_scalar
!
!   --------------------------------------------------------------------
!
      subroutine sph_horizontal_cutoff_vector(nnod_rj, ntot_comp_rj,    &
     &          i_fld, i_filtered, d_rj)
!
      integer(kind = kint), intent(in) :: i_filtered, i_fld
      integer(kind = kint), intent(in) :: nnod_rj, ntot_comp_rj
      real(kind= kreal), intent(inout) :: d_rj(nnod_rj, ntot_comp_rj)
!
      integer(kind = kint) :: inod, kr, j
!
!
!$omp parallel do private(kr,j,inod)
      do kr = 1, nidx_rj(1)
        do j = 1, nidx_rj(2)
          inod = j + (kr-1)*nidx_rj(2)
          d_rj(inod,i_filtered  ) = c_filter(j) * d_rj(inod,i_fld  )
          d_rj(inod,i_filtered+1) = c_filter(j) * d_rj(inod,i_fld+1)
          d_rj(inod,i_filtered+2) = c_filter(j) * d_rj(inod,i_fld+2)
        end do
      end do
!$omp end parallel do
!
      end subroutine sph_horizontal_cutoff_vector
!
!   --------------------------------------------------------------------
!
      subroutine sph_horizontal_cutoff_tensor(nnod_rj, ntot_comp_rj,    &
     &          i_fld, i_filtered, d_rj)
!
      integer(kind = kint), intent(in) :: i_filtered, i_fld
      integer(kind = kint), intent(in) :: nnod_rj, ntot_comp_rj
      real(kind= kreal), intent(inout) :: d_rj(nnod_rj, ntot_comp_rj)
!
      integer(kind = kint) :: inod, kr, j
!
!
!$omp parallel do private(kr,j,inod)
      do kr = 1, nidx_rj(1)
        do j = 1, nidx_rj(2)
          inod = j + (kr-1)*nidx_rj(2)
          d_rj(inod,i_filtered  ) = c_filter(j) * d_rj(inod,i_fld  )
          d_rj(inod,i_filtered+1) = c_filter(j) * d_rj(inod,i_fld+1)
          d_rj(inod,i_filtered+2) = c_filter(j) * d_rj(inod,i_fld+2)
          d_rj(inod,i_filtered+3) = c_filter(j) * d_rj(inod,i_fld+3)
          d_rj(inod,i_filtered+4) = c_filter(j) * d_rj(inod,i_fld+4)
          d_rj(inod,i_filtered+5) = c_filter(j) * d_rj(inod,i_fld+5)
        end do
      end do
!$omp end parallel do
!
      end subroutine sph_horizontal_cutoff_tensor
!
!   --------------------------------------------------------------------
!
      end module sph_cutoff_filter
