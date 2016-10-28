!>@file   sph_radial_filtering.f90
!!@brief  module sph_radial_filtering
!!
!!@author H. Matsui
!!@date Programmed on  Oct., 2016
!
!>@brief  Evaluate radial filtering
!!
!!@verbatim
!!      subroutine vector_sph_radial_filter(nri_filter, ntot_filter,    &
!!     &          istack_near_kr, kr_filter, kr_neib, weight,  nidx_rj, &
!!     &          i_field, i_filter, nnod_rj, ntot_comp, d_rj)
!!      subroutine scalar_sph_radial_filter(nri_filter, ntot_filter,    &
!!     &          istack_near_kr, kr_filter, kr_neib, weight,  nidx_rj, &
!!     &          i_field, i_filter, nnod_rj, ntot_comp, d_rj)
!!      subroutine sym_tensor_sph_radial_filter(nri_filter, ntot_filter,&
!!     &          istack_near_kr, kr_filter, kr_neib, weight,  nidx_rj, &
!!     &          i_field, i_filter, nnod_rj, ntot_comp, d_rj)
!!
!!      subroutine overwrt_vect_sph_horiz_filter                        &
!!     &         (ltr_filter, weight, nidx_rj, idx_gl_1d_rj_j,          &
!!     &          i_filter, nnod_rj, ntot_comp, d_rj)
!!      subroutine overwrt_scl_sph_horiz_filter                         &
!!     &         (ltr_filter, weight, nidx_rj, idx_gl_1d_rj_j,          &
!!     &          i_filter, nnod_rj, ntot_comp, d_rj)
!!      subroutine overwrt_tsr_sph_horiz_filter                         &
!!     &         (ltr_filter, weight, nidx_rj, idx_gl_1d_rj_j,          &
!!     &          i_filter, nnod_rj, ntot_comp, d_rj)
!!@endverbatim
!!
!
      module sph_radial_filtering
!
      use m_precision
      use m_constants
!
      implicit none
!
!
! ----------------------------------------------------------------------
!
      contains
!
! ----------------------------------------------------------------------
!
      subroutine vector_sph_radial_filter(nri_filter, ntot_filter,      &
     &          istack_near_kr, kr_filter, kr_neib, weight,  nidx_rj,   &
     &          i_field, i_filter, nnod_rj, ntot_comp, d_rj)
!
      integer(kind = kint),  intent(in) :: nri_filter, ntot_filter
      integer(kind = kint),  intent(in)                                 &
     &                     :: istack_near_kr(0:nri_filter)
      integer(kind = kint),  intent(in) :: kr_filter(nri_filter)
      integer(kind = kint),  intent(in) :: kr_neib(ntot_filter)
      real(kind = kreal), intent(in) :: weight(ntot_filter)
!
      integer(kind = kint),  intent(in) :: nidx_rj(2)
      integer(kind = kint),  intent(in) :: nnod_rj, ntot_comp
      integer(kind = kint),  intent(in) :: i_field, i_filter
      real(kind = kreal), intent(inout) :: d_rj(nnod_rj, ntot_comp)
!
      integer(kind = kint) :: inum, jst, jed, jnum ,j, inod, jnod
!
!
!$omp parallel workshare
      d_rj(1:nnod_rj,i_filter:i_filter+2) = zero
!$omp end parallel workshare
!
!$omp parallel do private(inum,jst,jed,jnum,j,inod,jnod)
      do inum = 1, nri_filter
        jst = istack_near_kr(inum-1) + 1
        jed = istack_near_kr(inum  )
        do jnum = jst, jed
          do j = 1, nidx_rj(2)
            inod = j + (kr_filter(inum)-1) * nidx_rj(2)
            jnod = j + (kr_neib(jnum)-1) * nidx_rj(2)
            d_rj(inod,i_filter  ) = d_rj(inod,i_filter  )               &
     &                           + d_rj(jnod,i_field  ) * weight(jnum)
            d_rj(inod,i_filter+1) = d_rj(inod,i_filter+1)               &
     &                           + d_rj(jnod,i_field+1) * weight(jnum)
            d_rj(inod,i_filter+2) = d_rj(inod,i_filter+2)               &
     &                           + d_rj(jnod,i_field+2) * weight(jnum)
          end do
        end do
      end do
!$omp end parallel do
!
      end subroutine vector_sph_radial_filter
!
! ----------------------------------------------------------------------
!
      subroutine scalar_sph_radial_filter(nri_filter, ntot_filter,      &
     &          istack_near_kr, kr_filter, kr_neib, weight, nidx_rj,    &
     &          i_field, i_filter, nnod_rj, ntot_comp, d_rj)
!
      integer(kind = kint),  intent(in) :: nri_filter, ntot_filter
      integer(kind = kint),  intent(in)                                 &
     &                     :: istack_near_kr(0:nri_filter)
      integer(kind = kint),  intent(in) :: kr_filter(nri_filter)
      integer(kind = kint),  intent(in) :: kr_neib(ntot_filter)
      real(kind = kreal), intent(in) :: weight(ntot_filter)
!
      integer(kind = kint),  intent(in) :: nidx_rj(2)
      integer(kind = kint),  intent(in) :: nnod_rj, ntot_comp
      integer(kind = kint),  intent(in) :: i_field, i_filter
      real(kind = kreal), intent(inout) :: d_rj(nnod_rj, ntot_comp)
!
      integer(kind = kint) :: inum, jst, jed, jnum ,j, inod, jnod
!
!
!$omp parallel workshare
      d_rj(1:nnod_rj,i_filter  ) = zero
!$omp end parallel workshare
!
!$omp parallel do private(inum,jst,jed,jnum,j,inod,jnod)
      do inum = 1, nri_filter
        jst = istack_near_kr(inum-1) + 1
        jed = istack_near_kr(inum  )
        do jnum = jst, jed
          do j = 1, nidx_rj(2)
            inod = j + (kr_filter(inum)-1) * nidx_rj(2)
            jnod = j + (kr_neib(jnum)-1) * nidx_rj(2)
            d_rj(inod,i_filter  ) = d_rj(inod,i_filter  )               &
     &                           + d_rj(jnod,i_field  ) * weight(jnum)
          end do
        end do
      end do
!$omp end parallel do
!
      end subroutine scalar_sph_radial_filter
!
! ----------------------------------------------------------------------
!
      subroutine sym_tensor_sph_radial_filter(nri_filter, ntot_filter,  &
     &          istack_near_kr, kr_filter, kr_neib, weight, nidx_rj,    &
     &          i_field, i_filter, nnod_rj, ntot_comp, d_rj)
!
      integer(kind = kint),  intent(in) :: nri_filter, ntot_filter
      integer(kind = kint),  intent(in)                                 &
     &                     :: istack_near_kr(0:nri_filter)
      integer(kind = kint),  intent(in) :: kr_filter(nri_filter)
      integer(kind = kint),  intent(in) :: kr_neib(ntot_filter)
      real(kind = kreal), intent(in) :: weight(ntot_filter)
!
      integer(kind = kint),  intent(in) :: nidx_rj(2)
      integer(kind = kint),  intent(in) :: nnod_rj, ntot_comp
      integer(kind = kint),  intent(in) :: i_field, i_filter
      real(kind = kreal), intent(inout) :: d_rj(nnod_rj, ntot_comp)
!
      integer(kind = kint) :: inum, jst, jed, jnum ,j, inod, jnod
!
!
!$omp parallel workshare
      d_rj(1:nnod_rj,i_filter:i_filter+5) = zero
!$omp end parallel workshare
!
!$omp parallel do private(inum,jst,jed,jnum,j,inod,jnod)
      do inum = 1, nri_filter
        jst = istack_near_kr(inum-1) + 1
        jed = istack_near_kr(inum  )
        do jnum = jst, jed
          do j = 1, nidx_rj(2)
            inod = j + (kr_filter(inum)-1) * nidx_rj(2)
            jnod = j + (kr_neib(jnum)-1) * nidx_rj(2)
            d_rj(inod,i_filter  ) = d_rj(inod,i_filter  )               &
     &                           + d_rj(jnod,i_field  ) * weight(jnum)
            d_rj(inod,i_filter+1) = d_rj(inod,i_filter+1)               &
     &                           + d_rj(jnod,i_field+1) * weight(jnum)
            d_rj(inod,i_filter+2) = d_rj(inod,i_filter+2)               &
     &                           + d_rj(jnod,i_field+2) * weight(jnum)
            d_rj(inod,i_filter+3) = d_rj(inod,i_filter+3)               &
     &                           + d_rj(jnod,i_field+3) * weight(jnum)
            d_rj(inod,i_filter+4) = d_rj(inod,i_filter+4)               &
     &                           + d_rj(jnod,i_field+4) * weight(jnum)
            d_rj(inod,i_filter+5) = d_rj(inod,i_filter+5)               &
     &                           + d_rj(jnod,i_field+5) * weight(jnum)
          end do
        end do
      end do
!$omp end parallel do
!
      end subroutine sym_tensor_sph_radial_filter
!
! ----------------------------------------------------------------------
! ----------------------------------------------------------------------
!
      subroutine overwrt_vect_sph_horiz_filter                          &
     &         (ltr_filter, weight, nidx_rj, idx_gl_1d_rj_j,            &
     &          i_filter, nnod_rj, ntot_comp, d_rj)
!
      integer(kind = kint),  intent(in) :: ltr_filter
      real(kind = kreal), intent(in) :: weight(0:ltr_filter)
!
      integer(kind = kint),  intent(in) :: nidx_rj(2)
      integer(kind = kint),  intent(in) :: idx_gl_1d_rj_j(nidx_rj(2),3)
      integer(kind = kint),  intent(in) :: nnod_rj, ntot_comp
      integer(kind = kint),  intent(in) :: i_filter
      real(kind = kreal), intent(inout) :: d_rj(nnod_rj, ntot_comp)
!
      integer(kind = kint) :: k, j, l, inod
!
!
!$omp parallel do private(k,j,l,inod)
      do k = 1, nidx_rj(1)
        do j = 1, nidx_rj(2)
          l = idx_gl_1d_rj_j(j,2)
          inod = j + (k-1) * nidx_rj(2)
          d_rj(inod,i_filter  ) = d_rj(inod,i_filter  ) * weight(l)
          d_rj(inod,i_filter+1) = d_rj(inod,i_filter+1) * weight(l)
          d_rj(inod,i_filter+2) = d_rj(inod,i_filter+2) * weight(l)
        end do
      end do
!$omp end parallel do
!
      end subroutine overwrt_vect_sph_horiz_filter
!
! ----------------------------------------------------------------------
!
      subroutine overwrt_scl_sph_horiz_filter                           &
     &         (ltr_filter, weight, nidx_rj, idx_gl_1d_rj_j,            &
     &          i_filter, nnod_rj, ntot_comp, d_rj)
!
      integer(kind = kint),  intent(in) :: ltr_filter
      real(kind = kreal), intent(in) :: weight(0:ltr_filter)
!
      integer(kind = kint),  intent(in) :: nidx_rj(2)
      integer(kind = kint),  intent(in) :: idx_gl_1d_rj_j(nidx_rj(2),3)
      integer(kind = kint),  intent(in) :: nnod_rj, ntot_comp
      integer(kind = kint),  intent(in) :: i_filter
      real(kind = kreal), intent(inout) :: d_rj(nnod_rj, ntot_comp)
!
      integer(kind = kint) :: k, j, l, inod
!
!
!$omp parallel do private(k,j,l,inod)
      do k = 1, nidx_rj(1)
        do j = 1, nidx_rj(2)
          l = idx_gl_1d_rj_j(j,2)
          inod = j + (k-1) * nidx_rj(2)
          d_rj(inod,i_filter  ) = d_rj(inod,i_filter  ) * weight(l)
        end do
      end do
!$omp end parallel do
!
      end subroutine overwrt_scl_sph_horiz_filter
!
! ----------------------------------------------------------------------
!
      subroutine overwrt_tsr_sph_horiz_filter                           &
     &         (ltr_filter, weight, nidx_rj, idx_gl_1d_rj_j,            &
     &          i_filter, nnod_rj, ntot_comp, d_rj)
!
      integer(kind = kint),  intent(in) :: ltr_filter
      real(kind = kreal), intent(in) :: weight(0:ltr_filter)
!
      integer(kind = kint),  intent(in) :: nidx_rj(2)
      integer(kind = kint),  intent(in) :: idx_gl_1d_rj_j(nidx_rj(2),3)
      integer(kind = kint),  intent(in) :: nnod_rj, ntot_comp
      integer(kind = kint),  intent(in) :: i_filter
      real(kind = kreal), intent(inout) :: d_rj(nnod_rj, ntot_comp)
!
      integer(kind = kint) :: k, j, l, inod
!
!
!$omp parallel do private(k,j,l,inod)
      do k = 1, nidx_rj(1)
        do j = 1, nidx_rj(2)
          l = idx_gl_1d_rj_j(j,2)
          inod = j + (k-1) * nidx_rj(2)
          d_rj(inod,i_filter  ) = d_rj(inod,i_filter  ) * weight(l)
          d_rj(inod,i_filter+1) = d_rj(inod,i_filter+1) * weight(l)
          d_rj(inod,i_filter+2) = d_rj(inod,i_filter+2) * weight(l)
          d_rj(inod,i_filter+3) = d_rj(inod,i_filter+3) * weight(l)
          d_rj(inod,i_filter+4) = d_rj(inod,i_filter+4) * weight(l)
          d_rj(inod,i_filter+5) = d_rj(inod,i_filter+5) * weight(l)
        end do
      end do
!$omp end parallel do
!
      end subroutine overwrt_tsr_sph_horiz_filter
!
! ----------------------------------------------------------------------
!
      end module sph_radial_filtering
