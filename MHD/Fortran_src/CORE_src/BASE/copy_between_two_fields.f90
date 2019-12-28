!>@file   copy_between_two_fields.f90
!!        module copy_between_two_fields
!!
!!@author H. Matsui
!!@date    programmed by H.Matsui on Apr., 2006
!!
!>@brief copy fields between two data arrays
!!
!!@verbatim
!!      subroutine copy_scalar_2_scalar_fld                             &
!!     &         (ifld_org, nnod_org, ntot_org_phys, d_org,             &
!!     &          ifld_dst, nnod_dst, ntot_dst_phys, d_dst)
!!      subroutine copy_vector_2_vector_fld                             &
!!     &         (ifld_org, nnod_org, ntot_org_phys, d_org,             &
!!     &          ifld_dst, nnod_dst, ntot_dst_phys, d_dst)
!!      subroutine copy_tensor_2_tensor_fld                             &
!!     &         (ifld_org, nnod_org, ntot_org_phys, d_org,             &
!!     &          ifld_dst, nnod_dst, ntot_dst_phys, d_dst)
!!      subroutine copy_fields_2_fields(numdir,                         &
!!     &          ifld_org, nnod_org, ntot_org_phys, d_org,             &
!!     &          ifld_dst, nnod_dst, ntot_dst_phys, d_dst)
!!
!!      subroutine fill_rest_scalar_field                               &
!!     &         (ifld_org, nnod_org, ntot_org_phys, d_org,             &
!!     &          ifld_dst, numnod, internal_node, ntot_dst_phys, d_dst)
!!      subroutine fill_rest_vector_field                               &
!!     &         (ifld_org, nnod_org, ntot_org_phys, d_org,             &
!!     &          ifld_dst, numnod, internal_node, ntot_dst_phys, d_dst)
!!      subroutine fill_rest_tensor_field                               &
!!     &         (ifld_org, nnod_org, ntot_org_phys, d_org,             &
!!     &          ifld_dst, numnod, internal_node, ntot_dst_phys, d_dst)
!!      subroutine fill_rest_fields(numdir,                             &
!!     &          ifld_org, ist_fill, ntot_org_phys, d_org,             &
!!     &          ifld_dst, numnod, internal_node, ntot_dst_phys, d_dst)
!!@endverbatim
!!
!!@param numdir          number of component
!!@param ifld_org        field ID for original data
!!@param nnod_org        number of node for original data
!!@param ntot_org_phys   total number of components for original field
!!@param d_org           original field data
!!
!!@param ifld_dst        field ID for copied data
!!@param nnod_dst        number of node for copied data
!!@param ntot_dst_phys   total number of components for copied field
!!@param d_dst           copied field data
!!
!!@param numnod          number of node for copied data
!!@param internal_node   number of internal node for copied data
!
      module copy_between_two_fields
!
      use m_precision
!
      implicit  none
!
! -------------------------------------------------------------------
!
      contains
!
! -------------------------------------------------------------------
!
      subroutine copy_scalar_2_scalar_fld                               &
     &         (ifld_org, nnod_org, ntot_org_phys, d_org,               &
     &          ifld_dst, nnod_dst, ntot_dst_phys, d_dst)
!
      integer(kind = kint), intent(in) :: ifld_org, ifld_dst
      integer(kind = kint), intent(in) :: nnod_org, ntot_org_phys
      integer(kind = kint), intent(in) :: nnod_dst, ntot_dst_phys
      real(kind = kreal), intent(in) :: d_org(nnod_org,ntot_org_phys)
      real(kind = kreal), intent(inout) :: d_dst(nnod_dst,ntot_dst_phys)
!
      integer(kind = kint) :: inod, num
!
!
      num = min(nnod_org, nnod_dst)
!$omp parallel do
      do inod = 1, num
        d_dst(inod,ifld_dst) = d_org(inod,ifld_org)
      end do
!$omp end parallel do
!
      end subroutine copy_scalar_2_scalar_fld
!
! -------------------------------------------------------------------
!
      subroutine copy_vector_2_vector_fld                               &
     &         (ifld_org, nnod_org, ntot_org_phys, d_org,               &
     &          ifld_dst, nnod_dst, ntot_dst_phys, d_dst)
!
      integer(kind = kint), intent(in) :: ifld_org, ifld_dst
      integer(kind = kint), intent(in) :: nnod_org, ntot_org_phys
      integer(kind = kint), intent(in) :: nnod_dst, ntot_dst_phys
      real(kind=kreal), intent(in) :: d_org(nnod_org, ntot_org_phys)
      real(kind=kreal), intent(inout) :: d_dst(nnod_dst,ntot_dst_phys)
!
      integer(kind = kint) :: inod, num
!
!
      num = min(nnod_org, nnod_dst)
!$omp parallel do
      do inod = 1, num
        d_dst(inod,ifld_dst  ) = d_org(inod,ifld_org  )
        d_dst(inod,ifld_dst+1) = d_org(inod,ifld_org+1)
        d_dst(inod,ifld_dst+2) = d_org(inod,ifld_org+2)
      end do
!$omp end parallel do
!
      end subroutine copy_vector_2_vector_fld
!
! -------------------------------------------------------------------
!
      subroutine copy_tensor_2_tensor_fld                               &
     &         (ifld_org, nnod_org, ntot_org_phys, d_org,               &
     &          ifld_dst, nnod_dst, ntot_dst_phys, d_dst)
!
      integer(kind = kint), intent(in) :: ifld_org, ifld_dst
      integer(kind = kint), intent(in) :: nnod_org, ntot_org_phys
      integer(kind = kint), intent(in) :: nnod_dst, ntot_dst_phys
      real(kind = kreal), intent(in) :: d_org(nnod_org,ntot_org_phys)
      real(kind=kreal), intent(inout) :: d_dst(nnod_dst,ntot_dst_phys)
!
      integer(kind = kint) :: inod, num
!
      num = min(nnod_org, nnod_dst)
!$omp parallel do
      do inod = 1, num
        d_dst(inod,ifld_dst  ) = d_org(inod,ifld_org  )
        d_dst(inod,ifld_dst+1) = d_org(inod,ifld_org+1)
        d_dst(inod,ifld_dst+2) = d_org(inod,ifld_org+2)
        d_dst(inod,ifld_dst+3) = d_org(inod,ifld_org+3)
        d_dst(inod,ifld_dst+4) = d_org(inod,ifld_org+4)
        d_dst(inod,ifld_dst+5) = d_org(inod,ifld_org+5)
      end do
!$omp end parallel do
!
      end subroutine copy_tensor_2_tensor_fld
!
! -------------------------------------------------------------------
!
      subroutine copy_fields_2_fields(numdir,                           &
     &          ifld_org, nnod_org, ntot_org_phys, d_org,               &
     &          ifld_dst, nnod_dst, ntot_dst_phys, d_dst)
!
      integer(kind = kint), intent(in) :: numdir
      integer(kind = kint), intent(in) :: ifld_org, ifld_dst
      integer(kind = kint_gl), intent(in) :: nnod_org, nnod_dst
      integer(kind = kint), intent(in) :: ntot_org_phys, ntot_dst_phys
      real(kind = kreal), intent(in) :: d_org(nnod_org,ntot_org_phys)
      real(kind=kreal), intent(inout) :: d_dst(nnod_dst,ntot_dst_phys)
!
      integer(kind = kint) :: nd
      integer(kind = kint_gl) :: num, inod
!
!
      num = min(nnod_org, nnod_dst)
!$omp parallel private(inod)
      do nd = 1, numdir
!$omp do
        do inod = 1, num
          d_dst(inod,ifld_dst+nd-1) = d_org(inod,ifld_org+nd-1)
        end do
!$omp end do nowait
      end do
!$omp end parallel
!
      end subroutine copy_fields_2_fields
!
! -------------------------------------------------------------------
! -------------------------------------------------------------------
!
      subroutine fill_rest_scalar_field                                 &
     &         (ifld_org, ist_fill, ntot_org_phys, d_org,               &
     &          ifld_dst, numnod, internal_node, ntot_dst_phys, d_dst)
!
      integer(kind = kint), intent(in) :: ifld_org, ifld_dst
      integer(kind = kint), intent(in) :: ist_fill, ntot_org_phys
      integer(kind = kint), intent(in) :: numnod, internal_node
      integer(kind = kint), intent(in) :: ntot_dst_phys
      real(kind = kreal), intent(in) :: d_org(numnod,ntot_org_phys)
      real(kind = kreal), intent(inout) :: d_dst(numnod,ntot_dst_phys)
!
      integer(kind = kint) :: inod
!
!
!
      if(ist_fill .ge. internal_node) return
!
!$omp parallel do
      do inod = ist_fill+1, internal_node
        d_dst(inod,ifld_dst) = d_org(inod,ifld_org)
      end do
!$omp end parallel do
!
      end subroutine fill_rest_scalar_field
!
! -------------------------------------------------------------------
!
      subroutine fill_rest_vector_field                                 &
     &         (ifld_org, ist_fill, ntot_org_phys, d_org,               &
     &          ifld_dst, numnod, internal_node, ntot_dst_phys, d_dst)
!
      integer(kind = kint), intent(in) :: ifld_org, ifld_dst
      integer(kind = kint), intent(in) :: ist_fill, ntot_org_phys
      integer(kind = kint), intent(in) :: numnod, internal_node
      integer(kind = kint), intent(in) :: ntot_dst_phys
      real(kind=kreal), intent(in) :: d_org(numnod, ntot_org_phys)
      real(kind=kreal), intent(inout) :: d_dst(numnod,ntot_dst_phys)
!
      integer(kind = kint) :: inod
!
!
      if(ist_fill .ge. internal_node) return
!
!$omp parallel do
      do inod = ist_fill+1, internal_node
        d_dst(inod,ifld_dst  ) = d_org(inod,ifld_org  )
        d_dst(inod,ifld_dst+1) = d_org(inod,ifld_org+1)
        d_dst(inod,ifld_dst+2) = d_org(inod,ifld_org+2)
      end do
!$omp end parallel do
!
      end subroutine fill_rest_vector_field
!
! -------------------------------------------------------------------
!
      subroutine fill_rest_tensor_field                                 &
     &         (ifld_org, ist_fill, ntot_org_phys, d_org,               &
     &          ifld_dst, numnod, internal_node, ntot_dst_phys, d_dst)
!
      integer(kind = kint), intent(in) :: ifld_org, ifld_dst
      integer(kind = kint), intent(in) :: ist_fill, ntot_org_phys
      integer(kind = kint), intent(in) :: numnod, internal_node
      integer(kind = kint), intent(in) :: ntot_dst_phys
      real(kind = kreal), intent(in) :: d_org(numnod,ntot_org_phys)
      real(kind=kreal), intent(inout) :: d_dst(numnod,ntot_dst_phys)
!
      integer(kind = kint) :: inod
!
!
      if(ist_fill .ge. internal_node) return
!
!$omp parallel do
      do inod = ist_fill+1, internal_node
        d_dst(inod,ifld_dst  ) = d_org(inod,ifld_org  )
        d_dst(inod,ifld_dst+1) = d_org(inod,ifld_org+1)
        d_dst(inod,ifld_dst+2) = d_org(inod,ifld_org+2)
        d_dst(inod,ifld_dst+3) = d_org(inod,ifld_org+3)
        d_dst(inod,ifld_dst+4) = d_org(inod,ifld_org+4)
        d_dst(inod,ifld_dst+5) = d_org(inod,ifld_org+5)
      end do
!$omp end parallel do
!
      end subroutine fill_rest_tensor_field
!
! -------------------------------------------------------------------
!
      subroutine fill_rest_fields(numdir,                               &
     &          ifld_org, ist_fill, ntot_org_phys, d_org,               &
     &          ifld_dst, numnod, internal_node, ntot_dst_phys, d_dst)
!
      integer(kind = kint), intent(in) :: numdir
      integer(kind = kint), intent(in) :: ifld_org, ifld_dst
      integer(kind = kint), intent(in) :: ist_fill, ntot_org_phys
      integer(kind = kint), intent(in) :: numnod, internal_node
      integer(kind = kint), intent(in) :: ntot_dst_phys
      real(kind = kreal), intent(in) :: d_org(numnod,ntot_org_phys)
      real(kind=kreal), intent(inout) :: d_dst(numnod,ntot_dst_phys)
!
      integer(kind = kint) :: inod, nd
!
!
      if(ist_fill .ge. internal_node) return
!
!$omp parallel private(inod)
      do nd = 1, numdir
!$omp do
        do inod = ist_fill+1, internal_node
          d_dst(inod,ifld_dst+nd-1) = d_org(inod,ifld_org+nd-1)
        end do
!$omp end do nowait
      end do
!$omp end parallel
!
      end subroutine fill_rest_fields
!
! -------------------------------------------------------------------
!
      end module copy_between_two_fields
