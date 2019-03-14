!set_each_refined_connect.f90
!      module set_each_refined_connect
!
!      Written by H. Matsui on Oct., 2007
!
!!      subroutine set_refined_connect_quad20(nele_refine, ist,         &
!!     &          inod_refine_local, ie_refine )
!!      subroutine set_refined_connect_quad27(nele_refine, ist,         &
!!     &          inod_refine_local, ie_refine )
!!
!!      subroutine set_refined_connect_dbl_w(nele_refine, ist,          &
!!     &          inod_refine_local, ie_refine )
!!      subroutine set_refined_connect_dbl_wx(nele_refine, ist,         &
!!     &          inod_refine_local, ie_refine )
!!      subroutine set_refined_connect_dbl_wy(nele_refine, ist,         &
!!     &          inod_refine_local, ie_refine )
!!      subroutine set_refined_connect_dbl_wz(nele_refine, ist,         &
!!     &          inod_refine_local, ie_refine )
!!
!!      subroutine set_refined_connect_tri_wx(nele_refine, ist,         &
!!     &          inod_refine_local, ie_refine )
!!      subroutine set_refined_connect_tri_wy(nele_refine, ist,         &
!!     &          inod_refine_local, ie_refine )
!!      subroutine set_refined_connect_tri_wz(nele_refine, ist,         &
!!     &          inod_refine_local, ie_refine )
!!      subroutine set_refined_connect_tri_1ds(nele_refine, ist,        &
!!     &          inod_refine_local, ie_refine )
!!
!!      subroutine set_refined_connect_tri_w(nele_refine, ist,          &
!!     &          inod_refine_local, ie_refine )
!!      subroutine set_refined_connect_tri_s(nele_refine, ist,          &
!!     &          inod_refine_local, ie_refine )
!!      subroutine set_refined_connect_tri_e(nele_refine, ist,          &
!!     &          inod_refine_local, ie_refine )
!!      subroutine set_refined_connect_tri_n(nele_refine, ist,          &
!!     &          inod_refine_local, ie_refine )
!!
!!      subroutine set_refined_connect(nele_sub, ie_new_sm_tri_e,       &
!!     &          nele_refine, ist, inod_refine_local, ie_refine)
!
      module set_each_refined_connect
!
      use m_precision
!
      use m_refined_connection_tbl
!
      implicit none
!
! ----------------------------------------------------------------------
!
      contains
!
! ----------------------------------------------------------------------
!
      subroutine set_no_refined_connect(nele_refine, ist,              &
     &          inod_refine_local, ie_refine )
!
      integer(kind = kint), intent(in) :: nele_refine
      integer(kind = kint), intent(in) :: ist
      integer(kind = kint), intent(in) :: inod_refine_local(8)
      integer(kind = kint), intent(inout) :: ie_refine(nele_refine,8)
!
      integer(kind = kint) :: k1
!
      do k1 = 1, 8
        ie_refine(ist+1,k1) = inod_refine_local(k1)
      end do
!
      end subroutine set_no_refined_connect
!
! ----------------------------------------------------------------------
!
      subroutine set_refined_connect_quad20(nele_refine, ist,           &
     &          inod_refine_local, ie_refine )
!
      integer(kind = kint), intent(in) :: nele_refine
      integer(kind = kint), intent(in) :: ist
      integer(kind = kint), intent(in) :: inod_refine_local(20)
      integer(kind = kint), intent(inout) :: ie_refine(nele_refine,20)
!
      integer(kind = kint) :: k1, l1
!
      do k1 = 1, 20
        l1 = ie_new_quad20(k1)
        ie_refine(ist+1,k1) = inod_refine_local(l1)
      end do
!
      end subroutine set_refined_connect_quad20
!
! ----------------------------------------------------------------------
!
      subroutine set_refined_connect_quad27(nele_refine, ist,           &
     &          inod_refine_local, ie_refine )
!
      integer(kind = kint), intent(in) :: nele_refine
      integer(kind = kint), intent(in) :: ist
      integer(kind = kint), intent(in) :: inod_refine_local(27)
      integer(kind = kint), intent(inout) :: ie_refine(nele_refine,27)
!
      integer(kind = kint) :: k1, l1
!
      do k1 = 1, 27
        l1 = ie_new_quad27(k1)
        ie_refine(ist+1,k1) = inod_refine_local(l1)
      end do
!
      end subroutine set_refined_connect_quad27
!
! ----------------------------------------------------------------------
! ----------------------------------------------------------------------
!
      subroutine set_refined_connect_dbl_w(nele_refine, ist,            &
     &          inod_refine_local, ie_refine )
!
      integer(kind = kint), intent(in) :: nele_refine
      integer(kind = kint), intent(in) :: ist
      integer(kind = kint), intent(in) :: inod_refine_local(27)
      integer(kind = kint), intent(inout) :: ie_refine(nele_refine,8)
!
      integer(kind = kint) :: inum, jnum, icou, k1, l1
!
      do inum = 1, 8
        do k1 = 1, 8
          jnum = k1 + (inum-1)*8
          icou = ist + inum
          l1 = ie_new_dbl_w(jnum)
          ie_refine(icou,k1) = inod_refine_local(l1)
        end do
      end do
!
      end subroutine set_refined_connect_dbl_w
!
! ----------------------------------------------------------------------
!
      subroutine set_refined_connect_dbl_wx(nele_refine, ist,           &
     &          inod_refine_local, ie_refine )
!
      integer(kind = kint), intent(in) :: nele_refine
      integer(kind = kint), intent(in) :: ist
      integer(kind = kint), intent(in) :: inod_refine_local(27)
      integer(kind = kint), intent(inout) :: ie_refine(nele_refine,8)
!
      integer(kind = kint) :: inum, jnum, icou, k1, l1
!
      do inum = 1, 4
        do k1 = 1, 8
          jnum = k1 + (inum-1)*8
          icou = ist + inum
          l1 = ie_new_dbl_wx(jnum)
          ie_refine(icou,k1) = inod_refine_local(l1)
        end do
      end do
!
      end subroutine set_refined_connect_dbl_wx
!
! ----------------------------------------------------------------------
!
      subroutine set_refined_connect_dbl_wy(nele_refine, ist,           &
     &          inod_refine_local, ie_refine )
!
      integer(kind = kint), intent(in) :: nele_refine
      integer(kind = kint), intent(in) :: ist
      integer(kind = kint), intent(in) :: inod_refine_local(27)
      integer(kind = kint), intent(inout) :: ie_refine(nele_refine,8)
!
      integer(kind = kint) :: inum, jnum, icou, k1, l1
!
      do inum = 1, 4
        do k1 = 1, 8
          jnum = k1 + (inum-1)*8
          icou = ist + inum
          l1 = ie_new_dbl_wy(jnum)
          ie_refine(icou,k1) = inod_refine_local(l1)
        end do
      end do
!
      end subroutine set_refined_connect_dbl_wy
!
! ----------------------------------------------------------------------
!
      subroutine set_refined_connect_dbl_wz(nele_refine, ist,           &
     &          inod_refine_local, ie_refine )
!
      integer(kind = kint), intent(in) :: nele_refine
      integer(kind = kint), intent(in) :: ist
      integer(kind = kint), intent(in) :: inod_refine_local(27)
      integer(kind = kint), intent(inout) :: ie_refine(nele_refine,8)
!
      integer(kind = kint) :: inum, jnum, icou, k1, l1
!
      do inum = 1, 4
        do k1 = 1, 8
          jnum = k1 + (inum-1)*8
          icou = ist + inum
          l1 = ie_new_dbl_wz(jnum)
          ie_refine(icou,k1) = inod_refine_local(l1)
        end do
      end do
!
      end subroutine set_refined_connect_dbl_wz
!
! ----------------------------------------------------------------------
! ----------------------------------------------------------------------
!
      subroutine set_refined_connect_tri_wx(nele_refine, ist,           &
     &          inod_refine_local, ie_refine )
!
      integer(kind = kint), intent(in) :: nele_refine
      integer(kind = kint), intent(in) :: ist
      integer(kind = kint), intent(in) :: inod_refine_local(64)
      integer(kind = kint), intent(inout) :: ie_refine(nele_refine,8)
!
      integer(kind = kint) :: inum, jnum, icou, k1, l1
!
      do inum = 1, 9
        do k1 = 1, 8
          jnum = k1 + (inum-1)*8
          icou = ist + inum
          l1 = ie_new_tri_wx(jnum)
          ie_refine(icou,k1) = inod_refine_local(l1)
        end do
      end do
!
      end subroutine set_refined_connect_tri_wx
!
! ----------------------------------------------------------------------
!
      subroutine set_refined_connect_tri_wy(nele_refine, ist,           &
     &          inod_refine_local, ie_refine )
!
      integer(kind = kint), intent(in) :: nele_refine
      integer(kind = kint), intent(in) :: ist
      integer(kind = kint), intent(in) :: inod_refine_local(64)
      integer(kind = kint), intent(inout) :: ie_refine(nele_refine,8)
!
      integer(kind = kint) :: inum, jnum, icou, k1, l1
!
      do inum = 1, 9
        do k1 = 1, 8
          jnum = k1 + (inum-1)*8
          icou = ist + inum
          l1 = ie_new_tri_wy(jnum)
          ie_refine(icou,k1) = inod_refine_local(l1)
        end do
      end do
!
      end subroutine set_refined_connect_tri_wy
!
! ----------------------------------------------------------------------
!
      subroutine set_refined_connect_tri_wz(nele_refine, ist,           &
     &          inod_refine_local, ie_refine )
!
      integer(kind = kint), intent(in) :: nele_refine
      integer(kind = kint), intent(in) :: ist
      integer(kind = kint), intent(in) :: inod_refine_local(64)
      integer(kind = kint), intent(inout) :: ie_refine(nele_refine,8)
!
      integer(kind = kint) :: inum, jnum, icou, k1, l1
!
      do inum = 1, 9
        do k1 = 1, 8
          jnum = k1 + (inum-1)*8
          icou = ist + inum
          l1 = ie_new_tri_wz(jnum)
          ie_refine(icou,k1) = inod_refine_local(l1)
        end do
      end do
!
      end subroutine set_refined_connect_tri_wz
!
! ----------------------------------------------------------------------
!
      subroutine set_refined_connect_tri_1ds(nele_refine, ist,          &
     &          inod_refine_local, ie_refine )
!
      integer(kind = kint), intent(in) :: nele_refine
      integer(kind = kint), intent(in) :: ist
      integer(kind = kint), intent(in) :: inod_refine_local(52)
      integer(kind = kint), intent(inout) :: ie_refine(nele_refine,8)
!
      integer(kind = kint) :: inum, jnum, icou, k1, l1
!
      do inum = 1, 13
        do k1 = 1, 8
          jnum = k1 + (inum-1)*8
          icou = ist + inum
          l1 = ie_new_tri_zs(jnum)
          ie_refine(icou,k1) = inod_refine_local(l1)
        end do
      end do
!
      end subroutine set_refined_connect_tri_1ds
!
! ----------------------------------------------------------------------
! ----------------------------------------------------------------------
!
      subroutine set_refined_connect_tri_w(nele_refine, ist,            &
     &          inod_refine_local, ie_refine )
!
      integer(kind = kint), intent(in) :: nele_refine
      integer(kind = kint), intent(in) :: ist
      integer(kind = kint), intent(in) :: inod_refine_local(64)
      integer(kind = kint), intent(inout) :: ie_refine(nele_refine,8)
!
      integer(kind = kint) :: inum, jnum, icou, k1, l1
!
      do inum = 1, 27
        do k1 = 1, 8
          jnum = k1 + (inum-1)*8
          icou = ist + inum
          l1 = ie_new_tri_w(jnum)
          ie_refine(icou,k1) = inod_refine_local(l1)
        end do
      end do
!
      end subroutine set_refined_connect_tri_w
!
! ----------------------------------------------------------------------
!
      subroutine set_refined_connect_tri_s(nele_refine, ist,            &
     &          inod_refine_local, ie_refine )
!
      integer(kind = kint), intent(in) :: nele_refine
      integer(kind = kint), intent(in) :: ist
      integer(kind = kint), intent(in) :: inod_refine_local(52)
      integer(kind = kint), intent(inout) :: ie_refine(nele_refine,8)
!
      integer(kind = kint) :: inum, jnum, icou, k1, l1
!
      do inum = 1, 22
        do k1 = 1, 8
          jnum = k1 + (inum-1)*8
          icou = ist + inum
          l1 = ie_new_tri_s(jnum)
          ie_refine(icou,k1) = inod_refine_local(l1)
        end do
      end do
!
      end subroutine set_refined_connect_tri_s
!
! ----------------------------------------------------------------------
!
      subroutine set_refined_connect_tri_e(nele_refine, ist,            &
     &          inod_refine_local, ie_refine )
!
      integer(kind = kint), intent(in) :: nele_refine
      integer(kind = kint), intent(in) :: ist
      integer(kind = kint), intent(in) :: inod_refine_local(30)
      integer(kind = kint), intent(inout) :: ie_refine(nele_refine,8)
!
      integer(kind = kint) :: inum, jnum, icou, k1, l1
!
      do inum = 1, 11
        do k1 = 1, 8
          jnum = k1 + (inum-1)*8
          icou = ist + inum
          l1 = ie_new_tri_e(jnum)
          ie_refine(icou,k1) = inod_refine_local(l1)
        end do
      end do
!
      end subroutine set_refined_connect_tri_e
!
! ----------------------------------------------------------------------
!
      subroutine set_refined_connect_tri_n(nele_refine, ist,            &
     &          inod_refine_local, ie_refine )
!
      integer(kind = kint), intent(in) :: nele_refine
      integer(kind = kint), intent(in) :: ist
      integer(kind = kint), intent(in) :: inod_refine_local(16)
      integer(kind = kint), intent(inout) :: ie_refine(nele_refine,8)
!
      integer(kind = kint) :: inum, jnum, icou, k1, l1
!
      do inum = 1, 4
        do k1 = 1, 8
          jnum = k1 + (inum-1)*8
          icou = ist + inum
          l1 = ie_new_tri_n(jnum)
          ie_refine(icou,k1) = inod_refine_local(l1)
        end do
      end do
!
      end subroutine set_refined_connect_tri_n
!
! ----------------------------------------------------------------------
! ----------------------------------------------------------------------
!
      subroutine set_refined_connect(nele_sub, ie_new_sm_tri_e,         &
     &          nele_refine, ist, inod_refine_local, ie_refine )
!
      integer(kind = kint), intent(in) :: nele_sub
      integer(kind = kint), intent(in) :: ie_new_sm_tri_e(nele_sub*8)
      integer(kind = kint), intent(in) :: nele_refine
      integer(kind = kint), intent(in) :: ist
      integer(kind = kint), intent(in) :: inod_refine_local(64)
      integer(kind = kint), intent(inout) :: ie_refine(nele_refine,8)
!
      integer(kind = kint) :: inum, jnum, icou, k1, l1
!
      do inum = 1, nele_sub
        do k1 = 1, 8
          jnum = k1 + (inum-1)*8
          icou = ist + inum
          l1 = ie_new_sm_tri_e(jnum)
          ie_refine(icou,k1) = inod_refine_local(l1)
        end do
      end do
!
      end subroutine set_refined_connect
!
! ----------------------------------------------------------------------
!
      end module set_each_refined_connect
