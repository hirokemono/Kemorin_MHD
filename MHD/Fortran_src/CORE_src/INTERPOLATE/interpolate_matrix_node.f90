!
!     module interpolate_matrix_node
!
!     Written by H. Matsui on Sep., 2006
!
!      subroutine count_interpolate_mat_node(np_smp, istack_smp,        &
!     &          NC, INOD_DJO, INM, NUM_SUM, IEND_SUM, IEND_SUM_smp)
!      subroutine set_interpolate_mat_node(np_smp, numele,              &
!     &          nnod_4_ele, ie, iele_gauss, inod_gauss,                &
!     &          NC, NCM, INM, IAM, AM, IEND_SUM_smp)
!
      module interpolate_matrix_node
!
      use m_precision
!
      implicit none
!
! ----------------------------------------------------------------------
!
      contains
!
!  ---------------------------------------------------------------------
!
      subroutine count_interpolate_mat_node(np_smp, istack_smp,         &
     &          NC, INOD_DJO, INM, NUM_SUM, IEND_SUM, IEND_SUM_smp)
!
      use m_constants
      use m_geometry_constants
!
      integer (kind = kint), intent(in) :: np_smp
      integer (kind = kint), intent(in) :: istack_smp(0:np_smp)
      integer (kind = kint), intent(in) :: NC
!
      integer(kind = kint), intent(inout) :: INOD_DJO(NC)
      integer(kind = kint), intent(inout) :: INM(NC)
      integer(kind = kint), intent(inout) :: NUM_SUM, IEND_SUM
      integer(kind = kint), intent(inout) :: IEND_SUM_smp(0:np_smp)
!
!
      integer (kind = kint) :: ip, ig, ist_s, ied_s
!
!
      NUM_SUM = 1
      do ip = 1, np_smp
        ist_s = IEND_SUM_smp(ip-1) + 1
        ied_s = IEND_SUM_smp(ip  )
        IEND_SUM_smp(ip) = istack_smp(ip)
        do ig = ist_s, ied_s
          INOD_DJO(ig) = ig
          INM(ig) = INM(ig-1) + NUM_SUM
        end do
      end do
      IEND_SUM = IEND_SUM_smp(np_smp)
!
      end subroutine count_interpolate_mat_node
!
! ----------------------------------------------------------------------
!
      subroutine set_interpolate_mat_node(np_smp, numele,               &
     &          nnod_4_ele, ie, iele_gauss, inod_gauss,                 &
     &          NC, NCM, INM, IAM, AM, IEND_SUM_smp)
!
      use m_constants
!
      integer (kind = kint), intent(in) :: np_smp
      integer (kind = kint), intent(in) :: numele, nnod_4_ele
      integer (kind = kint), intent(in) :: ie(numele,nnod_4_ele)
      integer (kind = kint), intent(in) :: iele_gauss(NC)
      integer (kind = kint), intent(in) :: inod_gauss(NC)
!
      integer (kind = kint), intent(in) :: NC, NCM
!
      integer(kind=kint), intent(in) :: INM(0:NC)
      integer(kind = kint), intent(in) :: IEND_SUM_smp(0:np_smp)
!
      integer(kind=kint), intent(inout) :: IAM(NCM)
      real(kind = kreal), intent(inout) :: AM(NCM)
!
      integer (kind = kint) :: ip, ist_s, ied_s, ist
      integer (kind = kint) :: iele, k1
      integer (kind = kint) :: ig
!
!
!$omp parallel do private(ist_s,ied_s,ist,ig,iele,k1)
      do ip = 1, np_smp
        ist_s = IEND_SUM_smp(ip-1) + 1
        ied_s = IEND_SUM_smp(ip  )
        do ig = ist_s, ied_s
          iele =  iele_gauss(ig)
          k1 = inod_gauss(ig)
!
          ist = INM(ig)
          IAM(ist+1) = ie(iele,k1)
          AM(ist+1) = one
        end do
      end do
!$omp end parallel do
!
      end subroutine set_interpolate_mat_node
!
!  ---------------------------------------------------------------------
!
      end module interpolate_matrix_node
