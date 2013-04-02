!interpolate_matrix_edge3.f90
!     module interpolate_matrix_edge3
!
!     Written by H. Matsui on Sep., 2006
!
!      subroutine count_interpolate_mat_edge3(np_smp, istack_smp,       &
!     &          NC, INOD_DJO, INM, NUM_SUM, IEND_SUM, IEND_SUM_smp)
!      subroutine set_interpolate_mat_edge3(np_smp, numele, ie,         &
!     &          iele_gauss, iedge_gauss, xi_gauss,                     &
!     &          NC, NCM, INM, IAM, AM, IEND_SUM_smp)
!
      module interpolate_matrix_edge3
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
      subroutine count_interpolate_mat_edge3(np_smp, istack_smp,        &
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
      integer (kind = kint) :: ip, ig, ist_s, ied_s
!
!
      NUM_SUM = 3
      do ip = 1, np_smp
        ist_s = IEND_SUM_smp(ip-1) + 1
        ied_s = IEND_SUM_smp(ip)
        IEND_SUM_smp(ip) = istack_smp(ip)
        do ig = ist_s, ied_s
          INOD_DJO(ig) = ig
          INM(ig) = INM(ig-1) + NUM_SUM
        end do
      end do
      IEND_SUM = IEND_SUM_smp(np_smp)
!
      end subroutine count_interpolate_mat_edge3
!
!  ---------------------------------------------------------------------
!
      subroutine set_interpolate_mat_edge3(np_smp, numele, ie,          &
     &          iele_gauss, iedge_gauss, xi_gauss,                      &
     &          NC, NCM, INM, IAM, AM, IEND_SUM_smp)
!
      use m_constants
      use m_geometry_constants
!
      integer (kind = kint), intent(in) :: np_smp, numele
      integer (kind = kint), intent(in) :: ie(numele,20)
      integer (kind = kint), intent(in) :: iele_gauss(NC)
      integer (kind = kint), intent(in) :: iedge_gauss(NC)
      real (kind=kreal), intent(in) :: xi_gauss(NC,3)
!
      integer (kind = kint), intent(in) :: NC, NCM
!
      integer(kind=kint), intent(in) :: INM(0:NC)
      integer(kind = kint), intent(in) :: IEND_SUM_smp(0:np_smp)
!
      integer(kind=kint), intent(inout) :: IAM(NCM)
      real(kind = kreal), intent(inout) :: AM(NCM)
!
      real (kind=kreal) :: xi, xi_nega, xi_posi, xi_sqre
!
      integer (kind = kint) :: ip, ist_s, ied_s
      integer (kind = kint) :: iele, ist
      integer (kind = kint) :: iedge, k1, k2, k3
      integer (kind = kint) :: ig, ld1
!
!
!$omp parallel do                                                       &
!$omp& private(ist_s,ied_s,ist,ig,iele,iedge,ld1,k1, k2, k3,            &
!$omp&         xi,xi_nega,xi_posi,xi_sqre)
      do ip = 1, np_smp
        ist_s = IEND_SUM_smp(ip-1) + 1
        ied_s = IEND_SUM_smp(ip)
        do ig = ist_s, ied_s
          ist = INM(ig)
!
          iele =  iele_gauss(ig)
          iedge = mod(iedge_gauss(ig),100)
!
          k1 = node_on_edge_q(1,iedge)
          k2 = node_on_edge_q(2,iedge)
          k3 = node_on_edge_q(3,iedge)
!
          ld1 = ishape_dir_edge(iedge)
!
          IAM(ist+1) = ie(iele,k1)
          IAM(ist+2) = ie(iele,k2)
          IAM(ist+3) = ie(iele,k3)
!
          xi = xi_gauss(ig,ld1)
!
          xi_nega = one - xi
          xi_posi = one + xi
          xi_sqre = one - xi*xi
!
          AM(ist+1) = -half * xi * xi_nega
          AM(ist+2) =              xi_sqre
          AM(ist+3) =  half * xi * xi_posi
        end do
      end do
!$omp end parallel do
!
      end subroutine set_interpolate_mat_edge3
!
! ----------------------------------------------------------------------
!
      end module interpolate_matrix_edge3
