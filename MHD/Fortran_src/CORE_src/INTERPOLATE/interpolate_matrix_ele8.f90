!interpolate_matrix_ele8.f90
!     module interpolate_matrix_ele8
!
!     Written by H. Matsui on Apr., 2012
!
!!      subroutine set_interpolate_mat_ele8(np_smp, numele, ie,         &
!!     &          iele_gauss, xi_gauss, NC, NCM, INM, IAM, AM,          &
!!     &          IEND_SUM_smp)
!!      subroutine set_interpolate_mat_surf4(np_smp, numele, ie,        &
!!     &          iele_gauss, isurf_gauss, xi_gauss,                    &
!!     &          NC, NCM, INM, IAM, AM, IEND_SUM_smp)
!!      subroutine set_interpolate_mat_edge2(np_smp, numele, ie,        &
!!     &          iele_gauss, iedge_gauss, xi_gauss,                    &
!!     &          NC, NCM, INM, IAM, AM, IEND_SUM_smp)
!!      subroutine set_interpolate_mat_node(np_smp, numele,             &
!!     &          nnod_4_ele, ie, iele_gauss, inod_gauss,               &
!!     &          NC, NCM, INM, IAM, AM, IEND_SUM_smp)
!
      module interpolate_matrix_ele8
!
      use m_precision
      use m_constants
      use m_geometry_constants
!
      implicit none
!
! ----------------------------------------------------------------------
!
      contains
!
! ----------------------------------------------------------------------
!
      subroutine set_interpolate_mat_ele8(np_smp, numele, ie,           &
     &          iele_gauss, xi_gauss, NC, NCM, INM, IAM, AM,            &
     &          IEND_SUM_smp)
!
      integer (kind = kint), intent(in) :: np_smp
      integer (kind = kint), intent(in) :: numele
      integer (kind = kint), intent(in) :: ie(numele,8)
      integer (kind = kint), intent(in) :: iele_gauss(NC)
      real (kind=kreal), intent(in) :: xi_gauss(NC,3)
      integer (kind = kint), intent(in) :: NC, NCM
!
      integer(kind=kint), intent(in) :: INM(0:NC)
      integer(kind = kint), intent(in) :: IEND_SUM_smp(0:np_smp)
!
      integer(kind=kint), intent(inout) :: IAM(NCM)
      real(kind = kreal), intent(inout) :: AM(NCM)
!
      real (kind=kreal) :: xi, ei, zi
      real (kind=kreal) :: xi_nega, ei_nega, zi_nega
      real (kind=kreal) :: xi_posi, ei_posi, zi_posi
      real (kind=kreal) :: xi_sqre, ei_sqre, zi_sqre
!
      integer (kind = kint) :: ip, ist_s, ied_s
      integer (kind = kint) :: iele, ist
!
      integer (kind = kint) :: ig
!
!
!$omp parallel do                                                       &
!$omp& private(ist_s,ied_s,ig,iele,ist,xi,ei,zi,xi_nega,xi_posi,ei_nega,&
!$omp&         ei_posi,zi_nega,zi_posi, xi_sqre, ei_sqre, zi_sqre)
      do ip = 1, np_smp
        ist_s = IEND_SUM_smp(ip-1) + 1
        ied_s = IEND_SUM_smp(ip  )
        do ig = ist_s, ied_s
          ist = INM(ig-1)
          iele = iele_gauss(ig)
!
          IAM(ist+ 1) = ie(iele,1)
          IAM(ist+ 2) = ie(iele,2)
          IAM(ist+ 3) = ie(iele,3)
          IAM(ist+ 4) = ie(iele,4)
          IAM(ist+ 5) = ie(iele,5)
          IAM(ist+ 6) = ie(iele,6)
          IAM(ist+ 7) = ie(iele,7)
          IAM(ist+ 8) = ie(iele,8)
!
          xi = xi_gauss(ig,1)
          ei = xi_gauss(ig,2)
          zi = xi_gauss(ig,3)
!
          xi_nega = one - xi
          xi_posi = one + xi
!
          ei_nega = one - ei
          ei_posi = one + ei
!
          zi_nega = one - zi
          zi_posi = one + zi
!
          AM(ist+1) = r125 * xi_nega * ei_nega * zi_nega
          AM(ist+2) = r125 * xi_posi * ei_nega * zi_nega
          AM(ist+3) = r125 * xi_posi * ei_posi * zi_nega
          AM(ist+4) = r125 * xi_nega * ei_posi * zi_nega
          AM(ist+5) = r125 * xi_nega * ei_nega * zi_posi
          AM(ist+6) = r125 * xi_posi * ei_nega * zi_posi
          AM(ist+7) = r125 * xi_posi * ei_posi * zi_posi
          AM(ist+8) = r125 * xi_nega * ei_posi * zi_posi
        end do
      end do
!$omp end parallel do
!
      end subroutine set_interpolate_mat_ele8
!
! ----------------------------------------------------------------------
!
      subroutine set_interpolate_mat_surf4(np_smp, numele, ie,          &
     &          iele_gauss, isurf_gauss, xi_gauss,                      &
     &          NC, NCM, INM, IAM, AM, IEND_SUM_smp)
!
      integer (kind = kint), intent(in) :: np_smp, numele
      integer (kind = kint), intent(in) :: ie(numele,8)
      integer (kind = kint), intent(in) :: iele_gauss(NC)
      integer (kind = kint), intent(in) :: isurf_gauss(NC)
      real (kind=kreal), intent(in) :: xi_gauss(NC,3)
      integer (kind = kint), intent(in) :: NC, NCM
!
      integer(kind=kint), intent(in) :: INM(0:NC)
      integer(kind = kint), intent(in) :: IEND_SUM_smp(0:np_smp)
!
      integer(kind=kint), intent(inout) :: IAM(NCM)
      real(kind = kreal), intent(inout) :: AM(NCM)
!
      real (kind=kreal) :: xi, ei
      real (kind=kreal) :: xi_nega, ei_nega
      real (kind=kreal) :: xi_posi, ei_posi
!
      integer (kind = kint) :: ip, ist_s, ied_s, ist, iele
      integer (kind = kint) :: isf, k1, k2, k3, k4
      integer (kind = kint) :: ig, ld1, ld2
!
!
!$omp parallel do                                                       &
!$omp& private(ist_s,ied_s,ist,ig,iele,isf,ld1,ld2,                     &
!$omp&         k1, k2, k3, k4,xi,ei,xi_nega,xi_posi,ei_nega,ei_posi)
      do ip = 1, np_smp
        ist_s = IEND_SUM_smp(ip-1) + 1
        ied_s = IEND_SUM_smp(ip  )
        do ig = ist_s, ied_s
          ist = INM(ig-1)
!
          iele = iele_gauss(ig)
          isf =  mod(isurf_gauss(ig),100)
!
          k1 = node_on_sf_4(1,isf)
          k2 = node_on_sf_4(2,isf)
          k3 = node_on_sf_4(3,isf)
          k4 = node_on_sf_4(4,isf)
!
          ld1 = ishape_dir_surf(1,isf)
          ld2 = ishape_dir_surf(2,isf)
!
          IAM(ist+1) = ie(iele,k1)
          IAM(ist+2) = ie(iele,k2)
          IAM(ist+3) = ie(iele,k3)
          IAM(ist+4) = ie(iele,k4)
!
          xi = xi_gauss(ig,ld1)
          ei = xi_gauss(ig,ld2)
!
          xi_nega = one - xi
          xi_posi = one + xi
!
          ei_nega = one - ei
          ei_posi = one + ei
!
          AM(ist+1) = quad * xi_nega * ei_nega
          AM(ist+2) = quad * xi_posi * ei_nega
          AM(ist+3) = quad * xi_posi * ei_posi
          AM(ist+4) = quad * xi_nega * ei_posi
        end do
      end do
!$omp end parallel do
!
      end subroutine set_interpolate_mat_surf4
!
! ----------------------------------------------------------------------
!
      subroutine set_interpolate_mat_edge2(np_smp, numele, ie,          &
     &          iele_gauss, iedge_gauss, xi_gauss,                      &
     &          NC, NCM, INM, IAM, AM, IEND_SUM_smp)
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
      real (kind=kreal) :: xi, xi_nega, xi_posi
!
      integer (kind = kint) :: ip, ist_s, ied_s
      integer (kind = kint) :: iele, ist
      integer (kind = kint) :: iedge, k1, k2
      integer (kind = kint) :: ig, ld1
!
!
!$omp parallel do                                                       &
!$omp& private(ist_s,ied_s,ist,ig,iele,iedge,ld1,k1, k2,                &
!$omp&         xi,xi_nega,xi_posi)
      do ip = 1, np_smp
        ist_s = IEND_SUM_smp(ip-1) + 1
        ied_s = IEND_SUM_smp(ip)
        do ig = ist_s, ied_s
          ist = INM(ig-1)
!
          iele =  iele_gauss(ig)
          iedge = mod(iedge_gauss(ig),100)
!
          k1 = node_on_edge_l(1,iedge)
          k2 = node_on_edge_l(2,iedge)
!
          ld1 = ishape_dir_edge(iedge)
!
          IAM(ist+1) = ie(iele,k1)
          IAM(ist+2) = ie(iele,k2)
!
          xi = xi_gauss(ig,ld1)
!
          xi_nega = one - xi
          xi_posi = one + xi
!
          AM(ist+1) = half * xi_nega
          AM(ist+2) = half * xi_posi
        end do
      end do
!$omp end parallel do
!
      end subroutine set_interpolate_mat_edge2
!
! ----------------------------------------------------------------------
!
      subroutine set_interpolate_mat_node(np_smp, numele,               &
     &          nnod_4_ele, ie, iele_gauss, inod_gauss,                 &
     &          NC, NCM, INM, IAM, AM, IEND_SUM_smp)
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
      end module interpolate_matrix_ele8
