!interpolate_matrix_ele20.f90
!     module interpolate_matrix_ele20
!
!     Written by H. Matsui on Apr., 2012
!
!!      subroutine set_interpolate_mat_ele20(np_smp, numele, ie,        &
!!     &          iele_gauss, xi_gauss, NC, NCM, INM, IAM, AM,          &
!!     &          IEND_SUM_smp)
!!      subroutine set_interpolate_mat_surf8(np_smp, numele, ie,        &
!!     &          iele_gauss, isurf_gauss, xi_gauss,                    &
!!     &          NC, NCM, INM, IAM, AM, IEND_SUM_smp)
!!      subroutine set_interpolate_mat_edge3(np_smp, numele, ie,        &
!!     &          iele_gauss, iedge_gauss, xi_gauss,                    &
!!     &          NC, NCM, INM, IAM, AM, IEND_SUM_smp)
!
      module interpolate_matrix_ele20
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
      subroutine set_interpolate_mat_ele20(np_smp, numele, ie,          &
     &          iele_gauss, xi_gauss, NC, NCM, INM, IAM, AM,            &
     &          IEND_SUM_smp)
!
      integer (kind = kint), intent(in) :: np_smp
      integer (kind = kint), intent(in) :: numele
      integer (kind = kint), intent(in) :: ie(numele,20)
      integer (kind = kint), intent(in) :: NC, NCM
      integer (kind = kint), intent(in) :: iele_gauss(NC)
      real (kind=kreal), intent(in) :: xi_gauss(NC,3)
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
          IAM(ist+ 9) = ie(iele,9)
          IAM(ist+10) = ie(iele,10)
          IAM(ist+11) = ie(iele,11)
          IAM(ist+12) = ie(iele,12)
          IAM(ist+13) = ie(iele,13)
          IAM(ist+14) = ie(iele,14)
          IAM(ist+15) = ie(iele,15)
          IAM(ist+16) = ie(iele,16)
          IAM(ist+17) = ie(iele,17)
          IAM(ist+18) = ie(iele,18)
          IAM(ist+19) = ie(iele,19)
          IAM(ist+20) = ie(iele,20)
!
          xi = xi_gauss(ig,1)
          ei = xi_gauss(ig,2)
          zi = xi_gauss(ig,3)
!
          xi_nega = one - xi
          xi_posi = one + xi
          xi_sqre = one - xi * xi
!
          ei_nega = one - ei
          ei_posi = one + ei
          ei_sqre = one - ei * ei
!
          zi_nega = one - zi
          zi_posi = one + zi
          zi_sqre = one - zi * zi
!
          AM(ist+ 1)  = r125 * xi_nega * ei_nega * zi_nega              &
     &                 * (-xi-ei-zi-two)
          AM(ist+ 2)  = r125 * xi_posi * ei_nega * zi_nega              &
     &                 * ( xi-ei-zi-two)
          AM(ist+ 3)  = r125 * xi_posi * ei_posi * zi_nega              &
     &                 * ( xi+ei-zi-two)
          AM(ist+ 4)  = r125 * xi_nega * ei_posi * zi_nega              &
     &                 * (-xi+ei-zi-two)
          AM(ist+ 5)  = r125 * xi_nega * ei_nega * zi_posi              &
     &                 * (-xi-ei+zi-two)
          AM(ist+ 6)  = r125 * xi_posi * ei_nega * zi_posi              &
     &                 * ( xi-ei+zi-two)
          AM(ist+ 7)  = r125 * xi_posi * ei_posi * zi_posi              &
     &                 * ( xi+ei+zi-two)
          AM(ist+ 8)  = r125 * xi_nega * ei_posi * zi_posi              &
     &                 * (-xi+ei+zi-two)
!
          AM(ist+ 9) =  quad * xi_sqre * ei_nega * zi_nega
          AM(ist+10) =  quad * xi_posi * ei_sqre * zi_nega
          AM(ist+11) =  quad * xi_sqre * ei_posi * zi_nega
          AM(ist+12) =  quad * xi_nega * ei_sqre * zi_nega
!
          AM(ist+13) =  quad * xi_sqre * ei_nega * zi_posi
          AM(ist+14) =  quad * xi_posi * ei_sqre * zi_posi
          AM(ist+15) =  quad * xi_sqre * ei_posi * zi_posi
          AM(ist+16) =  quad * xi_nega * ei_sqre * zi_posi
!
          AM(ist+17) =  quad * xi_nega * ei_nega * zi_sqre
          AM(ist+18) =  quad * xi_posi * ei_nega * zi_sqre
          AM(ist+19) =  quad * xi_posi * ei_posi * zi_sqre
          AM(ist+20) =  quad * xi_nega * ei_posi * zi_sqre
        end do
      end do
!$omp end parallel do
!
      end subroutine set_interpolate_mat_ele20
!
! ----------------------------------------------------------------------
!
      subroutine set_interpolate_mat_surf8(np_smp, numele, ie,          &
     &          iele_gauss, isurf_gauss, xi_gauss,                      &
     &          NC, NCM, INM, IAM, AM, IEND_SUM_smp)
!
      integer (kind = kint), intent(in) :: np_smp, numele
      integer (kind = kint), intent(in) :: ie(numele,20)
      integer (kind = kint), intent(in) :: NC, NCM
      integer (kind = kint), intent(in) :: iele_gauss(NC)
      integer (kind = kint), intent(in) :: isurf_gauss(NC)
      real (kind=kreal), intent(in) :: xi_gauss(NC,3)
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
      real (kind=kreal) :: xi_sqre, ei_sqre
!
      integer (kind = kint) :: ip, ist_s, ied_s, ist, iele
      integer (kind = kint) :: isf, k1, k2, k3, k4, k5, k6, k7, k8
      integer (kind = kint) :: ig, ld1, ld2
!
!
!$omp parallel do                                                       &
!$omp& private(ist_s,ied_s,ist,ig,iele,isf,ld1,ld2,                     &
!$omp&         k1, k2, k3, k4, k5, k6, k7, k8,                          &
!$omp&         xi,ei,xi_nega,xi_posi,xi_sqre,ei_nega,ei_posi,ei_sqre)
      do ip = 1, np_smp
        ist_s = IEND_SUM_smp(ip-1) + 1
        ied_s = IEND_SUM_smp(ip  )
        do ig = ist_s, ied_s
          ist = INM(ig-1)
!
          iele = iele_gauss(ig)
          isf =  mod(isurf_gauss(ig),icent)
!
          k1 = node_on_sf_8(1,isf)
          k2 = node_on_sf_8(2,isf)
          k3 = node_on_sf_8(3,isf)
          k4 = node_on_sf_8(4,isf)
          k5 = node_on_sf_8(5,isf)
          k6 = node_on_sf_8(6,isf)
          k7 = node_on_sf_8(7,isf)
          k8 = node_on_sf_8(8,isf)
!
          ld1 = ishape_dir_surf(1,isf)
          ld2 = ishape_dir_surf(2,isf)
!
          IAM(ist+1) = ie(iele,k1)
          IAM(ist+2) = ie(iele,k2)
          IAM(ist+3) = ie(iele,k3)
          IAM(ist+4) = ie(iele,k4)
          IAM(ist+5) = ie(iele,k5)
          IAM(ist+6) = ie(iele,k6)
          IAM(ist+7) = ie(iele,k7)
          IAM(ist+8) = ie(iele,k8)
!
          xi = xi_gauss(ig,ld1)
          ei = xi_gauss(ig,ld2)
!
          xi_nega = one - xi
          xi_posi = one + xi
          xi_sqre = one - xi*xi
!
          ei_nega = one - ei
          ei_posi = one + ei
          ei_sqre = one - ei*ei
!
          AM(ist+1) = quad * xi_nega * ei_nega * (-xi-ei-one)
          AM(ist+2) = quad * xi_posi * ei_nega * ( xi-ei-one)
          AM(ist+3) = quad * xi_posi * ei_posi * ( xi+ei-one)
          AM(ist+4) = quad * xi_nega * ei_posi * (-xi+ei-one)
!
          AM(ist+5) =  half * xi_sqre * ei_nega
          AM(ist+6) =  half * xi_posi * ei_sqre
          AM(ist+7) =  half * xi_sqre * ei_posi
          AM(ist+8) =  half * xi_nega * ei_sqre
        end do
      end do
!$omp end parallel do
!
      end subroutine set_interpolate_mat_surf8
!
! ----------------------------------------------------------------------
!
      subroutine set_interpolate_mat_edge3(np_smp, numele, ie,          &
     &          iele_gauss, iedge_gauss, xi_gauss,                      &
     &          NC, NCM, INM, IAM, AM, IEND_SUM_smp)
!
      integer (kind = kint), intent(in) :: np_smp, numele
      integer (kind = kint), intent(in) :: ie(numele,20)
      integer (kind = kint), intent(in) :: NC, NCM
      integer (kind = kint), intent(in) :: iele_gauss(NC)
      integer (kind = kint), intent(in) :: iedge_gauss(NC)
      real (kind=kreal), intent(in) :: xi_gauss(NC,3)
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
          ist = INM(ig-1)
!
          iele =  iele_gauss(ig)
          iedge = mod(iedge_gauss(ig),icent)
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
      end module interpolate_matrix_ele20
