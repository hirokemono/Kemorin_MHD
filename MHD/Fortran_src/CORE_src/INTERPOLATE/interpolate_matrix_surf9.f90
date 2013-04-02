!interpolate_matrix_surf9.f90
!     module interpolate_matrix_surf9
!
!     Written by H. Matsui on Sep., 2006
!
!      subroutine count_interpolate_mat_surf9(np_smp, istack_smp,       &
!     &          NC, INOD_DJO, INM, NUM_SUM, IEND_SUM, IEND_SUM_smp)
!      subroutine set_interpolate_mat_surf9(np_smp, numele, ie,         &
!     &          iele_gauss, isurf_gauss, xi_gauss,                     &
!     &          NC, NCM, INM, IAM, AM, IEND_SUM_smp)
!
      module interpolate_matrix_surf9
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
      subroutine count_interpolate_mat_surf9(np_smp, istack_smp,        &
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
      NUM_SUM = 9
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
      end subroutine count_interpolate_mat_surf9
!
! ----------------------------------------------------------------------
!
      subroutine set_interpolate_mat_surf9(np_smp, numele, ie,          &
     &          iele_gauss, isurf_gauss, xi_gauss,                      &
     &          NC, NCM, INM, IAM, AM, IEND_SUM_smp)
!
      use m_constants
      use m_geometry_constants
!
      integer (kind = kint), intent(in) :: np_smp, numele
      integer (kind = kint), intent(in) :: ie(numele,27)
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
      real (kind=kreal) :: xi_sqre, ei_sqre
!
      integer (kind = kint) :: ip, ist_s, ied_s, ist, iele
      integer (kind = kint) :: isf, k1, k2, k3, k4, k5, k6, k7, k8, k9
      integer (kind = kint) :: ig, ld1, ld2
!
!
!$omp parallel do                                                       &
!$omp& private(ist_s,ied_s,ist,ig,iele,isf,ld1,ld2,                     &
!$omp&         k1, k2, k3, k4, k5, k6, k7, k8, k9,                      &
!$omp&         xi,ei,xi_nega,xi_posi,xi_sqre,ei_nega,ei_posi,ei_sqre)
      do ip = 1, np_smp
        ist_s = IEND_SUM_smp(ip-1) + 1
        ied_s = IEND_SUM_smp(ip  )
        do ig = ist_s, ied_s
          ist = INM(ig)
!
          iele = iele_gauss(ig)
          isf =  mod(isurf_gauss(ig),100)
!
          k1 = node_on_sf_9(1,isf)
          k2 = node_on_sf_9(2,isf)
          k3 = node_on_sf_9(3,isf)
          k4 = node_on_sf_9(4,isf)
          k5 = node_on_sf_9(5,isf)
          k6 = node_on_sf_9(6,isf)
          k7 = node_on_sf_9(7,isf)
          k8 = node_on_sf_9(8,isf)
          k9 = node_on_sf_9(9,isf)
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
          IAM(ist+9) = ie(iele,k9)
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
          AM(ist+1) = quad * xi_nega * ei_nega * xi * ei
          AM(ist+2) = quad * xi_posi * ei_nega * xi * ei
          AM(ist+3) = quad * xi_posi * ei_posi * xi * ei
          AM(ist+4) = quad * xi_nega * ei_posi * xi * ei
!
          AM(ist+5) =  half * xi_sqre * ei_nega * ei
          AM(ist+6) =  half * xi_posi * ei_sqre * xi
          AM(ist+7) =  half * xi_sqre * ei_posi * ei
          AM(ist+8) =  half * xi_nega * ei_sqre * xi
!
          AM(ist+9) =       xi_sqre * ei_sqre
        end do
      end do
!$omp end parallel do
!
      end subroutine set_interpolate_mat_surf9
!
! ----------------------------------------------------------------------
!
      end module interpolate_matrix_surf9
