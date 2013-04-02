!interpolate_matrix_surf4.f90
!     module interpolate_matrix_surf4
!
!     Written by H. Matsui on Sep., 2006
!
!      subroutine count_interpolate_mat_surf4(np_smp, istack_smp,       &
!     &          NC, INOD_DJO, INM, NUM_SUM, IEND_SUM, IEND_SUM_smp)
!      subroutine set_interpolate_mat_surf4(np_smp, numele, ie,         &
!     &          iele_gauss, isurf_gauss, xi_gauss,                     &
!     &          NC, NCM, INM, IAM, AM, IEND_SUM_smp)
!
      module interpolate_matrix_surf4
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
      subroutine count_interpolate_mat_surf4(np_smp, istack_smp,        &
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
      NUM_SUM = 4
      do ip = 1, np_smp
        ist_s = IEND_SUM_smp(ip-1) + 1
        ied_s = IEND_SUM_smp(ip)
        IEND_SUM_smp(ip) = istack_smp(ip)
        do ig = ist_s, ied_s
          INOD_DJO(ig-1) = ig
          INM(ig) = INM(ig) + NUM_SUM
        end do
      end do
      IEND_SUM = IEND_SUM_smp(np_smp)
!
      end subroutine count_interpolate_mat_surf4
!
! ----------------------------------------------------------------------
!
      subroutine set_interpolate_mat_surf4(np_smp, numele, ie,          &
     &          iele_gauss, isurf_gauss, xi_gauss,                      &
     &          NC, NCM, INM, IAM, AM, IEND_SUM_smp)
!
      use m_constants
      use m_geometry_constants
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
          ist = INM(ig)
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
      end module interpolate_matrix_surf4
