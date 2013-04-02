!interpolate_matrix_ele20.f90
!     module interpolate_matrix_ele20
!
!     Written by H. Matsui on Apr., 2012
!
!      subroutine count_interpolate_mat_ele20(np_smp, istack_smp,       &
!     &          NC, INOD_DJO, INM, NUM_SUM, IEND_SUM, IEND_SUM_smp)
!      subroutine set_interpolate_mat_ele20(np_smp, numnod, numele, ie, &
!     &          iele_gauss, xi_gauss, NC, NCM, INM, IAM, AM,           &
!     &          IEND_SUM_smp)
!
      module interpolate_matrix_ele20
!
      use m_precision
!
      implicit none
!
! ----------------------------------------------------------------------
!
      contains
!
! ----------------------------------------------------------------------
!
      subroutine count_interpolate_mat_ele20(np_smp, istack_smp,        &
     &          NC, INOD_DJO, INM, NUM_SUM, IEND_SUM, IEND_SUM_smp)
!
      integer (kind = kint), intent(in) :: np_smp
      integer (kind = kint), intent(in) :: istack_smp(0:np_smp)
!
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
      NUM_SUM = 20
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
      end subroutine count_interpolate_mat_ele20
!
! ----------------------------------------------------------------------
!
      subroutine set_interpolate_mat_ele20(np_smp, numele, ie,          &
     &          iele_gauss, xi_gauss, NC, NCM, INM, IAM, AM,            &
     &          IEND_SUM_smp)
!
      use m_constants
!
      integer (kind = kint), intent(in) :: np_smp
      integer (kind = kint), intent(in) :: numele
      integer (kind = kint), intent(in) :: ie(numele,20)
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
          ist = INM(ig)
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
      end module interpolate_matrix_ele20
