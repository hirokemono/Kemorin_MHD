!cal_edat_on_coarse_mesh.f90
!     module cal_edat_on_coarse_mesh
!
!     Written by H. Matsui on Jan., 2009
!
!      subroutine cal_coef_4_edat_coarse_mesh(nele, num_points,         &
!     &          iele_gauss, int_inter_org, coef_ave)
!      subroutine cal_edat_on_coarse_mesh_1(nele, num_points,           &
!     &          iele_gauss, coef_inter_org, coef_ave, coef_new)
!      subroutine cal_edat_on_coarse_mesh_N(nele, NB, num_points,       &
!     &          iele_gauss, coef_inter_org, coef_ave, coef_new)
!
      module cal_edat_on_coarse_mesh
!
      use m_precision
!
      implicit none
!
!  ---------------------------------------------------------------------
!
      contains
!
!  ---------------------------------------------------------------------
!
      subroutine cal_coef_4_edat_coarse_mesh(nele, num_points,          &
     &          iele_gauss, int_inter_org, coef_ave)
!
      integer (kind = kint), intent(in) :: nele
      integer (kind = kint), intent(in) :: num_points
      integer (kind = kint), intent(in) :: iele_gauss(num_points)
      integer (kind = kint), intent(in) :: int_inter_org(num_points)
!
      real (kind = kreal), intent(inout) :: coef_ave(nele)
!
      integer(kind = kint) :: ig, iele
!
!
      coef_ave(1:nele) = 0.0d0
      do ig = 1, num_points
        iele = iele_gauss(ig)
        coef_ave(iele) = coef_ave(iele) + dble( int_inter_org(ig) )
      end do
!
!$omp parallel do
      do iele = 1, nele
        if( coef_ave(iele) .eq. 0) then
          coef_ave(iele) = 1.0d0
        else
          coef_ave(iele) = 1.0d0 / coef_ave(iele)
        end if
      end do
!$omp end parallel do
!
      end subroutine cal_coef_4_edat_coarse_mesh
!
!  ---------------------------------------------------------------------
!
      subroutine cal_edat_on_coarse_mesh_1(nele, num_points,            &
     &          iele_gauss, coef_inter_org, coef_ave, coef_new)
!
      integer (kind = kint), intent(in) :: nele
      integer (kind = kint), intent(in) :: num_points
      integer (kind = kint), intent(in) :: iele_gauss(num_points)
      real (kind = kreal), intent(in) :: coef_inter_org(num_points)
      real (kind = kreal), intent(in) :: coef_ave(nele)
!
      real (kind = kreal), intent(inout) :: coef_new(nele)
!
      integer(kind = kint) :: ig, iele
!
!
      coef_new(1:nele) = 0.0d0
      do ig = 1, num_points
        iele = iele_gauss(ig)
        coef_new(iele) = coef_new(iele) + coef_inter_org(ig)
      end do
!
!$omp parallel do
      do iele = 1, nele
        coef_new(iele) = coef_new(iele) * coef_ave(iele)
      end do
!$omp end parallel do
!
      end subroutine cal_edat_on_coarse_mesh_1
!
!  ---------------------------------------------------------------------
!
      subroutine cal_edat_on_coarse_mesh_N(nele, NB, num_points,        &
     &          iele_gauss, coef_inter_org, coef_ave, coef_new)
!
      integer (kind = kint), intent(in) :: nele, NB
      integer (kind = kint), intent(in) :: num_points
      integer (kind = kint), intent(in) :: iele_gauss(num_points)
      real (kind = kreal), intent(in) :: coef_inter_org(NB*num_points)
      real (kind = kreal), intent(in) :: coef_ave(nele)
!
      real (kind = kreal), intent(inout) :: coef_new(nele,NB)
!
      integer(kind = kint) :: ig, igg, nd, iele
!
!
      coef_new(1:nele,1:NB) = 0.0d0
!$omp parallel do private(nd,ig,iele)
      do nd = 1, NB
        do ig = 1, num_points
          iele = iele_gauss(ig)
          igg = nd + (ig-1)*NB
          coef_new(iele,nd) = coef_new(iele,nd) + coef_inter_org(igg)
        end do
      end do
!$omp end parallel do
!
!$omp parallel private(nd,iele)
      do nd = 1, NB
!$omp do
        do iele = 1, nele
          coef_new(iele,nd) = coef_new(iele,nd) * coef_ave(iele)
        end do
!$omp end do nowait
      end do
!$omp end parallel
!
      end subroutine cal_edat_on_coarse_mesh_N
!
!  ---------------------------------------------------------------------
!
      end module cal_edat_on_coarse_mesh
