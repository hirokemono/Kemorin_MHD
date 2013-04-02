!set_position_pvr_screen.f90
!      module set_position_pvr_screen
!
      module set_position_pvr_screen
!
!        programmed by H.Matsui on Aug., 2011
!
      use m_precision
!
      use m_constants
      use m_machine_parameter
!
      implicit  none
!
!      subroutine copy_node_position_for_pvr(numnod, numele,            &
!     &          inod_smp_stack, xx)
!      subroutine copy_node_position_pvr_domain(i_pvr)
!
!      subroutine cal_position_pvr_screen(model_mat, project_mat)
!      subroutine position_pvr_domain_on_screen(model_mat, project_mat)
!
! -----------------------------------------------------------------------
!
      contains
!
! -----------------------------------------------------------------------
!
      subroutine copy_node_position_for_pvr(numnod, numele,             &
     &          inod_smp_stack, xx)
!
      use m_geometries_in_pvr_screen
!
      integer(kind = kint), intent(in) :: numnod, numele
      integer(kind = kint), intent(in) :: inod_smp_stack(0:np_smp)
      real(kind = kreal), intent(in) :: xx(numnod,3)
!
      integer(kind = kint) :: inod
!
!
      nnod_pvr = numnod
      nele_pvr = numele
      call allocate_node_position_pvr
      istack_nod_pvr(0:np_smp) = inod_smp_stack(0:np_smp)
!
!$omp parallel do
      do inod = 1, numnod
        x_nod_sim(inod,1) = xx(inod,1)
        x_nod_sim(inod,2) = xx(inod,2)
        x_nod_sim(inod,3) = xx(inod,3)
        x_nod_sim(inod,4) = one
      end do
!$omp end parallel do
!
      end subroutine copy_node_position_for_pvr
!
! -----------------------------------------------------------------------
!
      subroutine copy_node_position_pvr_domain(numnod, numele,          &
     &          numsurf, nnod_4_surf, xx, ie_surf, isf_4_ele)
!
      use m_geometry_constants
      use m_surf_grp_4_pvr_domain
!
      integer(kind = kint), intent(in) :: numnod
      real(kind = kreal), intent(in) :: xx(numnod,3)
      integer(kind = kint), intent(in) :: numele, numsurf, nnod_4_surf
      integer(kind = kint), intent(in) :: ie_surf(numsurf,nnod_4_surf)
      integer(kind = kint), intent(in) :: isf_4_ele(numele,nsurf_4_ele)
!
      integer(kind = kint) :: inum, iele, k1, isurf
      integer(kind = kint) :: i1, i2, i3, i4
!
!
!$omp parallel do private (inum,iele,k1,isurf,i1,i2,i3,i4)
      do inum = 1, ntot_pvr_surf_domain
        iele = item_pvr_surf_domain(1,inum)
        k1 =   item_pvr_surf_domain(2,inum)
        isurf = abs(isf_4_ele(iele,k1))
!
        i1 = ie_surf(isurf,1)
        i2 = ie_surf(isurf,2)
        i3 = ie_surf(isurf,3)
        i4 = ie_surf(isurf,4)
!
        xx_nod_pvr_domain(4*inum-3,1:3) = xx(i1,1:3)
        xx_nod_pvr_domain(4*inum-2,1:3) = xx(i2,1:3)
        xx_nod_pvr_domain(4*inum-1,1:3) = xx(i3,1:3)
        xx_nod_pvr_domain(4*inum,  1:3) = xx(i4,1:3)
        xx_nod_pvr_domain(4*inum-3:4*inum,4) = one
      end do
!$omp end parallel do
!
      end subroutine copy_node_position_pvr_domain
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine cal_position_pvr_screen(model_mat, project_mat)
!
      use m_geometries_in_pvr_screen
      use cal_matrix_vector_smp
!
      real(kind = kreal), intent(in) :: model_mat(4,4)
      real(kind = kreal), intent(in) :: project_mat(4,4)
!
      integer(kind = kint) :: inod, ip
      real(kind = kreal) :: coef
!
!
!$omp parallel
      call cal_matvec_44_on_node(np_smp, nnod_pvr, istack_nod_pvr,      &
     &    model_mat, x_nod_sim, x_nod_model)
!$omp end parallel
!$omp parallel
      call cal_matvec_44_on_node(np_smp, nnod_pvr, istack_nod_pvr,      &
     &    project_mat, x_nod_model, x_nod_screen)
!$omp end parallel
!
!$omp parallel do private(coef,ip)
      do ip = 1, np_smp
        do inod = istack_nod_pvr(ip-1)+1, istack_nod_pvr(ip)
          coef = one / x_nod_screen(inod,4)
          x_nod_screen(inod,1) = x_nod_screen(inod,1) * coef
          x_nod_screen(inod,2) = x_nod_screen(inod,2) * coef
          x_nod_screen(inod,3) = x_nod_screen(inod,3) * coef
        end do
      end do
!$omp end parallel do
!
      end subroutine cal_position_pvr_screen
!
! -----------------------------------------------------------------------
!
      subroutine position_pvr_domain_on_screen(model_mat, project_mat)
!
      use m_surf_grp_4_pvr_domain
      use cal_matrix_vector_smp
!
      real(kind = kreal), intent(in) :: model_mat(4,4)
      real(kind = kreal), intent(in) :: project_mat(4,4)
!
      integer(kind = kint) :: inod, ip, k1
      integer(kind = kint) :: istack(0:4), ntot
      real(kind = kreal) :: coef
!
      istack(0) = 0
      do k1 = 1, 4
        istack(k1) = istack(k1-1) + ntot_pvr_surf_domain
      end do
      ntot = 4*ntot_pvr_surf_domain
!
!$omp parallel
      call cal_matvec_44_on_node(ifour, ntot, istack, model_mat,        &
     &    xx_nod_pvr_domain(1,1), xx_model_pvr_domain(1,1))
!$omp end parallel
!$omp parallel
      call cal_matvec_44_on_node(ifour, ntot, istack, project_mat,      &
     &    xx_model_pvr_domain(1,1), xx_screen_pvr_domain(1,1))
!$omp end parallel
!
!$omp parallel do private(coef,ip,inod)
      do ip = 1, ifour
        do inod = 1, ntot_pvr_surf_domain
          coef = one / xx_screen_pvr_domain(4*inod+ip-4,4)
          xx_screen_pvr_domain(4*inod+ip-4,1)                           &
     &               = xx_screen_pvr_domain(4*inod+ip-4,1) * coef
          xx_screen_pvr_domain(4*inod+ip-4,2)                           &
     &               = xx_screen_pvr_domain(4*inod+ip-4,2) * coef
          xx_screen_pvr_domain(4*inod+ip-4,3)                           &
     &               = xx_screen_pvr_domain(4*inod+ip-4,3) * coef
        end do
      end do
!$omp end parallel do
!
      end subroutine position_pvr_domain_on_screen
!
! -----------------------------------------------------------------------
!
      end module set_position_pvr_screen
