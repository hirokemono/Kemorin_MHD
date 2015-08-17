!int_rms_ave_ele_grps_type.f90
!      module int_rms_ave_ele_grps_type
!
!     Written by H. Matsui on Aug., 2007
!     Modified by H. Matsui on Nov., 2008
!     Modified by H. Matsui on June, 2011
!
!      subroutine int_vol_rms_ave_ele_grps_type(mesh, ele_grp, jac_3d,  &
!     &          num_int, d_nod, ave_l, rms_l)
!      subroutine int_vol_rms_ave_1egrp_type(mesh, jac_3d,              &
!     &          num_int, nitem_grp, iele_grp, d_nod, ave_l, rms_l)
!
!      subroutine int_vol_2rms_ave_ele_grps_type(mesh, ele_grp, jac_3d, &
!     &          num_int, d1_nod, d2_nod, ave_1, rms_1, ave_2, rms_2)
!      subroutine int_vol_2rms_ave_1egrp_type(mesh, jac_3d,             &
!     &          num_int, nitem_grp, iele_grp, d1_nod, d2_nod,          &
!     &          ave_1, rms_1, ave_2, rms_2)
!
!      subroutine int_vol_dev_cor_ele_grps_type(mesh, ele_grp, jac_3d,  &
!     &          num_int, d1_nod, d2_nod, ave_1, ave_2,                 &
!     &          sig_1, sig_2, cov_l)
!      subroutine int_vol_dev_cor_1egrp_type(mesh, jac_3d,              &
!     &          num_int,  nitem_grp, iele_grp, d1_nod, d2_nod,         &
!     &          ave_1, ave_2, sig_1, sig_2, cov_l)
!
      module int_rms_ave_ele_grps_type
!
      use m_precision
      use m_constants
      use m_geometry_constants
      use m_fem_gauss_int_coefs
!
      use t_mesh_data
      use t_phys_data
      use t_jacobians
!
      implicit none
!
!  ---------------------------------------------------------------------
!
      contains
!
!  ---------------------------------------------------------------------
!
      subroutine int_vol_rms_ave_ele_grps_type(mesh, ele_grp, jac_3d,   &
     &          num_int, d_nod, ave_l, rms_l)
!
      use int_vol_rms_ave_1egrp
!
      type(mesh_geometry), intent(in) :: mesh
      type(group_data), intent(in) :: ele_grp
      type(jacobians_3d), intent(in) :: jac_3d
!
      integer (kind = kint), intent(in) :: num_int
      real(kind = kreal), intent(in) :: d_nod(mesh%node%numnod)
!
      real(kind = kreal), intent(inout) :: ave_l(ele_grp%num_grp)
      real(kind = kreal), intent(inout) :: rms_l(ele_grp%num_grp)
!
      integer(kind = kint) :: igrp, ist_grp, nitem_grp
!
!
      if (mesh%ele%nnod_4_ele .eq. num_t_quad) then
!
!$omp parallel do private(igrp,ist_grp,nitem_grp)
        do igrp = 1, ele_grp%num_grp
          ist_grp =   ele_grp%istack_grp(igrp-1)
          nitem_grp = ele_grp%istack_grp(igrp)                          &
     &               - ele_grp%istack_grp(igrp-1)
          call int_vol_rms_ave_1egrp_q(mesh%node%numnod,                &
     &        mesh%ele%numele, mesh%ele%ie, mesh%ele%interior_ele,      &
     &        nitem_grp, ele_grp%item_grp(ist_grp+1:ist_grp+nitem_grp), &
     &        num_int, jac_3d%ntot_int, jac_3d%xjac, jac_3d%an,         &
     &        d_nod, ave_l(igrp), rms_l(igrp) )
        end do
!$omp end parallel do
!
      else
!
!$omp parallel do private(igrp,ist_grp,nitem_grp)
        do igrp = 1, ele_grp%num_grp
          ist_grp =   ele_grp%istack_grp(igrp-1)
          nitem_grp = ele_grp%istack_grp(igrp)                          &
     &               - ele_grp%istack_grp(igrp-1)
          call int_vol_rms_ave_1egrp_l(mesh%node%numnod,                &
     &        mesh%ele%numele, mesh%ele%ie, mesh%ele%interior_ele,      &
     &        nitem_grp, ele_grp%item_grp(ist_grp+1:ist_grp+nitem_grp), &
     &        num_int, jac_3d%ntot_int, jac_3d%xjac, jac_3d%an,         &
     &        d_nod, ave_l(igrp), rms_l(igrp) )
        end do
!$omp end parallel do
!
      end if
!
      end subroutine int_vol_rms_ave_ele_grps_type
!
!  ---------------------------------------------------------------------
!
      subroutine int_vol_rms_ave_1egrp_type(mesh, jac_3d,               &
     &          num_int, nitem_grp, iele_grp, d_nod, ave_l, rms_l)
!
      use int_vol_rms_ave_1egrp
!
      type(mesh_geometry), intent(in) :: mesh
      type(jacobians_3d), intent(in) :: jac_3d
!
      integer (kind = kint), intent(in) :: num_int
      integer (kind = kint), intent(in) :: nitem_grp
      integer (kind = kint), intent(in) :: iele_grp(nitem_grp)
      real(kind = kreal), intent(in) :: d_nod(mesh%node%numnod)
      real(kind = kreal), intent(inout) :: ave_l, rms_l
!
!
      if (mesh%ele%nnod_4_ele .eq. num_t_quad) then
        call int_vol_rms_ave_1egrp_q(mesh%node%numnod,                  &
     &      mesh%ele%numele, mesh%ele%ie, mesh%ele%interior_ele,        &
     &      nitem_grp, iele_grp, num_int, jac_3d%ntot_int,              &
     &      jac_3d%xjac, jac_3d%an, d_nod, ave_l, rms_l)
      else
        call int_vol_rms_ave_1egrp_(mesh%node%numnod,                   &
     &      mesh%ele%numele, mesh%ele%ie, mesh%ele%interior_ele,        &
     &      nitem_grp, iele_grp, num_int, jac_3d%ntot_int,              &
     &      jac_3d%xjac, jac_3d%an, d_nod, ave_l, rms_l)
      end if
!
      end subroutine int_vol_rms_ave_1egrp_type
!
!  ---------------------------------------------------------------------
!  ---------------------------------------------------------------------
!
      subroutine int_vol_2rms_ave_ele_grps_type(mesh, ele_grp, jac_3d,  &
     &          num_int, d1_nod, d2_nod, ave_1, rms_1, ave_2, rms_2)
! 
      use int_vol_2rms_ave_1egrp
!
      type(mesh_geometry), intent(in) :: mesh
      type(group_data), intent(in) :: ele_grp
      type(jacobians_3d), intent(in) :: jac_3d
!
      integer (kind = kint), intent(in) :: num_int
      real(kind = kreal), intent(in) :: d1_nod(mesh%node%numnod)
      real(kind = kreal), intent(in) :: d2_nod(mesh%node%numnod)
!
      real(kind = kreal), intent(inout) :: ave_1(ele_grp%num_grp)
      real(kind = kreal), intent(inout) :: rms_1(ele_grp%num_grp)
      real(kind = kreal), intent(inout) :: ave_2(ele_grp%num_grp)
      real(kind = kreal), intent(inout) :: rms_2(ele_grp%num_grp)
!
      integer(kind = kint) :: igrp, ist_grp, nitem_grp
!
!
      if (mesh%ele%nnod_4_ele .eq. num_t_quad) then
!
!$omp parallel do private(igrp,ist_grp,nitem_grp)
        do igrp = 1, ele_grp%num_grp
          ist_grp =   ele_grp%istack_grp(igrp-1)
          nitem_grp = ele_grp%istack_grp(igrp)                          &
     &               - ele_grp%istack_grp(igrp-1)
          call int_vol_2rms_ave_1egrp_q(mesh%node%numnod,               &
     &        mesh%ele%numele, mesh%ele%ie, mesh%ele%interior_ele,      &
     &        nitem_grp, ele_grp%item_grp(ist_grp+1:ist_grp+nitem_grp), &
     &        num_int, jac_3d%ntot_int, jac_3d%xjac, jac_3d%an,         &
     &        d1_nod, d2_nod, ave_1(igrp), rms_1(igrp),                 &
     &        ave_2(igrp), rms_2(igrp) )
        end do
!$omp end parallel do
!
      else
!
!$omp parallel do private(igrp,ist_grp,nitem_grp)
        do igrp = 1, ele_grp%num_grp
          ist_grp =   ele_grp%istack_grp(igrp-1)
          nitem_grp = ele_grp%istack_grp(igrp)                          &
     &               - ele_grp%istack_grp(igrp-1)
          call int_vol_2rms_ave_1egrp_l(mesh%node%numnod,               &
     &        mesh%ele%numele, mesh%ele%ie, mesh%ele%interior_ele,      &
     &        nitem_grp, ele_grp%item_grp(ist_grp+1:ist_grp+nitem_grp), &
     &        num_int, jac_3d%ntot_int, jac_3d%xjac, jac_3d%an,         &
     &        d1_nod, d2_nod, ave_1(igrp), rms_1(igrp),                 &
     &        ave_2(igrp), rms_2(igrp) )
        end do
!$omp end parallel do
!
      end if
!
      end subroutine int_vol_2rms_ave_ele_grps_type
!
!  ---------------------------------------------------------------------
!
      subroutine int_vol_2rms_ave_1egrp_type(mesh, jac_3d,              &
     &          num_int, nitem_grp, iele_grp, d1_nod, d2_nod,           &
     &          ave_1, rms_1, ave_2, rms_2)
!
      use int_vol_2rms_ave_1egrp
!
      type(mesh_geometry), intent(in) :: mesh
      type(jacobians_3d), intent(in) :: jac_3d
!
      integer (kind = kint), intent(in) :: num_int
      integer (kind = kint), intent(in) :: nitem_grp
      integer (kind = kint), intent(in) :: iele_grp(nitem_grp)
      real(kind = kreal), intent(in) :: d1_nod(mesh%node%numnod)
      real(kind = kreal), intent(in) :: d2_nod(mesh%node%numnod)
!
      real(kind = kreal), intent(inout) :: ave_1, rms_1, ave_2, rms_2
!
!
      if (mesh%ele%nnod_4_ele .eq. num_t_quad) then
        call int_vol_2rms_ave_1egrp_q(mesh%node%numnod,                 &
     &      mesh%ele%numele, mesh%ele%ie, mesh%ele%interior_ele,        &
     &      nitem_grp, iele_grp, num_int, jac_3d%ntot_int,              &
     &      jac_3d%xjac, jac_3d%an, d1_nod, d2_nod,                     &
     &      ave_1, rms_1, ave_2, rms_2)
      else
        call int_vol_2rms_ave_1egrp_l(mesh%node%numnod,                 &
     &      mesh%ele%numele, mesh%ele%ie, mesh%ele%interior_ele,        &
     &      nitem_grp, iele_grp, num_int, jac_3d%ntot_int,              &
     &      jac_3d%xjac, jac_3d%an, d1_nod, d2_nod,                     &
     &      ave_1, rms_1, ave_2, rms_2)
      end if
!
      end subroutine int_vol_2rms_ave_1egrp_type
!
!  ---------------------------------------------------------------------
!  ---------------------------------------------------------------------
!
      subroutine int_vol_dev_cor_ele_grps_type(mesh, ele_grp, jac_3d,   &
     &          num_int, d1_nod, d2_nod, ave_1, ave_2,                  &
     &          sig_1, sig_2, cov_l)
!
      use int_vol_dev_cor_1egrp
!
      type(mesh_geometry), intent(in) :: mesh
      type(group_data), intent(in) :: ele_grp
      type(jacobians_3d), intent(in) :: jac_3d
!
      integer (kind = kint), intent(in) :: num_int
      real(kind = kreal), intent(in) :: d1_nod(mesh%node%numnod)
      real(kind = kreal), intent(in) :: d2_nod(mesh%node%numnod)
      real(kind = kreal), intent(in) :: ave_1(ele_grp%num_grp)
      real(kind = kreal), intent(in) :: ave_2(ele_grp%num_grp)
!
      real(kind = kreal), intent(inout) :: sig_1(ele_grp%num_grp)
      real(kind = kreal), intent(inout) :: sig_2(ele_grp%num_grp)
      real(kind = kreal), intent(inout) :: cov_l(ele_grp%num_grp)
!
      integer(kind = kint) :: igrp, ist_grp, nitem_grp
!
!
!
      if (mesh%ele%nnod_4_ele .eq. num_t_quad) then
!
!$omp parallel do private(igrp,ist_grp,nitem_grp)
        do igrp = 1, ele_grp%num_grp
          ist_grp =   ele_grp%istack_grp(igrp-1)
          nitem_grp = ele_grp%istack_grp(igrp)                          &
     &               - ele_grp%istack_grp(igrp-1)
          call int_vol_dev_cor_1egrp_q(mesh%node%numnod,                &
     &        mesh%ele%numele, mesh%ele%ie, mesh%ele%interior_ele,      &
     &        nitem_grp, ele_grp%item_grp(ist_grp+1:ist_grp+nitem_grp), &
     &        num_int, jac_3d%ntot_int, jac_3d%xjac, jac_3d%an,         &
     &        d1_nod, d2_nod, ave_1(igrp), ave_2(igrp),                 &
     &        sig_1(igrp), sig_2(igrp), cov_l(igrp) )
        end do
!$omp end parallel do
!
      else
!
!$omp parallel do private(igrp,ist_grp,nitem_grp)
        do igrp = 1, ele_grp%num_grp
          ist_grp =   ele_grp%istack_grp(igrp-1)
          nitem_grp = ele_grp%istack_grp(igrp)                          &
     &               - ele_grp%istack_grp(igrp-1)
          call int_vol_dev_cor_1egrp_l(mesh%node%numnod,                &
     &        mesh%ele%numele, mesh%ele%ie, mesh%ele%interior_ele,      &
     &        nitem_grp, ele_grp%item_grp(ist_grp+1:ist_grp+nitem_grp), &
     &        num_int, jac_3d%ntot_int, jac_3d%xjac, jac_3d%an,         &
     &        d1_nod, d2_nod, ave_1(igrp), ave_2(igrp),                 &
     &        sig_1(igrp), sig_2(igrp), cov_l(igrp) )
        end do
!$omp end parallel do
!
      end if
!
      end subroutine int_vol_dev_cor_ele_grps_type
!
!  ---------------------------------------------------------------------
!
      subroutine int_vol_dev_cor_1egrp_type(mesh, jac_3d,               &
     &          num_int,  nitem_grp, iele_grp, d1_nod, d2_nod,          &
     &          ave_1, ave_2, sig_1, sig_2, cov_l)
!
      use int_vol_dev_cor_1egrp
!
      type(mesh_geometry), intent(in) :: mesh
      type(jacobians_3d), intent(in) :: jac_3d
!
      integer (kind = kint), intent(in) :: num_int
      integer (kind = kint), intent(in) :: nitem_grp
      integer (kind = kint), intent(in) :: iele_grp(nitem_grp)
      real(kind = kreal), intent(in) :: d1_nod(mesh%node%numnod)
      real(kind = kreal), intent(in) :: d2_nod(mesh%node%numnod)
      real(kind = kreal), intent(in) :: ave_1, ave_2
!
      real(kind = kreal), intent(inout) :: sig_1, sig_2, cov_l
!
!
      if (mesh%ele%nnod_4_ele .eq. num_t_quad) then
        call int_vol_dev_cor_1egrp_q(mesh%node%numnod,                  &
     &      mesh%ele%numele, mesh%ele%ie, mesh%ele%interior_ele,        &
     &      nitem_grp, iele_grp, num_int, jac_3d%ntot_int,              &
     &      jac_3d%xjac, jac_3d%an, d1_nod, d2_nod,                     &
     &      ave_1, ave_2, sig_1, sig_2, cov_l)
      else
        call int_vol_dev_cor_1egrp_l(mesh%node%numnod,                  &
     &      mesh%ele%numele, mesh%ele%ie, mesh%ele%interior_ele,        &
     &      nitem_grp, iele_grp, num_int, jac_3d%ntot_int,              &
     &      jac_3d%xjac, jac_3d%an, d1_nod, d2_nod,                     &
     &       ave_1, ave_2, sig_1, sig_2, cov_l)
      end if
!
      end subroutine int_vol_dev_cor_1egrp_type
!
!  ---------------------------------------------------------------------
!
      end module int_rms_ave_ele_grps_type
