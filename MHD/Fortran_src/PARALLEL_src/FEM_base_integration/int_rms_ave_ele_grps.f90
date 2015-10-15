!int_rms_ave_ele_grps.f90
!      module int_rms_ave_ele_grps
!
!     Written by H. Matsui on Aug., 2007
!     Modified by H. Matsui on Nov., 2008
!     Modified by H. Matsui on June, 2011
!
!!      subroutine int_vol_rms_ave_ele_grps                             &
!!     &         (node, ele, ele_grp, jac_3d_q, jac_3d_l, num_int,      &
!!     &          ncomp_nod, i_fld, d_nod, ave_l, rms_l)
!!
!!      subroutine int_vol_2rms_ave_ele_grps                            &
!!     &         (node, ele, ele_grp, jac_3d_q, jac_3d_l, num_int,      &
!!     &          ncomp_1, ifld_1, d1_nod, ncomp_2, ifld_2, d2_nod,     &
!!     &          ave_1, rms_1, ave_2, rms_2)
!!
!!      subroutine int_vol_dev_cor_ele_grps                             &
!!     &         (node, ele, ele_grp, jac_3d_q, jac_3d_l, num_int,      &
!!     &          ncomp_1, ifld_1, d1_nod, ncomp_2, ifld_2, d2_nod,     &
!!     &          ave_1, ave_2, sig_1, sig_2, cov_l)
!
      module int_rms_ave_ele_grps
!
      use m_precision
      use m_constants
      use m_geometry_constants
      use m_fem_gauss_int_coefs
!
      use t_geometry_data
      use t_jacobians
!
      implicit none
!
      private :: sel_int_vol_rms_ave_1egrp, sel_int_vol_2rms_ave_1egrp
      private :: sel_int_vol_dev_cor_1egrp
!
!  ---------------------------------------------------------------------
!
      contains
!
!  ---------------------------------------------------------------------
!
      subroutine int_vol_rms_ave_ele_grps                               &
     &         (node, ele, ele_grp, jac_3d_q, jac_3d_l, num_int,       &
     &          ncomp_nod, i_fld, d_nod, ave_l, rms_l)
!
      use t_group_data
!
      type(node_data),    intent(in) :: node
      type(element_data), intent(in) :: ele
      type(group_data), intent(in) ::   ele_grp
      type(jacobians_3d), intent(in) :: jac_3d_q, jac_3d_l
!
      integer (kind = kint), intent(in) :: num_int
!
      integer (kind = kint), intent(in) :: ncomp_nod, i_fld
      real(kind = kreal), intent(in) :: d_nod(node%numnod,ncomp_nod)
!
      real(kind = kreal), intent(inout) :: ave_l(ele_grp%num_grp)
      real(kind = kreal), intent(inout) :: rms_l(ele_grp%num_grp)
!
      integer(kind = kint) :: igrp
!
!
!$omp parallel do private(igrp)
      do igrp = 1, ele_grp%num_grp
        call sel_int_vol_rms_ave_1egrp                                  &
     &     (node, ele, jac_3d_q, jac_3d_l, num_int, igrp,               &
     &      ele_grp%num_grp, ele_grp%num_item,                          &
     &      ele_grp%istack_grp, ele_grp%item_grp,                       &
     &      ncomp_nod, i_fld, d_nod, ave_l(igrp), rms_l(igrp))
      end do
!$omp end parallel do
!
      end subroutine int_vol_rms_ave_ele_grps
!
!  ---------------------------------------------------------------------
!
      subroutine int_vol_2rms_ave_ele_grps                              &
     &         (node, ele, ele_grp, jac_3d_q, jac_3d_l, num_int,        &
     &          ncomp_1, ifld_1, d1_nod, ncomp_2, ifld_2, d2_nod,       &
     &          ave_1, rms_1, ave_2, rms_2)
! 
      use t_group_data
!
      type(node_data),    intent(in) :: node
      type(element_data), intent(in) :: ele
      type(group_data), intent(in)   :: ele_grp
      type(jacobians_3d), intent(in) :: jac_3d_q, jac_3d_l
!
      integer (kind = kint), intent(in) :: num_int
!
      integer (kind = kint), intent(in) :: ncomp_1, ifld_1
      integer (kind = kint), intent(in) :: ncomp_2, ifld_2
      real(kind = kreal), intent(in) :: d1_nod(node%numnod,ncomp_1)
      real(kind = kreal), intent(in) :: d2_nod(node%numnod,ncomp_2)
!
      real(kind = kreal), intent(inout) :: ave_1(ele_grp%num_grp)
      real(kind = kreal), intent(inout) :: rms_1(ele_grp%num_grp)
      real(kind = kreal), intent(inout) :: ave_2(ele_grp%num_grp)
      real(kind = kreal), intent(inout) :: rms_2(ele_grp%num_grp)
!
      integer(kind = kint) :: igrp
!
!
!$omp parallel do private(igrp)
      do igrp = 1, ele_grp%num_grp
        call sel_int_vol_2rms_ave_1egrp                                 &
     &     (node, ele, jac_3d_q, jac_3d_l, num_int, igrp,               &
     &      ele_grp%num_grp, ele_grp%num_item,                          &
     &      ele_grp%istack_grp, ele_grp%item_grp,                       &
     &      ncomp_1, ifld_1, d1_nod, ncomp_2, ifld_2, d2_nod,           &
     &      ave_1(igrp), rms_1(igrp), ave_2(igrp), rms_2(igrp))
      end do
!$omp end parallel do
!
      end subroutine int_vol_2rms_ave_ele_grps
!
!  ---------------------------------------------------------------------
!  ---------------------------------------------------------------------
!
      subroutine int_vol_dev_cor_ele_grps                               &
     &         (node, ele, ele_grp, jac_3d_q, jac_3d_l, num_int,        &
     &          ncomp_1, ifld_1, d1_nod, ncomp_2, ifld_2, d2_nod,       &
     &          ave_1, ave_2, sig_1, sig_2, cov_l)
!
      use t_group_data
!
      type(node_data),    intent(in) :: node
      type(element_data), intent(in) :: ele
      type(group_data), intent(in) :: ele_grp
      type(jacobians_3d), intent(in) :: jac_3d_q, jac_3d_l
!
      integer (kind = kint), intent(in) :: num_int
      integer (kind = kint), intent(in) :: ncomp_1, ifld_1
      integer (kind = kint), intent(in) :: ncomp_2, ifld_2
      real(kind = kreal), intent(in) :: d1_nod(node%numnod,ncomp_1)
      real(kind = kreal), intent(in) :: d2_nod(node%numnod,ncomp_2)
      real(kind = kreal), intent(in) :: ave_1(ele_grp%num_grp)
      real(kind = kreal), intent(in) :: ave_2(ele_grp%num_grp)
!
      real(kind = kreal), intent(inout) :: sig_1(ele_grp%num_grp)
      real(kind = kreal), intent(inout) :: sig_2(ele_grp%num_grp)
      real(kind = kreal), intent(inout) :: cov_l(ele_grp%num_grp)
!
      integer(kind = kint) :: igrp
!
!
!
!$omp parallel do private(igrp)
      do igrp = 1, ele_grp%num_grp
        call sel_int_vol_dev_cor_1egrp                                &
     &     (node, ele, jac_3d_q, jac_3d_l, num_int, igrp,             &
     &      ele_grp%num_grp, ele_grp%num_item,                        &
     &      ele_grp%istack_grp, ele_grp%item_grp,                     &
     &      ncomp_1, ifld_1, d1_nod, ncomp_2, ifld_2, d2_nod,         &
     &      ave_1(igrp), ave_2(igrp), sig_1(igrp), sig_2(igrp),       &
     &      cov_l(igrp))
      end do
!$omp end parallel do
!
      end subroutine int_vol_dev_cor_ele_grps
!
!  ---------------------------------------------------------------------
!  ---------------------------------------------------------------------
!
      subroutine sel_int_vol_rms_ave_1egrp                              &
     &         (node, ele, jac_3d_q, jac_3d_l, num_int, igrp,           &
     &          num_egrp, ntot_egrp, istack_egrp, iele_grp,             &
     &          ncomp_nod, i_fld, d_nod, ave_l, rms_l)
!
      use int_vol_rms_ave_1egrp
!
      type(node_data),    intent(in) :: node
      type(element_data), intent(in) :: ele
      type(jacobians_3d), intent(in) :: jac_3d_q, jac_3d_l
!
      integer (kind = kint), intent(in) :: num_int
      integer (kind = kint), intent(in) :: num_egrp, ntot_egrp, igrp
      integer (kind = kint), intent(in) :: istack_egrp(0:num_egrp)
      integer (kind = kint), intent(in) :: iele_grp(ntot_egrp)
      integer (kind = kint), intent(in) :: ncomp_nod, i_fld
      real(kind = kreal), intent(in) :: d_nod(node%numnod,ncomp_nod)
      real(kind = kreal), intent(inout) :: ave_l, rms_l
!
      integer(kind = kint) :: ist_grp, nitem_grp
!
!
      ist_grp =   istack_egrp(igrp-1) + 1
      nitem_grp = istack_egrp(igrp) - istack_egrp(igrp-1)
      if (ele%nnod_4_ele .eq. num_t_quad) then
        call int_vol_rms_ave_1egrp_q(node%numnod, ele%numele, ele%ie,   &
     &      ele%interior_ele, nitem_grp, iele_grp(ist_grp), num_int,    &
     &      jac_3d_q%ntot_int, jac_3d_q%xjac, jac_3d_q%an,              &
     &      d_nod(1,i_fld), ave_l, rms_l)
      else
        call int_vol_rms_ave_1egrp_l(node%numnod, ele%numele, ele%ie,   &
     &      ele%interior_ele, nitem_grp, iele_grp(ist_grp), num_int,    &
     &      jac_3d_l%ntot_int, jac_3d_l%xjac, jac_3d_l%an,              &
     &      d_nod(1,i_fld), ave_l, rms_l)
      end if
!
      end subroutine sel_int_vol_rms_ave_1egrp
!
!  ---------------------------------------------------------------------
!
      subroutine sel_int_vol_2rms_ave_1egrp                             &
     &         (node, ele, jac_3d_q, jac_3d_l, num_int, igrp,           &
     &          num_egrp, ntot_egrp, istack_egrp, iele_grp,             &
     &          ncomp_1, ifld_1, d1_nod, ncomp_2, ifld_2, d2_nod,       &
     &          ave_1, rms_1, ave_2, rms_2)
!
      use int_vol_2rms_ave_1egrp
!
      type(node_data),    intent(in) :: node
      type(element_data), intent(in) :: ele
      type(jacobians_3d), intent(in) :: jac_3d_q, jac_3d_l
!
      integer (kind = kint), intent(in) :: num_int
      integer (kind = kint), intent(in) :: num_egrp, ntot_egrp, igrp
      integer (kind = kint), intent(in) :: istack_egrp(0:num_egrp)
      integer (kind = kint), intent(in) :: iele_grp(ntot_egrp)
      integer (kind = kint), intent(in) :: ncomp_1, ifld_1
      integer (kind = kint), intent(in) :: ncomp_2, ifld_2
      real(kind = kreal), intent(in) :: d1_nod(node%numnod,ncomp_1)
      real(kind = kreal), intent(in) :: d2_nod(node%numnod,ncomp_2)
      real(kind = kreal), intent(inout) :: ave_1, rms_1, ave_2, rms_2
!
      integer(kind = kint) :: ist_grp, nitem_grp
!
!
      ist_grp =   istack_egrp(igrp-1) + 1
      nitem_grp = istack_egrp(igrp) - istack_egrp(igrp-1)
      if (ele%nnod_4_ele .eq. num_t_quad) then
        call int_vol_2rms_ave_1egrp_q                                   &
     &     (node%numnod, ele%numele, ele%ie, ele%interior_ele,          &
     &      nitem_grp, iele_grp(ist_grp), num_int,                      &
     &      jac_3d_q%ntot_int, jac_3d_q%xjac, jac_3d_q%an,              &
     &      d1_nod(1,ifld_1), d2_nod(1,ifld_2), ave_1, rms_1, ave_2,    &
     &      rms_2)
      else
        call int_vol_2rms_ave_1egrp_l                                   &
     &     (node%numnod, ele%numele, ele%ie, ele%interior_ele,          &
     &      nitem_grp, iele_grp(ist_grp), num_int,                      &
     &      jac_3d_l%ntot_int, jac_3d_l%xjac, jac_3d_l%an,              &
     &      d1_nod(1,ifld_1), d2_nod(1,ifld_2), ave_1, rms_1, ave_2,    &
     &      rms_2)
      end if
!
      end subroutine sel_int_vol_2rms_ave_1egrp
!
!  ---------------------------------------------------------------------
!
      subroutine sel_int_vol_dev_cor_1egrp                              &
     &         (node, ele, jac_3d_q, jac_3d_l, num_int, igrp,           &
     &          num_egrp, ntot_egrp, istack_egrp, iele_grp,             &
     &          ncomp_1, ifld_1, d1_nod, ncomp_2, ifld_2, d2_nod,       &
     &          ave_1, ave_2, sig_1, sig_2, cov_l)
!
      use int_vol_dev_cor_1egrp
!
      type(node_data),    intent(in) :: node
      type(element_data), intent(in) :: ele
      type(jacobians_3d), intent(in) :: jac_3d_q, jac_3d_l
!
      integer (kind = kint), intent(in) :: num_int
      integer (kind = kint), intent(in) :: num_egrp, ntot_egrp, igrp
      integer (kind = kint), intent(in) :: istack_egrp(0:num_egrp)
      integer (kind = kint), intent(in) :: iele_grp(ntot_egrp)
      integer (kind = kint), intent(in) :: ncomp_1, ifld_1
      integer (kind = kint), intent(in) :: ncomp_2, ifld_2
      real(kind = kreal), intent(in) :: d1_nod(node%numnod,ncomp_1)
      real(kind = kreal), intent(in) :: d2_nod(node%numnod,ncomp_2)
      real(kind = kreal), intent(in) :: ave_1, ave_2
      real(kind = kreal), intent(inout) :: sig_1, sig_2, cov_l
!
      integer(kind = kint) :: ist_grp, nitem_grp
!
!
      ist_grp =   istack_egrp(igrp-1) + 1
      nitem_grp = istack_egrp(igrp) - istack_egrp(igrp-1)
      if (ele%nnod_4_ele .eq. num_t_quad) then
        call int_vol_dev_cor_1egrp_q                                    &
     &     (node%numnod, ele%numele, ele%ie, ele%interior_ele,          &
     &      nitem_grp, iele_grp(ist_grp), num_int,                      &
     &      jac_3d_q%ntot_int, jac_3d_q%xjac, jac_3d_q%an,              &
     &      d1_nod(1,ifld_1), d2_nod(1,ifld_2), ave_1, ave_2,           &
     &      sig_1, sig_2, cov_l)
      else
        call int_vol_dev_cor_1egrp_l                                    &
     &     (node%numnod, ele%numele, ele%ie, ele%interior_ele,          &
     &      nitem_grp, iele_grp(ist_grp), num_int,                      &
     &      jac_3d_l%ntot_int, jac_3d_l%xjac, jac_3d_l%an,              &
     &      d1_nod(1,ifld_1), d2_nod(1,ifld_2), ave_1, ave_2,           &
     &      sig_1, sig_2, cov_l)
      end if
!
      end subroutine sel_int_vol_dev_cor_1egrp
!
!  ---------------------------------------------------------------------
!
      end module int_rms_ave_ele_grps
