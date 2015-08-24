!int_rms_ave_ele_grps_1st.f90
!      module int_rms_ave_ele_grps_1st
!
!     Written by H. Matsui on Aug., 2007
!     Modified by H. Matsui on Nov., 2008
!     Modified by H. Matsui on June, 2011
!
!      subroutine int_vol_rms_ave_ele_grps_1st(num_int,                 &
!     &          num_egrp, ntot_egrp, istack_egrp, iele_grp,            &
!     &          d_nod, ave_l, rms_l)
!      subroutine int_vol_rms_ave_1egrp_1st(num_int,                    &
!     &          nitem_grp, iele_grp, d_nod, ave_l, rms_l)
!
!      subroutine int_vol_2rms_ave_ele_grps_1st(num_int,                &
!     &          num_egrp, ntot_egrp, istack_egrp, iele_grp,            &
!     &          d1_nod, d2_nod, ave_1, rms_1, ave_2, rms_2)
!      subroutine int_vol_2rms_ave_1egrp_1st(num_int,                   &
!     &          nitem_grp, iele_grp, d1_nod, d2_nod,                   &
!     &          ave_1, rms_1, ave_2, rms_2)
!
!      subroutine int_vol_dev_cor_ele_grps_1st(num_int,                 &
!     &          num_egrp, ntot_egrp, istack_egrp, iele_grp,            &
!     &          d1_nod, d2_nod, ave_1, ave_2, sig_1, sig_2, cov_l)
!      subroutine int_vol_dev_cor_1egrp_1st(num_int,                    &
!     &          nitem_grp, iele_grp, d1_nod, d2_nod,                   &
!     &          ave_1, ave_2, sig_1, sig_2, cov_l)
!
      module int_rms_ave_ele_grps_1st
!
      use m_precision
      use m_constants
      use m_geometry_constants
      use m_fem_gauss_int_coefs
!
      use m_geometry_data
      use m_jacobians
!
      implicit none
!
!  ---------------------------------------------------------------------
!
      contains
!
!  ---------------------------------------------------------------------
!
      subroutine int_vol_rms_ave_ele_grps_1st(num_int,                  &
     &          num_egrp, ntot_egrp, istack_egrp, iele_grp,             &
     &          d_nod, ave_l, rms_l)
!
      use int_vol_rms_ave_1egrp
!
      integer (kind = kint), intent(in) :: num_int
!
      integer (kind = kint), intent(in) :: num_egrp, ntot_egrp
      integer (kind = kint), intent(in) :: istack_egrp(0:num_egrp)
      integer (kind = kint), intent(in) :: iele_grp(ntot_egrp)
      real(kind = kreal), intent(in) :: d_nod(node1%numnod)
!
      real(kind = kreal), intent(inout) :: ave_l(num_egrp)
      real(kind = kreal), intent(inout) :: rms_l(num_egrp)
!
      integer(kind = kint) :: igrp, ist_grp, nitem_grp
!
!
      if (ele1%nnod_4_ele .eq. num_t_quad) then
!
!$omp parallel do private(igrp,ist_grp,nitem_grp)
        do igrp = 1, num_egrp
          ist_grp =   istack_egrp(igrp-1) + 1
          nitem_grp = istack_egrp(igrp) - istack_egrp(igrp-1)
          call int_vol_rms_ave_1egrp_q                                  &
     &       (node1%numnod, ele1%numele, ele1%ie, ele1%interior_ele,    &
     &        nitem_grp, iele_grp(ist_grp), num_int,                    &
     &        jac1_3d_q%ntot_int, jac1_3d_q%xjac, jac1_3d_l%an,         &
     &        d_nod, ave_l(igrp), rms_l(igrp) )
        end do
!$omp end parallel do
!
      else
!
!$omp parallel do private(igrp,ist_grp,nitem_grp)
        do igrp = 1, num_egrp
          ist_grp =   istack_egrp(igrp-1) + 1
          nitem_grp = istack_egrp(igrp) - istack_egrp(igrp-1)
          call int_vol_rms_ave_1egrp_l                                  &
     &       (node1%numnod, ele1%numele, ele1%ie, ele1%interior_ele,    &
     &        nitem_grp, iele_grp(ist_grp), num_int,                    &
     &        jac1_3d_l%ntot_int, jac1_3d_l%xjac, jac1_3d_l%an,         &
     &        d_nod, ave_l(igrp), rms_l(igrp) )
        end do
!$omp end parallel do
!
      end if
!
      end subroutine int_vol_rms_ave_ele_grps_1st
!
!  ---------------------------------------------------------------------
!
      subroutine int_vol_rms_ave_1egrp_1st(num_int,                     &
     &          nitem_grp, iele_grp, d_nod, ave_l, rms_l)
!
      use int_vol_rms_ave_1egrp
!
      integer (kind = kint), intent(in) :: num_int
      integer (kind = kint), intent(in) :: nitem_grp
      integer (kind = kint), intent(in) :: iele_grp(nitem_grp)
      real(kind = kreal), intent(in) :: d_nod(node1%numnod)
      real(kind = kreal), intent(inout) :: ave_l, rms_l
!
!
      if (ele1%nnod_4_ele .eq. num_t_quad) then
        call int_vol_rms_ave_1egrp_q(node1%numnod, ele1%numele,         &
     &      ele1%ie, ele1%interior_ele, nitem_grp, iele_grp, num_int,   &
     &      jac1_3d_q%ntot_int, jac1_3d_q%xjac, jac1_3d_q%an,           &
     &      d_nod, ave_l, rms_l)
      else
        call int_vol_rms_ave_1egrp_l(node1%numnod, ele1%numele,         &
     &      ele1%ie, ele1%interior_ele, nitem_grp, iele_grp, num_int,   &
     &      jac1_3d_l%ntot_int, jac1_3d_l%xjac, jac1_3d_l%an,           &
     &      d_nod, ave_l, rms_l)
      end if
!
      end subroutine int_vol_rms_ave_1egrp_1st
!
!  ---------------------------------------------------------------------
!  ---------------------------------------------------------------------
!
      subroutine int_vol_2rms_ave_ele_grps_1st(num_int,                 &
     &          num_egrp, ntot_egrp, istack_egrp, iele_grp,             &
     &          d1_nod, d2_nod, ave_1, rms_1, ave_2, rms_2)
! 
      use int_vol_2rms_ave_1egrp
!
      integer (kind = kint), intent(in) :: num_int
!
      integer (kind = kint), intent(in) :: num_egrp, ntot_egrp
      integer (kind = kint), intent(in) :: istack_egrp(0:num_egrp)
      integer (kind = kint), intent(in) :: iele_grp(ntot_egrp)
      real(kind = kreal), intent(in) :: d1_nod(node1%numnod)
      real(kind = kreal), intent(in) :: d2_nod(node1%numnod)
!
      real(kind = kreal), intent(inout) :: ave_1(num_egrp)
      real(kind = kreal), intent(inout) :: rms_1(num_egrp)
      real(kind = kreal), intent(inout) :: ave_2(num_egrp)
      real(kind = kreal), intent(inout) :: rms_2(num_egrp)
!
      integer(kind = kint) :: igrp, ist_grp, nitem_grp
!
!
      if (ele1%nnod_4_ele .eq. num_t_quad) then
!
!$omp parallel do private(igrp,ist_grp,nitem_grp)
        do igrp = 1, num_egrp
          ist_grp =   istack_egrp(igrp-1) + 1
          nitem_grp = istack_egrp(igrp) - istack_egrp(igrp-1)
          call int_vol_2rms_ave_1egrp_q                                 &
     &       (node1%numnod, ele1%numele, ele1%ie, ele1%interior_ele,    &
     &        nitem_grp, iele_grp(ist_grp),                             &
     &        num_int, jac1_3d_q%ntot_int, jac1_3d_q%xjac,              &
     &        jac1_3d_q%an, d1_nod, d2_nod, ave_1(igrp), rms_1(igrp),   &
     &        ave_2(igrp), rms_2(igrp) )
        end do
!$omp end parallel do
!
      else
!
!$omp parallel do private(igrp,ist_grp,nitem_grp)
        do igrp = 1, num_egrp
          ist_grp =   istack_egrp(igrp-1) + 1
          nitem_grp = istack_egrp(igrp) - istack_egrp(igrp-1)
          call int_vol_2rms_ave_1egrp_l                                 &
     &       (node1%numnod, ele1%numele, ele1%ie, ele1%interior_ele,    &
     &        nitem_grp, iele_grp(ist_grp), num_int,                    &
     &        jac1_3d_l%ntot_int, jac1_3d_l%xjac,  jac1_3d_l%an,        &
     &        d1_nod, d2_nod, ave_1(igrp), rms_1(igrp),                 &
     &        ave_2(igrp), rms_2(igrp) )
        end do
!$omp end parallel do
!
      end if
!
      end subroutine int_vol_2rms_ave_ele_grps_1st
!
!  ---------------------------------------------------------------------
!
      subroutine int_vol_2rms_ave_1egrp_1st(num_int,                    &
     &          nitem_grp, iele_grp, d1_nod, d2_nod,                    &
     &          ave_1, rms_1, ave_2, rms_2)
!
      use int_vol_2rms_ave_1egrp
!
      integer (kind = kint), intent(in) :: num_int
      integer (kind = kint), intent(in) :: nitem_grp
      integer (kind = kint), intent(in) :: iele_grp(nitem_grp)
      real(kind = kreal), intent(in) :: d1_nod(node1%numnod)
      real(kind = kreal), intent(in) :: d2_nod(node1%numnod)
      real(kind = kreal), intent(inout) :: ave_1, rms_1, ave_2, rms_2
!
!
      if (ele1%nnod_4_ele .eq. num_t_quad) then
        call int_vol_2rms_ave_1egrp_q                                   &
     &     (node1%numnod, ele1%numele, ele1%ie, ele1%interior_ele,      &
     &      nitem_grp, iele_grp, num_int,                               &
     &      jac1_3d_q%ntot_int, jac1_3d_q%xjac, jac1_3d_q%an,           &
     &      d1_nod, d2_nod, ave_1, rms_1, ave_2, rms_2)
      else
        call int_vol_2rms_ave_1egrp_l                                   &
     &     (node1%numnod, ele1%numele, ele1%ie, ele1%interior_ele,      &
     &      nitem_grp, iele_grp, num_int,                               &
     &      jac1_3d_l%ntot_int, jac1_3d_l%xjac, jac1_3d_l%an,           &
     &      d1_nod, d2_nod, ave_1, rms_1, ave_2, rms_2)
      end if
!
      end subroutine int_vol_2rms_ave_1egrp_1st
!
!  ---------------------------------------------------------------------
!  ---------------------------------------------------------------------
!
      subroutine int_vol_dev_cor_ele_grps_1st(num_int,                  &
     &          num_egrp, ntot_egrp, istack_egrp, iele_grp,             &
     &          d1_nod, d2_nod, ave_1, ave_2, sig_1, sig_2, cov_l)
!
      use int_vol_dev_cor_1egrp
!
      integer (kind = kint), intent(in) :: num_int
      integer (kind = kint), intent(in) :: num_egrp, ntot_egrp
      integer (kind = kint), intent(in) :: istack_egrp(0:num_egrp)
      integer (kind = kint), intent(in) :: iele_grp(ntot_egrp)
      real(kind = kreal), intent(in) :: d1_nod(node1%numnod)
      real(kind = kreal), intent(in) :: d2_nod(node1%numnod)
      real(kind = kreal), intent(in) :: ave_1(num_egrp)
      real(kind = kreal), intent(in) :: ave_2(num_egrp)
!
      real(kind = kreal), intent(inout) :: sig_1(num_egrp)
      real(kind = kreal), intent(inout) :: sig_2(num_egrp)
      real(kind = kreal), intent(inout) :: cov_l(num_egrp)
!
      integer(kind = kint) :: igrp, ist_grp, nitem_grp
!
!
!
      if (ele1%nnod_4_ele .eq. num_t_quad) then
!
!$omp parallel do private(igrp,ist_grp,nitem_grp)
        do igrp = 1, num_egrp
          ist_grp =   istack_egrp(igrp-1) + 1
          nitem_grp = istack_egrp(igrp) - istack_egrp(igrp-1)
          call int_vol_dev_cor_1egrp_q                                  &
     &       (node1%numnod, ele1%numele, ele1%ie, ele1%interior_ele,    &
     &        nitem_grp, iele_grp(ist_grp),                             &
     &        num_int, jac1_3d_q%ntot_int, jac1_3d_q%xjac,              &
     &        jac1_3d_q%an, d1_nod, d2_nod, ave_1(igrp), ave_2(igrp),   &
     &        sig_1(igrp), sig_2(igrp), cov_l(igrp) )
        end do
!$omp end parallel do
!
      else
!
!$omp parallel do private(igrp,ist_grp,nitem_grp)
        do igrp = 1, num_egrp
          ist_grp =   istack_egrp(igrp-1) + 1
          nitem_grp = istack_egrp(igrp) - istack_egrp(igrp-1)
          call int_vol_dev_cor_1egrp_l                                  &
     &       (node1%numnod, ele1%numele, ele1%ie, ele1%interior_ele,    &
     &        nitem_grp, iele_grp(ist_grp), num_int,                    &
     &        jac1_3d_l%ntot_int, jac1_3d_l%xjac, jac1_3d_l%an,         &
     &        d1_nod, d2_nod, ave_1(igrp), ave_2(igrp),                 &
     &        sig_1(igrp), sig_2(igrp), cov_l(igrp) )
        end do
!$omp end parallel do
!
      end if
!
      end subroutine int_vol_dev_cor_ele_grps_1st
!
!  ---------------------------------------------------------------------
!
      subroutine int_vol_dev_cor_1egrp_1st(num_int,                     &
     &          nitem_grp, iele_grp, d1_nod, d2_nod,                    &
     &          ave_1, ave_2, sig_1, sig_2, cov_l)
!
      use int_vol_dev_cor_1egrp
!
      integer (kind = kint), intent(in) :: num_int
      integer (kind = kint), intent(in) :: nitem_grp
      integer (kind = kint), intent(in) :: iele_grp(nitem_grp)
      real(kind = kreal), intent(in) :: d1_nod(node1%numnod)
      real(kind = kreal), intent(in) :: d2_nod(node1%numnod)
      real(kind = kreal), intent(in) :: ave_1, ave_2
      real(kind = kreal), intent(inout) :: sig_1, sig_2, cov_l
!
!
      if (ele1%nnod_4_ele .eq. num_t_quad) then
        call int_vol_dev_cor_1egrp_q                                    &
     &     (node1%numnod, ele1%numele, ele1%ie, ele1%interior_ele,      &
     &      nitem_grp, iele_grp, num_int,                               &
     &      jac1_3d_q%ntot_int, jac1_3d_q%xjac, jac1_3d_q%an,           &
     &      d1_nod, d2_nod, ave_1, ave_2, sig_1, sig_2, cov_l)
      else
        call int_vol_dev_cor_1egrp_l                                    &
     &     (node1%numnod, ele1%numele, ele1%ie, ele1%interior_ele,      &
     &      nitem_grp, iele_grp, num_int,                               &
     &      jac1_3d_l%ntot_int, jac1_3d_l%xjac, jac1_3d_l%an,           &
     &      d1_nod, d2_nod, ave_1, ave_2, sig_1, sig_2, cov_l)
      end if
!
      end subroutine int_vol_dev_cor_1egrp_1st
!
!  ---------------------------------------------------------------------
!
      end module int_rms_ave_ele_grps_1st
