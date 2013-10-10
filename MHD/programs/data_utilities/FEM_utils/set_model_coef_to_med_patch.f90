!set_model_coef_to_med_patch.f90
!      module set_model_coef_to_med_patch
!
!      written by Kemorin
!
!      subroutine set_ele_grp_patch_2_psf_grd
!      subroutine set_field_to_med_patch
!      subroutine cal_ave_rms_csim
!
      module set_model_coef_to_med_patch
!
      use m_precision
      use m_constants
!
      implicit none
!
!  ---------------------------------------------------------------------
!
      contains
!
!  ---------------------------------------------------------------------
!
      subroutine set_ele_grp_patch_2_psf_grd
!
      use m_element_group
      use m_psf_results
      use m_ctl_params_ele_grp_udt
      use m_tave_SGS_model_coefs
      use m_merdional_grouping_patch
      use coordinate_converter
!
      integer(kind = kint) :: igrp, i, ist, ied, ist_nod
      integer(kind = kint) :: icou, inod, inum
!
!
      call find_start_element_group(num_mat, mat_name)
!
      numele_psf = mat_istack(iend_ele_grp_drmd)                        &
     &            - mat_istack(istart_ele_grp_drmd-1)
      numnod_psf = 3 * numele_psf
!
      nfield_psf =   num_comp
      ncomptot_psf = num_comp
!
      call allocate_psf_results
      call allocate_psf_num_field
      call allocate_psf_field_data
!
      do i = 1, nfield_psf
        ncomp_psf(i) = 1
        istack_comp_psf(i) = i
        write(psf_data_name(i),'(a)') comp_name(i)
      end do
!
      ist = mat_istack(istart_ele_grp_drmd-1)
      do i = 1, numele_psf
        inod_psf(i) = i
        iele_psf(i) = i
        ie_psf(i,1:3) = ie_egrp(i+ist,1:3)                              &
     &                 - 3*mat_istack(istart_ele_grp_drmd-1)
      end do
!
      ist_nod = 3*mat_istack(istart_ele_grp_drmd-1)
      icou = 0
      do inum = 1, num_ele_grp_drmd
        igrp = inum + istart_ele_grp_drmd - 1
        ist = 3*mat_istack(igrp-1) + 1
        ied = 3*mat_istack(igrp)
        do inod = ist, ied
          icou = icou + 1
          inod_psf(icou) = icou
          xx_psf(icou,1:3) = xx_egrp(inod,1:3)
        end do
      end do
!
      call position_2_sph (numnod_psf, xx_psf,                          &
     &    rtp_psf(1,1), rtp_psf(1,2), rtp_psf(1,3),                     &
     &    ar_psf, ss_psf, ar_psf)
!
      end subroutine set_ele_grp_patch_2_psf_grd
!
!  ---------------------------------------------------------------------
!
      subroutine set_field_to_med_patch(num_grp, num_comp,              &
     &          comp_name, d_grp)
!
      use m_element_group
      use m_psf_results
      use m_ctl_params_ele_grp_udt
      use m_merdional_grouping_patch
!
      integer(kind = kint), intent(in) :: num_grp, num_comp
      character(len = kchara), intent(in) :: comp_name(num_comp)
      real(kind = kreal), intent(in) :: d_grp(num_grp, num_comp)
!
      integer(kind = kint) :: igrp, i, ist, ied, ist_nod
      integer(kind = kint) :: icou, inod, inum
!
!
      do i = 1, nfield_psf
        ncomp_psf(i) = 1
        istack_comp_psf(i) = i
        write(psf_data_name(i),'(a)') comp_name(i)
      end do
!
      ist_nod = 3*mat_istack(istart_ele_grp_drmd-1)
      icou = 0
      do inum = 1, num_ele_grp_drmd
        igrp = inum + istart_ele_grp_drmd - 1
        ist = 3*mat_istack(igrp-1) + 1
        ied = 3*mat_istack(igrp)
        do inod = ist, ied
          icou = icou + 1
          inod_psf(icou) = icou
          d_nod_psf(icou,1:num_comp) = d_grp(inum,1:num_comp)
        end do
      end do
!
      end subroutine set_field_to_med_patch
!
!  ---------------------------------------------------------------------
!
      subroutine cal_ave_rms_csim
!
      use m_element_group
      use m_ctl_params_ele_grp_udt
      use m_tave_SGS_model_coefs
      use m_psf_results
!
      integer(kind = kint) :: inod, nd, inum
!
!
      xmin_psf(1:ncomptot_psf) = coef(1,1:ncomptot_psf)
      xmax_psf(1:ncomptot_psf) = coef(1,1:ncomptot_psf)
      do nd = 1, ncomptot_psf
        do inum = 2, num_ele_grp_drmd
          xmin_psf(nd) = min(xmin_psf(nd), coef(inum,nd))
          xmax_psf(nd) = max(xmax_psf(nd), coef(inum,nd))
        end do
      end do
!
      ave_psf(1:ncomptot_psf) = zero
      rms_psf(1:ncomptot_psf) = zero
      do inod = 1, numnod_psf
        ave_psf(1:ncomptot_psf) = ave_psf(1:ncomptot_psf)               &
      &                          + d_nod_psf(inod,1:ncomptot_psf)
        rms_psf(1:ncomptot_psf) = rms_psf(1:ncomptot_psf)               &
      &                          + d_nod_psf(inod,1:ncomptot_psf)**2
      end do
      ave_psf(1:ncomptot_psf) = ave_psf(1:ncomptot_psf)                 &
     &                         / dble(numnod_psf)
      rms_psf(1:ncomptot_psf) = sqrt(rms_psf(1:ncomptot_psf))           &
     &                         / dble(numnod_psf)
!
      end subroutine cal_ave_rms_csim
!
!  ---------------------------------------------------------------------
!
      subroutine sqrt_of_rms_coefs(num_grp, num_comp, d_grp)
!
      use m_ctl_params_ele_grp_udt
!
      integer(kind = kint), intent(in) :: num_grp, num_comp
      real(kind = kreal), intent(inout) :: d_grp(num_grp, num_comp)
!
      integer(kind = kint) :: nd
!
!
      if(iflag_sqrt_rms .eq. 1) then
        do nd = 1, num_comp
          d_grp(1:num_grp,nd) = sqrt(d_grp(1:num_grp,nd))
        end do
      end if
!
      end subroutine sqrt_of_rms_coefs
!
!  ---------------------------------------------------------------------
!
      end module set_model_coef_to_med_patch
