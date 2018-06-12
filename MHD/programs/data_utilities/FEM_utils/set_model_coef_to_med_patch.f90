!set_model_coef_to_med_patch.f90
!      module set_model_coef_to_med_patch
!
!      written by Kemorin
!
!!      subroutine set_ele_grp_patch_2_psf_grd(ele_grp, psf_nod, psf_ele)
!      subroutine cal_ave_rms_csim(numnod_psf)
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
      subroutine set_ele_grp_patch_2_psf_grd                            &
     &         (ele_grp, psf_nod, psf_ele, psf_phys)
!
      use t_geometry_data
      use t_group_data
      use t_phys_data
      use m_geometry_constants
      use m_ctl_params_ele_grp_udt
      use m_tave_SGS_model_coefs
      use m_merdional_grouping_patch
      use cal_mesh_position
!
      type(group_data), intent(in) :: ele_grp
!
      type(node_data), intent(inout) :: psf_nod
      type(element_data), intent(inout) :: psf_ele
      type(phys_data), intent(inout) :: psf_phys
!
      integer(kind = kint) :: igrp, i, ist, ied, ist_nod
      integer(kind = kint) :: icou, inod, inum
!
!
      call find_start_element_group(ele_grp%num_grp, ele_grp%grp_name)
!
      psf_ele%nnod_4_ele = num_triangle
      psf_ele%numele = ele_grp%istack_grp(iend_ele_grp_drmd)            &
     &            - ele_grp%istack_grp(istart_ele_grp_drmd-1)
      psf_nod%numnod = 3 * psf_ele%numele
!
      psf_phys%num_phys =  num_comp
      psf_phys%ntot_phys = num_comp
!
      call alloc_node_geometry_w_sph(psf_nod)
      call allocate_ele_connect_type(psf_ele)
      call alloc_phys_name_type(psf_phys)
      call alloc_phys_data_type(psf_nod%numnod, psf_phys)
!
      do i = 1, psf_phys%num_phys
        psf_phys%num_component(i) = 1
        psf_phys%istack_component(i) = i
        write(psf_phys%phys_name(i),'(a)') comp_name(i)
      end do
!
      ist = ele_grp%istack_grp(istart_ele_grp_drmd-1)
      do i = 1, psf_ele%numele
        psf_nod%inod_global(i) = i
        psf_ele%iele_global(i) = i
        psf_ele%ie(i,1:3) = ie_egrp(i+ist,1:3)                          &
     &                 - 3*ele_grp%istack_grp(istart_ele_grp_drmd-1)
      end do
!
      ist_nod = 3*ele_grp%istack_grp(istart_ele_grp_drmd-1)
      icou = 0
      do inum = 1, num_ele_grp_drmd
        igrp = inum + istart_ele_grp_drmd - 1
        ist = 3*ele_grp%istack_grp(igrp-1) + 1
        ied = 3*ele_grp%istack_grp(igrp)
        do inod = ist, ied
          icou = icou + 1
          psf_nod%inod_global(icou) = icou
          psf_nod%xx(icou,1:3) = xx_egrp(inod,1:3)
        end do
      end do
!
      call set_spherical_position(psf_nod)
!
      end subroutine set_ele_grp_patch_2_psf_grd
!
!  ---------------------------------------------------------------------
!
      subroutine cal_ave_rms_csim(psf_phys, numnod_psf,                 &
     &          xmin_psf, xmax_psf, ave_psf, rms_psf)
!
      use m_ctl_params_ele_grp_udt
      use m_tave_SGS_model_coefs
      use t_phys_data
!
      integer(kind = kint), intent(in) :: numnod_psf
!
      type(phys_data), intent(inout) :: psf_phys
      real(kind = kreal), intent(inout) :: xmin_psf(psf_phys%ntot_phys)
      real(kind = kreal), intent(inout) :: xmax_psf(psf_phys%ntot_phys)
      real(kind = kreal), intent(inout) :: ave_psf(psf_phys%ntot_phys)
      real(kind = kreal), intent(inout) :: rms_psf(psf_phys%ntot_phys)
!
      integer(kind = kint) :: inod, nd, inum
!
!
      xmin_psf(1:psf_phys%ntot_phys) = coef(1,1:psf_phys%ntot_phys)
      xmax_psf(1:psf_phys%ntot_phys) = coef(1,1:psf_phys%ntot_phys)
      do nd = 1, psf_phys%ntot_phys
        do inum = 2, num_ele_grp_drmd
          xmin_psf(nd) = min(xmin_psf(nd), coef(inum,nd))
          xmax_psf(nd) = max(xmax_psf(nd), coef(inum,nd))
        end do
      end do
!
      ave_psf(1:psf_phys%ntot_phys) = zero
      rms_psf(1:psf_phys%ntot_phys) = zero
      do inod = 1, numnod_psf
        ave_psf(1:psf_phys%ntot_phys) = ave_psf(1:psf_phys%ntot_phys)   &
      &                + psf_phys%d_fld(inod,1:psf_phys%ntot_phys)
        rms_psf(1:psf_phys%ntot_phys) = rms_psf(1:psf_phys%ntot_phys)   &
      &                + psf_phys%d_fld(inod,1:psf_phys%ntot_phys)**2
      end do
      ave_psf(1:psf_phys%ntot_phys) = ave_psf(1:psf_phys%ntot_phys)     &
     &                 / dble(numnod_psf)
      rms_psf(1:psf_phys%ntot_phys)                                     &
     &        = sqrt(rms_psf(1:psf_phys%ntot_phys)) / dble(numnod_psf)
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
