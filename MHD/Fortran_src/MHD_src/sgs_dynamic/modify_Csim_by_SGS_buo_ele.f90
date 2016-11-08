!modify_Csim_by_SGS_buo_ele.f90
!      module modify_Csim_by_SGS_buo_ele
!
!      written by H. Matsui on Aug., 2007
!
!!      subroutine mod_Csim_by_SGS_buoyancy_ele(ele, layer_egrp,        &
!!     &          ifld_sgs, icomp_sgs, wk_sgs, sgs_coefs)
!!        type(element_data), intent(in) :: ele
!!        type(group_data), intent(in) :: layer_egrp
!!        type(SGS_terms_address), intent(in) :: ifld_sgs
!!        type(SGS_terms_address), intent(in) :: icomp_sgs
!!        type(dynamic_model_data), intent(in) :: wk_sgs
!!        type(SGS_coefficients_type), intent(inout) :: sgs_coefs
!
      module modify_Csim_by_SGS_buo_ele
!
      use m_precision
      use m_constants
!
      use m_phys_constants
!
      implicit none
!
      private :: modify_cmpCsim_by_SGS_dbuo_ele
      private :: modify_fldCsim_by_SGS_dbuo_ele
      private :: modify_cmpCsim_by_SGS_buo_ele
      private :: modify_fldCsim_by_SGS_buo_ele
!
!  ---------------------------------------------------------------------
!
      contains
!
!  ---------------------------------------------------------------------
!
      subroutine mod_Csim_by_SGS_buoyancy_ele(ele, layer_egrp,          &
     &          ifld_sgs, icomp_sgs, wk_sgs, sgs_coefs)
!
      use m_control_parameter
      use t_geometry_data
      use t_group_data
      use t_material_property
      use t_SGS_model_coefs
      use t_ele_info_4_dynamic
      use set_sgs_diff_model_coefs
!
      type(element_data), intent(in) :: ele
      type(group_data), intent(in) :: layer_egrp
      type(SGS_terms_address), intent(in) :: ifld_sgs
      type(SGS_terms_address), intent(in) :: icomp_sgs
      type(dynamic_model_data), intent(in) :: wk_sgs
      type(SGS_coefficients_type), intent(inout) :: sgs_coefs
!
!
      call clear_model_coefs_2_ele(ele, n_sym_tensor,                   &
     &    icomp_sgs%i_mom_flux, sgs_coefs%ntot_comp, sgs_coefs%ak)
!
      if(iflag_4_gravity .gt. id_turn_OFF                               &
     &     .and. iflag_4_composit_buo .gt. id_turn_OFF) then
        if(itype_SGS_m_flux_coef .eq. 1) then
          call modify_cmpCsim_by_SGS_dbuo_ele(ifld_sgs%i_comp_buoyancy, &
     &        ifld_sgs%i_buoyancy, icomp_sgs%i_mom_flux,                &
     &        layer_egrp%num_grp, layer_egrp%num_item,                  &
     &        layer_egrp%istack_grp_smp, layer_egrp%item_grp,           &
     &        ele%numele, sgs_coefs%num_field, sgs_coefs%ntot_comp,     &
     &        wk_sgs%fld_coef, wk_sgs%comp_clip, sgs_coefs%ak)
        else
          call modify_fldCsim_by_SGS_dbuo_ele                           &
     &       (ifld_sgs%i_comp_buoyancy, ifld_sgs%i_buoyancy,            &
     &        ifld_sgs%i_mom_flux, icomp_sgs%i_mom_flux,                &
     &        layer_egrp%num_grp, layer_egrp%num_item,                  &
     &        layer_egrp%istack_grp_smp, layer_egrp%item_grp,           &
     &        ele%numele, sgs_coefs%num_field, sgs_coefs%ntot_comp,     &
     &        wk_sgs%fld_coef, wk_sgs%fld_clip, sgs_coefs%ak)
        end if
      else if(iflag_4_gravity .gt. id_turn_OFF) then
        if(itype_SGS_m_flux_coef .eq. 1) then
          call modify_cmpCsim_by_SGS_buo_ele                            &
     &       (ifld_sgs%i_buoyancy, icomp_sgs%i_mom_flux,                &
     &        layer_egrp%num_grp, layer_egrp%num_item,                  &
     &        layer_egrp%istack_grp_smp, layer_egrp%item_grp,           &
     &        ele%numele, sgs_coefs%num_field, sgs_coefs%ntot_comp,     &
     &        wk_sgs%fld_coef, wk_sgs%comp_clip, sgs_coefs%ak)
        else
          call modify_fldCsim_by_SGS_buo_ele(ifld_sgs%i_buoyancy,       &
     &        ifld_sgs%i_mom_flux, icomp_sgs%i_mom_flux,                &
     &        layer_egrp%num_grp, layer_egrp%num_item,                  &
     &        layer_egrp%istack_grp_smp, layer_egrp%item_grp,           &
     &        ele%numele, sgs_coefs%num_field, sgs_coefs%ntot_comp,     &
     &        wk_sgs%fld_coef, wk_sgs%fld_clip, sgs_coefs%ak)
        end if
      else if(iflag_4_composit_buo .gt. id_turn_OFF) then
        if(itype_SGS_m_flux_coef .eq. 1) then
          call modify_cmpCsim_by_SGS_buo_ele                            &
     &       (ifld_sgs%i_comp_buoyancy, icomp_sgs%i_mom_flux,           &
     &        layer_egrp%num_grp, layer_egrp%num_item,                  &
     &        layer_egrp%istack_grp_smp, layer_egrp%item_grp,           &
     &        ele%numele, sgs_coefs%num_field, sgs_coefs%ntot_comp,     &
     &        wk_sgs%fld_coef, wk_sgs%comp_clip, sgs_coefs%ak)
        else
          call modify_fldCsim_by_SGS_buo_ele(ifld_sgs%i_comp_buoyancy,  &
     &       ifld_sgs%i_mom_flux, icomp_sgs%i_mom_flux,                 &
     &        layer_egrp%num_grp, layer_egrp%num_item,                  &
     &        layer_egrp%istack_grp_smp, layer_egrp%item_grp,           &
     &        ele%numele, sgs_coefs%num_field, sgs_coefs%ntot_comp,     &
     &        wk_sgs%fld_coef, wk_sgs%fld_clip, sgs_coefs%ak)
        end if
      end if
!
      end subroutine mod_Csim_by_SGS_buoyancy_ele
!
!  ---------------------------------------------------------------------
!  ---------------------------------------------------------------------
!
      subroutine modify_fldCsim_by_SGS_buo_ele                          &
     &         (iak_sgs_buo, iak_sgs_mf, icomp_sgs_mf,                  &
     &          n_layer_d, n_item_layer_d, layer_stack_smp, item_layer, &
     &          numele, num_kinds_ele, ntot_comp_ele,                   &
     &          sgs_f_coef, sgs_f_clip, ak_sgs)
!
      use m_machine_parameter
!
      integer (kind = kint), intent(in) :: iak_sgs_buo
      integer (kind = kint), intent(in) :: icomp_sgs_mf, iak_sgs_mf
      integer (kind = kint), intent(in) :: n_layer_d, n_item_layer_d
      integer (kind = kint), intent(in)                                 &
     &                      :: layer_stack_smp(0:n_layer_d*np_smp)
      integer (kind = kint), intent(in) :: item_layer(n_item_layer_d)
      integer (kind = kint), intent(in) :: numele
      integer (kind = kint), intent(in) :: num_kinds_ele, ntot_comp_ele
      real(kind = kreal), intent(in)                                    &
     &          :: sgs_f_coef(n_layer_d,num_kinds_ele)
      real(kind = kreal), intent(in)                                    &
     &          :: sgs_f_clip(n_layer_d,num_kinds_ele)
!
      real(kind = kreal), intent(inout) :: ak_sgs(numele,ntot_comp_ele)
!
      integer (kind = kint) :: ip, is, ist, ied, nst, ned
      integer (kind = kint) :: igrp, iele0, iele, nd
!
!
      nst = icomp_sgs_mf
      ned = icomp_sgs_mf + n_sym_tensor - 1
!
!$omp parallel do private(is,ist,ied,igrp,iele0,iele)
        do ip = 1, np_smp
          do nd = nst, ned
            do igrp = 1, n_layer_d
              is = (igrp-1)*np_smp + ip
              ist = layer_stack_smp(is-1) + 1
              ied = layer_stack_smp(is  )
!
!cdir nodep
              do iele0 = ist, ied
                iele = item_layer(iele0)
                ak_sgs(iele,nd) =  sgs_f_clip(igrp,iak_sgs_mf)          &
                                 * (one + sgs_f_coef(igrp,iak_sgs_buo))
              end do
            end do
          end do
        end do
!$omp end parallel do
!
      end subroutine modify_fldCsim_by_SGS_buo_ele
!
!  ---------------------------------------------------------------------
!
      subroutine modify_fldCsim_by_SGS_dbuo_ele                         &
     &         (iak_sgs_cbuo, iak_sgs_tbuo, iak_sgs_mf, icomp_sgs_mf,   &
     &          n_layer_d, n_item_layer_d, layer_stack_smp, item_layer, &
     &          numele, num_kinds_ele, ntot_comp_ele,                   &
     &          sgs_f_coef, sgs_f_clip, ak_sgs)
!
      use m_machine_parameter
!
      integer (kind = kint), intent(in) :: n_layer_d, n_item_layer_d
      integer (kind = kint), intent(in)                                 &
     &                      :: layer_stack_smp(0:n_layer_d*np_smp)
      integer (kind = kint), intent(in) :: item_layer(n_item_layer_d)
      integer (kind = kint), intent(in) :: numele
      integer (kind = kint), intent(in) :: num_kinds_ele, ntot_comp_ele
      integer (kind = kint), intent(in) :: iak_sgs_cbuo, iak_sgs_tbuo
      integer (kind = kint), intent(in) :: iak_sgs_mf, icomp_sgs_mf
      real(kind = kreal), intent(in)                                    &
     &          :: sgs_f_coef(n_layer_d,num_kinds_ele)
      real(kind = kreal), intent(in)                                    &
     &          :: sgs_f_clip(n_layer_d,num_kinds_ele)
!
      real(kind = kreal), intent(inout) :: ak_sgs(numele,ntot_comp_ele)
!
      integer (kind = kint) :: ip, is, ist, ied, nst, ned
      integer (kind = kint) :: igrp, iele0, iele, nd
!
!
      nst = icomp_sgs_mf
      ned = icomp_sgs_mf + n_sym_tensor - 1
!
!$omp parallel do private(is,ist,ied,igrp,iele0,iele)
        do ip = 1, np_smp
          do nd = nst, ned
            do igrp = 1, n_layer_d
              is = (igrp-1)*np_smp + ip
              ist = layer_stack_smp(is-1) + 1
              ied = layer_stack_smp(is  )
!
!cdir nodep
              do iele0 = ist, ied
                iele = item_layer(iele0)
                ak_sgs(iele,nd) =  sgs_f_clip(igrp,iak_sgs_mf)          &
                                 * (one + sgs_f_coef(igrp,iak_sgs_tbuo) &
     &                            + sgs_f_coef(igrp,iak_sgs_cbuo) )
              end do
            end do
          end do
        end do
!$omp end parallel do
!
      end subroutine modify_fldCsim_by_SGS_dbuo_ele
!
!  ---------------------------------------------------------------------
!
      subroutine modify_cmpCsim_by_SGS_buo_ele                          &
     &         (iak_sgs_buo, icomp_sgs_mf,                              &
     &          n_layer_d, n_item_layer_d, layer_stack_smp, item_layer, &
     &          numele, num_kinds_ele, ntot_comp_ele,                   &
     &          sgs_f_coef, sgs_c_clip, ak_sgs)
!
      use m_machine_parameter
!
      integer (kind = kint), intent(in) :: n_layer_d, n_item_layer_d
      integer (kind = kint), intent(in)                                 &
     &                      :: layer_stack_smp(0:n_layer_d*np_smp)
      integer (kind = kint), intent(in) :: item_layer(n_item_layer_d)
      integer (kind = kint), intent(in) :: iak_sgs_buo
      integer (kind = kint), intent(in) :: numele
      integer (kind = kint), intent(in) :: num_kinds_ele, ntot_comp_ele
      integer (kind = kint), intent(in) :: icomp_sgs_mf
      real(kind = kreal), intent(in)                                    &
     &          :: sgs_f_coef(n_layer_d,num_kinds_ele)
      real(kind = kreal), intent(in)                                    &
     &          :: sgs_c_clip(n_layer_d,ntot_comp_ele)
!
      real(kind = kreal), intent(inout) :: ak_sgs(numele,ntot_comp_ele)
!
      integer (kind = kint) :: ip, is, ist, ied, nst, ned
      integer (kind = kint) :: igrp, iele0, iele, nd
!
!
      nst = icomp_sgs_mf
      ned = icomp_sgs_mf + n_sym_tensor - 1
!
!$omp parallel do private(is,ist,ied,igrp,iele0,iele)
        do ip = 1, np_smp
          do nd = nst, ned
            do igrp = 1, n_layer_d
              is = (igrp-1)*np_smp + ip
              ist = layer_stack_smp(is-1) + 1
              ied = layer_stack_smp(is  )
!
!cdir nodep
              do iele0 = ist, ied
                iele = item_layer(iele0)
                ak_sgs(iele,nd) =  sgs_c_clip(igrp,nd)                  &
                                 * (one + sgs_f_coef(igrp,iak_sgs_buo))
              end do
            end do
          end do
        end do
!$omp end parallel do
!
      end subroutine modify_cmpCsim_by_SGS_buo_ele
!
!  ---------------------------------------------------------------------
!
      subroutine modify_cmpCsim_by_SGS_dbuo_ele                         &
     &         (iak_sgs_cbuo, iak_sgs_tbuo, icomp_sgs_mf,               &
     &          n_layer_d, n_item_layer_d, layer_stack_smp, item_layer, &
     &          numele, num_kinds_ele, ntot_comp_ele,                   &
     &          sgs_f_coef, sgs_c_clip, ak_sgs)
!
      use m_machine_parameter
!
      integer (kind = kint), intent(in) :: n_layer_d, n_item_layer_d
      integer (kind = kint), intent(in)                                 &
     &                      :: layer_stack_smp(0:n_layer_d*np_smp)
      integer (kind = kint), intent(in) :: item_layer(n_item_layer_d)
      integer (kind = kint), intent(in) :: numele
      integer (kind = kint), intent(in) :: num_kinds_ele, ntot_comp_ele
      integer (kind = kint), intent(in) :: iak_sgs_cbuo, iak_sgs_tbuo
      integer (kind = kint), intent(in) :: icomp_sgs_mf
      real(kind = kreal), intent(in)                                    &
     &          :: sgs_f_coef(n_layer_d,num_kinds_ele)
      real(kind = kreal), intent(in)                                    &
     &          :: sgs_c_clip(n_layer_d,ntot_comp_ele)
!
      real(kind = kreal), intent(inout) :: ak_sgs(numele,ntot_comp_ele)
!
      integer (kind = kint) :: ip, is, ist, ied, nst, ned
      integer (kind = kint) :: igrp, iele0, iele, nd
!
!
      nst = icomp_sgs_mf
      ned = icomp_sgs_mf + n_sym_tensor - 1
!
!$omp parallel do private(is,ist,ied,igrp,iele0,iele)
        do ip = 1, np_smp
          do nd = nst, ned
            do igrp = 1, n_layer_d
              is = (igrp-1)*np_smp + ip
              ist = layer_stack_smp(is-1) + 1
              ied = layer_stack_smp(is  )
!
!cdir nodep
              do iele0 = ist, ied
                iele = item_layer(iele0)
                ak_sgs(iele,nd) =  sgs_c_clip(igrp,nd)                  &
                                 * (one + sgs_f_coef(igrp,iak_sgs_tbuo) &
     &                            + sgs_f_coef(igrp,iak_sgs_cbuo) )
              end do
            end do
          end do
        end do
!$omp end parallel do
!
      end subroutine modify_cmpCsim_by_SGS_dbuo_ele
!
!  ---------------------------------------------------------------------
!
      end module modify_Csim_by_SGS_buo_ele
