!modify_Csim_by_SGS_buo_ele.f90
!      module modify_Csim_by_SGS_buo_ele
!
!      written by H. Matsui on Aug., 2007
!
!!      subroutine mod_Csim_by_SGS_buoyancy_ele                         &
!!     &         (SGS_param, ele, layer_egrp, fl_prop,                  &
!!     &          iak_sgs_t_buo, iak_sgs_c_buo, wk_sgs, Csim_SGS_mf)
!!        type(SGS_model_control_params), intent(in) :: SGS_param
!!        type(element_data), intent(in) :: ele
!!        type(group_data), intent(in) :: layer_egrp
!!        type(fluid_property), intent(in) :: fl_prop
!!        type(dynamic_model_data), intent(in) :: wk_sgs
!!        type(SGS_model_coefficient), intent(inout) :: Csim_SGS_mf
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
      subroutine mod_Csim_by_SGS_buoyancy_ele                           &
     &         (SGS_param, ele, layer_egrp, fl_prop,                    &
     &          iak_sgs_t_buo, iak_sgs_c_buo, wk_sgs, Csim_SGS_mf)
!
      use t_SGS_control_parameter
      use t_geometry_data
      use t_group_data
      use t_physical_property
      use t_material_property
      use t_SGS_term_labels
      use t_FEM_SGS_model_coefs
      use t_ele_info_4_dynamic
      use set_sgs_diff_model_coefs
!
      integer(kind = kint), intent(in) :: iak_sgs_t_buo
      integer(kind = kint), intent(in) :: iak_sgs_c_buo
!
      type(SGS_model_control_params), intent(in) :: SGS_param
      type(element_data), intent(in) :: ele
      type(group_data), intent(in) :: layer_egrp
      type(fluid_property), intent(in) :: fl_prop
      type(dynamic_model_data), intent(in) :: wk_sgs
!
      type(SGS_model_coefficient), intent(inout) :: Csim_SGS_mf
!
!
      call clear_model_coefs_2_ele(ele, Csim_SGS_mf)
!
      if(fl_prop%iflag_4_gravity                                        &
     &     .and. fl_prop%iflag_4_composit_buo) then
        if(SGS_param%SGS_momentum%itype_Csym_flux                       &
     &      .eq. id_CSIM_COMPONENT) then
          call modify_cmpCsim_by_SGS_dbuo_ele(n_sym_tensor, ele%numele, &
     &        layer_egrp%num_grp, layer_egrp%num_item,                  &
     &        layer_egrp%istack_grp_smp, layer_egrp%item_grp,           &
     &        wk_sgs%fld_coef(1,iak_sgs_t_buo),                         &
     &        wk_sgs%fld_coef(1,iak_sgs_c_buo),                         &
     &        wk_sgs%comp_clip(1,Csim_SGS_mf%icomp_Csim),               &
     &        Csim_SGS_mf%coef(1,1))
        else
          call modify_fldCsim_by_SGS_dbuo_ele(n_sym_tensor, ele%numele, &
     &        layer_egrp%num_grp, layer_egrp%num_item,                  &
     &        layer_egrp%istack_grp_smp, layer_egrp%item_grp,           &
     &        wk_sgs%fld_coef(1,iak_sgs_t_buo),                         &
     &        wk_sgs%fld_coef(1,iak_sgs_c_buo),                         &
     &        wk_sgs%fld_clip(1,Csim_SGS_mf%iak_Csim),                  &
     &        Csim_SGS_mf%coef(1,1))
        end if
      else if(fl_prop%iflag_4_gravity) then
        if(SGS_param%SGS_momentum%itype_Csym_flux                       &
     &      .eq. id_CSIM_COMPONENT) then
          call modify_cmpCsim_by_SGS_buo_ele(n_sym_tensor, ele%numele,  &
     &        layer_egrp%num_grp, layer_egrp%num_item,                  &
     &        layer_egrp%istack_grp_smp, layer_egrp%item_grp,           &
     &        wk_sgs%fld_coef(1,iak_sgs_t_buo),                         &
     &        wk_sgs%comp_clip(1,Csim_SGS_mf%icomp_Csim),               &
     &        Csim_SGS_mf%coef(1,1))
        else
          call modify_fldCsim_by_SGS_buo_ele(n_sym_tensor, ele%numele,  &
     &        layer_egrp%num_grp, layer_egrp%num_item,                  &
     &        layer_egrp%istack_grp_smp, layer_egrp%item_grp,           &
     &        wk_sgs%fld_coef(1,iak_sgs_t_buo),                         &
     &        wk_sgs%fld_clip(1,Csim_SGS_mf%iak_Csim),                  &
     &        Csim_SGS_mf%coef(1,1))
        end if
      else if(fl_prop%iflag_4_composit_buo) then
        if(SGS_param%SGS_momentum%itype_Csym_flux                       &
     &      .eq. id_CSIM_COMPONENT) then
          call modify_cmpCsim_by_SGS_buo_ele(n_sym_tensor, ele%numele,  &
     &        layer_egrp%num_grp, layer_egrp%num_item,                  &
     &        layer_egrp%istack_grp_smp, layer_egrp%item_grp,           &
     &        wk_sgs%fld_coef(1,iak_sgs_c_buo),                         &
     &        wk_sgs%comp_clip(1,Csim_SGS_mf%icomp_Csim),               &
     &        Csim_SGS_mf%coef(1,1))
        else
          call modify_fldCsim_by_SGS_buo_ele(n_sym_tensor, ele%numele,  &
     &        layer_egrp%num_grp, layer_egrp%num_item,                  &
     &        layer_egrp%istack_grp_smp, layer_egrp%item_grp,           &
     &        wk_sgs%fld_coef(1,iak_sgs_c_buo),                         &
     &        wk_sgs%fld_clip(1,Csim_SGS_mf%iak_Csim),                  &
     &        Csim_SGS_mf%coef(1,1))
        end if
      end if
!
      end subroutine mod_Csim_by_SGS_buoyancy_ele
!
!  ---------------------------------------------------------------------
!  ---------------------------------------------------------------------
!
      subroutine modify_fldCsim_by_SGS_buo_ele(numdir, numele,          &
     &          n_layer_d, n_item_layer_d, layer_stack_smp, item_layer, &
     &          sgs_f_coef, sgs_f_clip, ak_sgs)
!
      use m_machine_parameter
!
      integer (kind = kint), intent(in) :: n_layer_d, n_item_layer_d
      integer (kind = kint), intent(in)                                 &
     &                      :: layer_stack_smp(0:n_layer_d*np_smp)
      integer (kind = kint), intent(in) :: item_layer(n_item_layer_d)
      integer (kind = kint), intent(in) :: numele, numdir
      real(kind = kreal), intent(in) :: sgs_f_coef(n_layer_d)
      real(kind = kreal), intent(in) :: sgs_f_clip(n_layer_d)
!
      real(kind = kreal), intent(inout) :: ak_sgs(numele,numdir)
!
      integer (kind = kint) :: ip, is, ist, ied
      integer (kind = kint) :: igrp, iele0, iele, nd
!
!
!$omp parallel do private(is,ist,ied,igrp,iele0,iele)
        do ip = 1, np_smp
          do nd = 1, numdir
            do igrp = 1, n_layer_d
              is = (igrp-1)*np_smp + ip
              ist = layer_stack_smp(is-1) + 1
              ied = layer_stack_smp(is  )
!
!cdir nodep
              do iele0 = ist, ied
                iele = item_layer(iele0)
                ak_sgs(iele,nd) =  sgs_f_clip(igrp)                     &
                                 * (one + sgs_f_coef(igrp))
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
      subroutine modify_fldCsim_by_SGS_dbuo_ele(numdir, numele,         &
     &          n_layer_d, n_item_layer_d, layer_stack_smp, item_layer, &
     &          sgs_f_tbuo, sgs_f_cbuo, sgs_f_clip, ak_sgs)
!
      use m_machine_parameter
!
      integer (kind = kint), intent(in) :: n_layer_d, n_item_layer_d
      integer (kind = kint), intent(in)                                 &
     &                      :: layer_stack_smp(0:n_layer_d*np_smp)
      integer (kind = kint), intent(in) :: item_layer(n_item_layer_d)
      integer (kind = kint), intent(in) :: numele, numdir
      real(kind = kreal), intent(in) :: sgs_f_tbuo(n_layer_d)
      real(kind = kreal), intent(in) :: sgs_f_cbuo(n_layer_d)
      real(kind = kreal), intent(in) :: sgs_f_clip(n_layer_d)
!
      real(kind = kreal), intent(inout) :: ak_sgs(numele,numdir)
!
      integer (kind = kint) :: ip, is, ist, ied
      integer (kind = kint) :: igrp, iele0, iele, nd
!
!
!$omp parallel do private(is,ist,ied,igrp,iele0,iele)
        do ip = 1, np_smp
          do nd = 1, numdir
            do igrp = 1, n_layer_d
              is = (igrp-1)*np_smp + ip
              ist = layer_stack_smp(is-1) + 1
              ied = layer_stack_smp(is  )
!
!cdir nodep
              do iele0 = ist, ied
                iele = item_layer(iele0)
                ak_sgs(iele,nd) =  sgs_f_clip(igrp)                     &
                                 * (one + sgs_f_tbuo(igrp)              &
     &                                  + sgs_f_cbuo(igrp) )
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
      subroutine modify_cmpCsim_by_SGS_buo_ele(numdir, numele,          &
     &          n_layer_d, n_item_layer_d, layer_stack_smp, item_layer, &
     &          sgs_f_coef, sgs_c_clip, ak_sgs)
!
      use m_machine_parameter
!
      integer (kind = kint), intent(in) :: n_layer_d, n_item_layer_d
      integer (kind = kint), intent(in)                                 &
     &                      :: layer_stack_smp(0:n_layer_d*np_smp)
      integer (kind = kint), intent(in) :: item_layer(n_item_layer_d)
      integer (kind = kint), intent(in) :: numele, numdir
      real(kind = kreal), intent(in) :: sgs_f_coef(n_layer_d)
      real(kind = kreal), intent(in) :: sgs_c_clip(n_layer_d,numdir)
!
      real(kind = kreal), intent(inout) :: ak_sgs(numele,numdir)
!
      integer (kind = kint) :: ip, is, ist, ied
      integer (kind = kint) :: igrp, iele0, iele, nd
!
!
!$omp parallel do private(is,ist,ied,igrp,iele0,iele)
        do ip = 1, np_smp
          do nd = 1, numdir
            do igrp = 1, n_layer_d
              is = (igrp-1)*np_smp + ip
              ist = layer_stack_smp(is-1) + 1
              ied = layer_stack_smp(is  )
!
!cdir nodep
              do iele0 = ist, ied
                iele = item_layer(iele0)
                ak_sgs(iele,nd) =  sgs_c_clip(igrp,nd)                  &
                                 * (one + sgs_f_coef(igrp))
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
      subroutine modify_cmpCsim_by_SGS_dbuo_ele(numdir, numele,         &
     &          n_layer_d, n_item_layer_d, layer_stack_smp, item_layer, &
     &          sgs_tbuo_coef, sgs_cbuo_coef, sgs_c_clip, ak_sgs)
!
      use m_machine_parameter
!
      integer (kind = kint), intent(in) :: n_layer_d, n_item_layer_d
      integer (kind = kint), intent(in)                                 &
     &                      :: layer_stack_smp(0:n_layer_d*np_smp)
      integer (kind = kint), intent(in) :: item_layer(n_item_layer_d)
      integer (kind = kint), intent(in) :: numele, numdir
      real(kind = kreal), intent(in) :: sgs_tbuo_coef(n_layer_d)
      real(kind = kreal), intent(in) :: sgs_cbuo_coef(n_layer_d)
      real(kind = kreal), intent(in) :: sgs_c_clip(n_layer_d,numdir)
!
      real(kind = kreal), intent(inout) :: ak_sgs(numele,numdir)
!
      integer (kind = kint) :: ip, is, ist, ied
      integer (kind = kint) :: igrp, iele0, iele, nd
!
!
!$omp parallel do private(is,ist,ied,igrp,iele0,iele)
        do ip = 1, np_smp
          do nd = 1, n_sym_tensor
            do igrp = 1, n_layer_d
              is = (igrp-1)*np_smp + ip
              ist = layer_stack_smp(is-1) + 1
              ied = layer_stack_smp(is  )
!
!cdir nodep
              do iele0 = ist, ied
                iele = item_layer(iele0)
                ak_sgs(iele,nd) =  sgs_c_clip(igrp,nd)                  &
                                 * (one + sgs_tbuo_coef(igrp)           &
     &                                  + sgs_cbuo_coef(igrp) )
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
