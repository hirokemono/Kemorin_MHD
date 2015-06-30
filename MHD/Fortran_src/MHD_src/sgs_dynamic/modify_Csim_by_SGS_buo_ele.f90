!modify_Csim_by_SGS_buo_ele.f90
!      module modify_Csim_by_SGS_buo_ele
!
!      written by H. Matsui on Aug., 2007
!
!      subroutine mod_Csim_by_SGS_buoyancy_ele
!
      module modify_Csim_by_SGS_buo_ele
!
      use m_precision
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
      subroutine mod_Csim_by_SGS_buoyancy_ele
!
      use m_layering_ele_list
      use m_SGS_address
      use m_control_parameter
      use set_sgs_diff_model_coefs
!
!
      call clear_model_coefs_2_ele(n_sym_tensor, icomp_sgs_mf)
!
      if(iflag_4_gravity .gt. id_turn_OFF                               &
     &     .and. iflag_4_composit_buo .gt. id_turn_OFF) then
        if(itype_SGS_m_flux_coef .eq. 1) then
          call modify_cmpCsim_by_SGS_dbuo_ele                           &
     &       (layer_tbl1%n_layer_d, layer_tbl1%n_item_layer_d,          &
     &        layer_tbl1%layer_stack_smp, layer_tbl1%item_layer)
        else
          call modify_fldCsim_by_SGS_dbuo_ele                           &
     &       (layer_tbl1%n_layer_d, layer_tbl1%n_item_layer_d,          &
     &        layer_tbl1%layer_stack_smp, layer_tbl1%item_layer)
        end if
      else if(iflag_4_gravity .gt. id_turn_OFF) then
        if(itype_SGS_m_flux_coef .eq. 1) then
          call modify_cmpCsim_by_SGS_buo_ele(iak_sgs_tbuo,              &
     &        layer_tbl1%n_layer_d, layer_tbl1%n_item_layer_d,          &
     &        layer_tbl1%layer_stack_smp, layer_tbl1%item_layer)
        else
          call modify_fldCsim_by_SGS_buo_ele(iak_sgs_tbuo,              &
     &        layer_tbl1%n_layer_d, layer_tbl1%n_item_layer_d,          &
     &        layer_tbl1%layer_stack_smp, layer_tbl1%item_layer)
        end if
      else if(iflag_4_composit_buo .gt. id_turn_OFF) then
        if(itype_SGS_m_flux_coef .eq. 1) then
          call modify_cmpCsim_by_SGS_buo_ele(iak_sgs_cbuo,              &
     &        layer_tbl1%n_layer_d, layer_tbl1%n_item_layer_d,          &
     &        layer_tbl1%layer_stack_smp, layer_tbl1%item_layer)
        else
          call modify_fldCsim_by_SGS_buo_ele(iak_sgs_cbuo,              &
     &        layer_tbl1%n_layer_d, layer_tbl1%n_item_layer_d,          &
     &        layer_tbl1%layer_stack_smp, layer_tbl1%item_layer)
        end if
      end if
!
      end subroutine mod_Csim_by_SGS_buoyancy_ele
!
!  ---------------------------------------------------------------------
!  ---------------------------------------------------------------------
!
      subroutine modify_fldCsim_by_SGS_buo_ele(iak_sgs_buo,             &
     &          n_layer_d, n_item_layer_d, layer_stack_smp, item_layer)
!
      use m_machine_parameter
      use m_ele_info_4_dynamical
      use m_SGS_address
      use m_SGS_model_coefs
      use m_work_4_dynamic_model
!
      integer (kind = kint), intent(in) :: iak_sgs_buo
      integer (kind = kint), intent(in) :: n_layer_d, n_item_layer_d
      integer (kind = kint), intent(in)                                 &
     &                      :: layer_stack_smp(0:n_layer_d*np_smp)
      integer (kind = kint), intent(in) :: item_layer(n_item_layer_d)
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
     &         (n_layer_d, n_item_layer_d, layer_stack_smp, item_layer)
!
      use m_machine_parameter
      use m_ele_info_4_dynamical
      use m_SGS_address
      use m_SGS_model_coefs
      use m_work_4_dynamic_model
!
      integer (kind = kint), intent(in) :: n_layer_d, n_item_layer_d
      integer (kind = kint), intent(in)                                 &
     &                      :: layer_stack_smp(0:n_layer_d*np_smp)
      integer (kind = kint), intent(in) :: item_layer(n_item_layer_d)
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
      subroutine modify_cmpCsim_by_SGS_buo_ele(iak_sgs_buo,             &
     &          n_layer_d, n_item_layer_d, layer_stack_smp, item_layer)
!
      use m_machine_parameter
      use m_ele_info_4_dynamical
      use m_SGS_address
      use m_SGS_model_coefs
      use m_work_4_dynamic_model
!
      integer (kind = kint), intent(in) :: n_layer_d, n_item_layer_d
      integer (kind = kint), intent(in)                                 &
     &                      :: layer_stack_smp(0:n_layer_d*np_smp)
      integer (kind = kint), intent(in) :: item_layer(n_item_layer_d)
!
      integer (kind = kint), intent(in) :: iak_sgs_buo
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
     &         (n_layer_d, n_item_layer_d, layer_stack_smp, item_layer)
!
      use m_machine_parameter
      use m_ele_info_4_dynamical
      use m_SGS_address
      use m_SGS_model_coefs
      use m_work_4_dynamic_model
!
      integer (kind = kint), intent(in) :: n_layer_d, n_item_layer_d
      integer (kind = kint), intent(in)                                 &
     &                      :: layer_stack_smp(0:n_layer_d*np_smp)
      integer (kind = kint), intent(in) :: item_layer(n_item_layer_d)
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
