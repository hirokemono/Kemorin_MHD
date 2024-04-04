!>@file   set_sgs_diff_model_coefs.f90
!!@brief  module set_sgs_diff_model_coefs
!!
!!@author  H.Matsui and H.Okuda
!!@date   Programmed in Oct., 2005
!!@n      Modified in July, 2007
!!@n      Modified in Nov., 2009
!
!>@brief Set model coefficients for scalars
!!
!!@verbatim
!!      subroutine clippging_sgs_coefs                                  &
!!     &         (iflag_SGS_initial, SGS_param, numdir, nlayer_SGS,     &
!!     &          sgs_f_coef, sgs_c_coef, sgs_f_whole, sgs_c_whole,     &
!!     &          sgs_f_clip, sgs_c_clip, sgs_fw_clip, sgs_cw_clip)
!!        integer(kind = kint), intent(in) :: iflag_SGS_initial
!!        type(SGS_model_control_params), intent(in) :: SGS_param
!!        integer(kind = kint), intent(in) :: numdir
!!        integer (kind = kint), intent(in) :: nlayer_SGS
!!        real(kind = kreal), intent(in)  :: sgs_f_coef(nlayer_SGS)
!!        real(kind = kreal), intent(in) :: sgs_c_coef(nlayer_SGS,numdir)
!!        real(kind = kreal), intent(in) :: sgs_f_whole
!!        real(kind = kreal), intent(in) :: sgs_c_whole(numdir)
!!        real(kind = kreal), intent(inout):: sgs_f_clip(nlayer_SGS)
!!        real(kind = kreal), intent(inout):: sgs_c_clip(nlayer_SGS,numdir)
!!        real(kind = kreal), intent(inout):: sgs_fw_clip
!!        real(kind = kreal), intent(inout):: sgs_cw_clip(numdir)
!
!!
!!      subroutine clear_model_coefs_2_ele(ele, Csim)
!!      subroutine sel_model_coefs_2_ele(ele, layer_egrp, itype_csim,   &
!!     &                                 wk_sgs, Csim)
!!        type(element_data), intent(in) :: ele
!!        type(group_data), intent(in) :: layer_egrp
!!       integer (kind = kint), intent(in) :: itype_csim
!!        type(dynamic_model_data), intent(in) :: wk_sgs
!!        type(SGS_model_coefficient), intent(inout) :: Csim
!!
!!      subroutine set_model_coefs_2_ele(ele, layer_egrp, itype_csim,   &
!!     &          numdir, sgs_f_clip, sgs_c_clip, ak_sgs)
!!        type(element_data), intent(in) :: ele
!!        type(group_data), intent(in) :: layer_egrp
!!      subroutine set_diff_coefs_layer_ele                             &
!!     &         (ele, n_layer_d, n_item_layer_d, layer_stack_smp,      &
!!     &          item_layer, diff_f_clip, ak_diff)
!!      subroutine set_diff_coefs_whole_ele(ele, iele_fsmp_stack,       &
!!     &                                    diff_f_whole_clip, ak_diff)
!!@endverbatim
!
      module set_sgs_diff_model_coefs
!
      use m_precision
      use m_machine_parameter
!
      use m_constants
      use t_geometry_data
      use t_group_data
      use t_FEM_SGS_model_coefs
!
      implicit none
!
      private :: delete_negative_coefs, ignore_negative_coefs
      private :: init_negative_coefs, copy_sgs_coefs
      private :: each_comps_model_coefs_2_ele, field_model_coefs_2_ele
!
!  ---------------------------------------------------------------------
!
      contains
!
!  ---------------------------------------------------------------------
!
      subroutine clippging_sgs_coefs                                    &
     &         (iflag_SGS_initial, SGS_param, numdir, nlayer_SGS,       &
     &          sgs_f_coef, sgs_c_coef, sgs_f_whole, sgs_c_whole,       &
     &          sgs_f_clip, sgs_c_clip, sgs_fw_clip, sgs_cw_clip)
!
      use calypso_mpi
      use t_SGS_control_parameter
!
      integer(kind = kint), intent(in) :: iflag_SGS_initial
      type(SGS_model_control_params), intent(in) :: SGS_param
!
      integer(kind = kint), intent(in) :: numdir
      integer (kind = kint), intent(in) :: nlayer_SGS
!
      real(kind = kreal), intent(in)  :: sgs_f_coef(nlayer_SGS)
      real(kind = kreal), intent(in) :: sgs_c_coef(nlayer_SGS,numdir)
      real(kind = kreal), intent(in) :: sgs_f_whole
      real(kind = kreal), intent(in) :: sgs_c_whole(numdir)
!
      real(kind = kreal), intent(inout):: sgs_f_clip(nlayer_SGS)
      real(kind = kreal), intent(inout):: sgs_c_clip(nlayer_SGS,numdir)
      real(kind = kreal), intent(inout):: sgs_fw_clip
      real(kind = kreal), intent(inout):: sgs_cw_clip(numdir)
!
      integer (kind = kint) :: nd
!
!
      if (SGS_param%iflag_nagetive_clip .eq. id_SGS_ZERO_CLIP) then
        do nd = 1, numdir
          call delete_negative_coefs                                    &
     &       (nlayer_SGS, SGS_param%clipping_limit,                     &
     &        sgs_c_coef(1,nd), sgs_c_whole(nd),                        &
     &        sgs_c_clip(1,nd), sgs_cw_clip(nd))
        end do
          call delete_negative_coefs                                    &
     &       (nlayer_SGS, SGS_param%clipping_limit,                     &
     &        sgs_f_coef(1), sgs_f_whole, sgs_f_clip(1), sgs_fw_clip)
!
!
      else if(SGS_param%iflag_nagetive_clip .eq. id_SGS_KEEP_PREVIOUS) &
     & then
        do nd = 1, numdir
          call ignore_negative_coefs                                    &
     &       (nlayer_SGS, SGS_param%clipping_limit,                     &
     &        sgs_c_coef(1,nd), sgs_c_whole(nd),                        &
     &        sgs_c_clip(1,nd), sgs_cw_clip(nd))
        end do
          call ignore_negative_coefs                                    &
     &       (nlayer_SGS, SGS_param%clipping_limit,                     &
     &        sgs_f_coef(1), sgs_f_whole, sgs_f_clip(1), sgs_fw_clip)
!
        if (iflag_SGS_initial .eq. 1) then
          do nd = 1, numdir
            call init_negative_coefs                                    &
     &         (nlayer_SGS, SGS_param%clipping_limit,                   &
     &          sgs_c_coef(1,nd), sgs_c_whole(nd),                      &
     &          sgs_c_clip(1,nd), sgs_cw_clip(nd))
          end do
            call init_negative_coefs                                    &
     &         (nlayer_SGS, SGS_param%clipping_limit,                   &
     &          sgs_c_coef(1,nd), sgs_c_whole(nd),                      &
     &          sgs_c_clip(1,nd), sgs_cw_clip(nd))
        end if
!
      else
!
        do nd = 1, numdir
          call copy_sgs_coefs(nlayer_SGS,                               &
     &      sgs_c_coef(1,nd), sgs_c_whole(nd),                          &
     &      sgs_c_clip(1,nd), sgs_cw_clip(nd))
        end do
        call copy_sgs_coefs(nlayer_SGS,                                 &
     &       sgs_f_coef(1), sgs_f_whole, sgs_f_clip(1), sgs_fw_clip)
      end if
!
      end subroutine clippging_sgs_coefs
!
!  ---------------------------------------------------------------------
!  ---------------------------------------------------------------------
!
      subroutine copy_sgs_coefs(n_layer_d, coef, coef_w,                &
     &          clipped, clipped_w)
!
      integer (kind = kint), intent(in) :: n_layer_d
      real (kind = kreal), intent(in) :: coef(n_layer_d)
      real (kind = kreal), intent(in) :: coef_w
!
      real (kind = kreal), intent(inout) :: clipped(n_layer_d)
      real (kind = kreal), intent(inout) :: clipped_w
!
      integer(kind = kint) :: inum
!
!
      do inum = 1, n_layer_d
        clipped(inum) = coef(inum)
      end do
      clipped_w = coef_w
!
      end subroutine copy_sgs_coefs
!
!  ---------------------------------------------------------------------
!
      subroutine delete_negative_coefs(nlayer_SGS, clip_level,          &
     &          coef, coef_w, clipped, clipped_w)
!
      integer (kind = kint), intent(in) :: nlayer_SGS
      real (kind = kreal), intent(in) :: clip_level
      real (kind = kreal), intent(in) :: coef(nlayer_SGS)
      real (kind = kreal), intent(in) :: coef_w
!
      real (kind = kreal), intent(inout) :: clipped(nlayer_SGS)
      real (kind = kreal), intent(inout) :: clipped_w
!
      integer (kind = kint) :: inum
!
!
      do inum = 1, nlayer_SGS
        if (coef(inum) .lt. clip_level) then
          clipped(inum) = zero
        else
          clipped(inum) = coef(inum)
        end if
      end do
      if (coef_w .lt. clip_level) then
        clipped_w = zero
      else
        clipped_w = coef_w
      end if
!
      end subroutine delete_negative_coefs
!
!  ---------------------------------------------------------------------
!
      subroutine ignore_negative_coefs(nlayer_SGS, clip_level,          &
     &          coef, coef_w, clipped, clipped_w)
!
      integer (kind = kint), intent(in) :: nlayer_SGS
      real (kind = kreal), intent(in) :: clip_level
      real (kind = kreal), intent(in) :: coef(nlayer_SGS)
      real (kind = kreal), intent(in) :: coef_w
!
      real (kind = kreal), intent(inout) :: clipped(nlayer_SGS)
      real (kind = kreal), intent(inout) :: clipped_w
!
      integer (kind = kint) :: inum
!
        do inum = 1, nlayer_SGS
          if (coef(inum) .gt. clip_level) clipped(inum) = coef(inum)
        end do
        if (coef_w .gt. clip_level) clipped_w = coef_w
!
      end subroutine ignore_negative_coefs
!
!  ---------------------------------------------------------------------
!
      subroutine init_negative_coefs(nlayer_SGS, clip_level,            &
     &          coef, coef_w, clipped, clipped_w)
!
      integer (kind = kint), intent(in) :: nlayer_SGS
      real (kind = kreal), intent(in) :: clip_level
      real (kind = kreal), intent(in) :: coef(nlayer_SGS)
      real (kind = kreal), intent(in) :: coef_w
!
      real (kind = kreal), intent(inout) :: clipped(nlayer_SGS)
      real (kind = kreal), intent(inout) :: clipped_w
!
      integer(kind = kint) :: inum
!
      if (coef_w .lt. clip_level)  clipped_w = zero
      do inum = 1, nlayer_SGS
        if (coef(inum) .lt. clip_level) clipped(inum) = clipped_w
      end do
!
      end subroutine init_negative_coefs
!
!  ---------------------------------------------------------------------
!
      subroutine clear_model_coefs_2_ele(ele, Csim)
!
      use delete_field_smp
!
      type(element_data), intent(in) :: ele
      type(SGS_model_coefficient), intent(inout) :: Csim
!
!
!$omp parallel
      call delete_phys_data_smp(ele%numele, ione, ele%numele,           &
     &    Csim%num_comp, Csim%num_comp, ione, Csim%coef(1,1))
!$omp end parallel
!
      end subroutine clear_model_coefs_2_ele
!
!  ---------------------------------------------------------------------
!
      subroutine sel_model_coefs_2_ele(ele, layer_egrp, itype_csim,     &
     &                                 wk_sgs, Csim)
!
      use t_ele_info_4_dynamic
!
      type(element_data), intent(in) :: ele
      type(group_data), intent(in) :: layer_egrp
      integer (kind = kint), intent(in) :: itype_csim
      type(dynamic_model_data), intent(in) :: wk_sgs
!
      type(SGS_model_coefficient), intent(inout) :: Csim
!
!
      if(Csim%num_comp .le. 0) return
      call clear_model_coefs_2_ele(ele, Csim)
!
      if(itype_csim .eq. 1) then
        call each_comps_model_coefs_2_ele                               &
     &     (ele, layer_egrp, Csim%num_comp,                             &
     &      wk_sgs%comp_clip(1,Csim%icomp_Csim), Csim%coef(1,1))
      else
        call field_model_coefs_2_ele(ele, layer_egrp, Csim%num_comp,    &
     &      wk_sgs%fld_clip(1,Csim%iak_Csim), Csim%coef(1,1))
      end if
!
      end subroutine sel_model_coefs_2_ele
!
!  ---------------------------------------------------------------------
!  ---------------------------------------------------------------------
!
      subroutine each_comps_model_coefs_2_ele(ele, layer_egrp,          &
     &          numdir, sgs_c_clip, ak_sgs)
!
      type(element_data), intent(in) :: ele
      type(group_data), intent(in) :: layer_egrp
      integer (kind = kint), intent(in) :: numdir
      real(kind = kreal), intent(in)                                    &
     &                   :: sgs_c_clip(layer_egrp%num_item,numdir)
!
      real(kind = kreal), intent(inout) :: ak_sgs(ele%numele,numdir)
!
      integer (kind = kint) :: ip, is, ist, ied
      integer (kind = kint) :: inum, iele0, iele, nd
!
!
!$omp parallel do private(is,ist,ied,inum,iele0,iele)
        do ip = 1, np_smp
          do nd = 1, numdir
            do inum = 1, layer_egrp%num_grp
              is = (inum-1)*np_smp + ip
              ist = layer_egrp%istack_grp_smp(is-1) + 1
              ied = layer_egrp%istack_grp_smp(is  )
!
!cdir nodep
              do iele0 = ist, ied
                iele = layer_egrp%item_grp(iele0)
                ak_sgs(iele,nd) = sgs_c_clip(inum,nd)
              end do
            end do
          end do
        end do
!$omp end parallel do
!
      end subroutine each_comps_model_coefs_2_ele
!
!  ---------------------------------------------------------------------
!
      subroutine field_model_coefs_2_ele(ele, layer_egrp,               &
     &          numdir, sgs_f_clip, ak_sgs)
!
      type(element_data), intent(in) :: ele
      type(group_data), intent(in) :: layer_egrp
      integer (kind = kint), intent(in) :: numdir
      real(kind = kreal), intent(in) :: sgs_f_clip(layer_egrp%num_item)
!
      real(kind = kreal), intent(inout) :: ak_sgs(ele%numele,numdir)
!
      integer (kind = kint) :: ip, is, ist, ied
      integer (kind = kint) :: inum, iele0, iele, nd
!
!
!$omp parallel do private(is,ist,ied,inum,iele0,iele)
        do ip = 1, np_smp
          do nd = 1, numdir
            do inum = 1, layer_egrp%num_grp
              is = (inum-1)*np_smp + ip
              ist = layer_egrp%istack_grp_smp(is-1) + 1
              ied = layer_egrp%istack_grp_smp(is  )
!
!cdir nodep
              do iele0 = ist, ied
                iele = layer_egrp%item_grp(iele0)
                ak_sgs(iele,nd) = sgs_f_clip(inum)
              end do
            end do
          end do
        end do
!$omp end parallel do
!
      end subroutine field_model_coefs_2_ele
!
!  ---------------------------------------------------------------------
!  ---------------------------------------------------------------------
!
      subroutine set_diff_coefs_layer_ele                               &
     &         (ele, n_layer_d, n_item_layer_d, layer_stack_smp,        &
     &          item_layer, diff_f_clip, ak_diff)
!
      type(element_data), intent(in) :: ele
      integer (kind = kint), intent(in) :: n_layer_d, n_item_layer_d
      integer (kind = kint), intent(in)                                 &
     &                      :: layer_stack_smp(0:n_layer_d*np_smp)
      integer (kind = kint), intent(in) :: item_layer(n_item_layer_d)
      real(kind = kreal), intent(in) :: diff_f_clip(n_item_layer_d)
!
      real(kind = kreal), intent(inout) :: ak_diff(ele%numele)
!
      integer (kind = kint) :: ip, is, ist, ied, inum, iele0, iele
!
!
!$omp parallel do private(ist,ied)
      do ip = 1, np_smp
          ist = ele%istack_ele_smp(ip-1) + 1
          ied = ele%istack_ele_smp(ip  )
          ak_diff(ist:ied) = zero
      end do
!$omp end parallel do
!
!$omp parallel do private(is,ist,ied,inum,iele0,iele)
      do ip = 1, np_smp
          do inum = 1, n_layer_d
            is = (inum-1)*np_smp + ip
            ist = layer_stack_smp(is-1) + 1
            ied = layer_stack_smp(is  )
!
!cdir nodep
            do iele0 = ist, ied
              iele = item_layer(iele0)
              ak_diff(iele) = diff_f_clip(inum)
            end do
          end do
      end do
!$omp end parallel do
!
      end subroutine set_diff_coefs_layer_ele
!
!  ---------------------------------------------------------------------
!
      subroutine set_diff_coefs_whole_ele(ele, iele_fsmp_stack,         &
     &                                    diff_f_whole_clip, ak_diff)
!
      type(element_data), intent(in) :: ele
      integer(kind=kint), intent(in) :: iele_fsmp_stack(0:np_smp)
      real(kind = kreal), intent(in) :: diff_f_whole_clip
!
      real(kind = kreal), intent(inout) :: ak_diff(ele%numele)
!
      integer (kind = kint) :: ip, ist, ied
!
!
!$omp parallel do private(ist,ied)
      do ip = 1, np_smp
        ist = ele%istack_ele_smp(ip-1) + 1
        ied = ele%istack_ele_smp(ip  )
        ak_diff(ist:ied) = zero
      end do
!$omp end parallel do
!
!$omp parallel do private(ist,ied)
      do ip = 1, np_smp
        ist = iele_fsmp_stack(ip-1) + 1
        ied = iele_fsmp_stack(ip  )
        ak_diff(ist:ied) = diff_f_whole_clip
      end do
!$omp end parallel do
!
      end subroutine set_diff_coefs_whole_ele
!
!  ---------------------------------------------------------------------
!
      end module set_sgs_diff_model_coefs
