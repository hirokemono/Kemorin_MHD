!
!      module set_sgs_diff_model_coefs
!
!     Written by H. Matsui on Oct., 2005
!     Modified by H. Matsui on July, 2007
!     Modified by H. Matsui on Nov., 2009
!
!!      subroutine clippging_sgs_diff_coefs                             &
!!     &         (numdir, ifield_d, icomp_f, SGS_param, wk_sgs)
!!        type(SGS_model_control_params), intent(in) :: SGS_param
!!        type(dynamic_model_data), intent(inout) :: wk_sgs
!!
!!      subroutine clear_model_coefs_2_ele                              &
!!     &         (ele, numdir, icomp_f, ntot_comp_ele, ak_sgs)
!!        type(dynamic_model_data), intent(inout) :: wk_sgs
!!
!!      subroutine set_model_coefs_2_ele(ele, itype_csim, numdir,       &
!!     &          ifield_d, icomp_f, n_layer_d, n_item_layer_d,         &
!!     &          layer_stack_smp, item_layer, num_kind_ele,            &
!!     &          ntot_comp_ele, sgs_f_clip, sgs_c_clip, ak_sgs)
!!      subroutine set_diff_coefs_layer_ele(ele, ifield_d,              &
!!     &         n_layer_d, n_item_layer_d, layer_stack_smp, item_layer,&
!!     &         ntot_fld_ele, diff_f_clip, ak_diff)
!!      subroutine set_diff_coefs_whole_ele(ele, iele_fsmp_stack,       &
!!     &          ifield_d, ntot_fld_ele, diff_f_whole_clip, ak_diff)
!
      module set_sgs_diff_model_coefs
!
      use m_precision
!
      use m_constants
      use t_geometry_data
!
      implicit none
!
      private :: clippging_sgs_coefs
      private :: delete_negative_coefs, ignore_negative_coefs
      private :: init_negative_coefs, copy_sgs_coefs
!
!  ---------------------------------------------------------------------
!
      contains
!
!  ---------------------------------------------------------------------
!
      subroutine clippging_sgs_diff_coefs                               &
     &         (numdir, ifield_d, icomp_f, SGS_param, wk_sgs)
!
      use t_ele_info_4_dynamic
      use t_SGS_control_parameter
!
      integer(kind = kint), intent(in) :: numdir, ifield_d, icomp_f
      type(SGS_model_control_params), intent(in) :: SGS_param
      type(dynamic_model_data), intent(inout) :: wk_sgs
!
!
      call clippging_sgs_coefs(SGS_param, numdir, ifield_d, icomp_f,    &
     &    wk_sgs%nlayer, wk_sgs%num_kinds, wk_sgs%ntot_comp,            &
     &    wk_sgs%fld_coef, wk_sgs%comp_coef, wk_sgs%fld_whole,          &
     &    wk_sgs%comp_whole, wk_sgs%fld_clip, wk_sgs%comp_clip,         &
     &    wk_sgs%fld_whole_clip, wk_sgs%comp_whole_clip)
!
      end subroutine clippging_sgs_diff_coefs
!
!  ---------------------------------------------------------------------
!  ---------------------------------------------------------------------
!
      subroutine clippging_sgs_coefs                                    &
     &         (SGS_param, numdir, ifield_d, icomp_f,                   &
     &          nlayer_SGS, num_sgs_kinds, num_sgs_coefs,               &
     &          sgs_f_coef, sgs_c_coef, sgs_f_whole, sgs_c_whole,       &
     &          sgs_f_clip, sgs_c_clip, sgs_fw_clip, sgs_cw_clip)
!
      use calypso_mpi
      use m_t_step_parameter
      use t_SGS_control_parameter
!
      type(SGS_model_control_params), intent(in) :: SGS_param
!
      integer(kind = kint), intent(in) :: numdir, ifield_d, icomp_f
      integer (kind = kint), intent(in) :: nlayer_SGS
      integer (kind = kint), intent(in) :: num_sgs_kinds, num_sgs_coefs
!
      real(kind = kreal), intent(in)                                    &
     &          :: sgs_f_coef(nlayer_SGS,num_sgs_kinds)
      real(kind = kreal), intent(in)                                    &
     &          :: sgs_c_coef(nlayer_SGS,num_sgs_coefs)
      real(kind = kreal), intent(in) :: sgs_f_whole(num_sgs_kinds)
      real(kind = kreal), intent(in) :: sgs_c_whole(num_sgs_coefs)
!
      real(kind = kreal), intent(inout)                                 &
     &          :: sgs_f_clip(nlayer_SGS,num_sgs_kinds)
      real(kind = kreal), intent(inout)                                 &
     &          :: sgs_c_clip(nlayer_SGS,num_sgs_coefs)
      real(kind = kreal), intent(inout) :: sgs_fw_clip(num_sgs_kinds)
      real(kind = kreal), intent(inout) :: sgs_cw_clip(num_sgs_coefs)
!
      integer (kind = kint) :: nd
!
!
!      write(50+my_rank,*) 'nd, i, sgs_c_coef(i,icomp_f:icomp_f+numdir-1)'
!      do nd = 1, nlayer_SGS
!        write(50+my_rank,*) nd, i, sgs_c_coef(i,icomp_f:icomp_f+numdir-1)
!      end do
!
!
      if (SGS_param%iflag_nagetive_clip .eq. id_SGS_ZERO_CLIP) then
        do nd = 1, numdir
          call delete_negative_coefs                                    &
     &       (nlayer_SGS, SGS_param%clipping_limit,                     &
     &        sgs_c_coef(1,icomp_f+nd-1), sgs_c_whole(icomp_f+nd-1),    &
     &        sgs_c_clip(1,icomp_f+nd-1), sgs_cw_clip(icomp_f+nd-1))
        end do
          call delete_negative_coefs                                    &
     &       (nlayer_SGS, SGS_param%clipping_limit,                     &
     &        sgs_f_coef(1,ifield_d), sgs_f_whole(ifield_d),            &
     &        sgs_f_clip(1,ifield_d), sgs_fw_clip(ifield_d))
!
!
      else if(SGS_param%iflag_nagetive_clip .eq. id_SGS_KEEP_PREVIOUS) &
     & then
        do nd = 1, numdir
          call ignore_negative_coefs                                    &
     &       (nlayer_SGS, SGS_param%clipping_limit,                     &
     &        sgs_c_coef(1,icomp_f+nd-1), sgs_c_whole(icomp_f+nd-1),    &
     &        sgs_c_clip(1,icomp_f+nd-1), sgs_cw_clip(icomp_f+nd-1))
        end do
          call ignore_negative_coefs                                    &
     &       (nlayer_SGS, SGS_param%clipping_limit,                     &
     &        sgs_f_coef(1,ifield_d), sgs_f_whole(ifield_d),            &
     &        sgs_f_clip(1,ifield_d), sgs_fw_clip(ifield_d))
!
        if (iflag_SGS_initial .eq. 1) then
          do nd = 1, numdir
            call init_negative_coefs                                    &
     &         (nlayer_SGS, SGS_param%clipping_limit,                   &
     &          sgs_c_coef(1,icomp_f+nd-1), sgs_c_whole(icomp_f+nd-1),  &
     &          sgs_c_clip(1,icomp_f+nd-1), sgs_cw_clip(icomp_f+nd-1))
          end do
            call init_negative_coefs                                    &
     &         (nlayer_SGS, SGS_param%clipping_limit,                   &
     &          sgs_c_coef(1,icomp_f+nd-1), sgs_c_whole(icomp_f+nd-1),  &
     &          sgs_c_clip(1,icomp_f+nd-1), sgs_cw_clip(icomp_f+nd-1))
        end if
!
      else
!
        do nd = 1, numdir
          call copy_sgs_coefs(nlayer_SGS,                               &
     &      sgs_c_coef(1,icomp_f+nd-1), sgs_c_whole(icomp_f+nd-1),      &
     &      sgs_c_clip(1,icomp_f+nd-1), sgs_cw_clip(icomp_f+nd-1))
        end do
        call copy_sgs_coefs(nlayer_SGS,                                 &
     &       sgs_f_coef(1,ifield_d), sgs_f_whole(ifield_d),             &
     &       sgs_f_clip(1,ifield_d), sgs_fw_clip(ifield_d))
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
      subroutine clear_model_coefs_2_ele                                &
     &         (ele, numdir, icomp_f, ntot_comp_ele, ak_sgs)
!
      use m_machine_parameter
!
      type(element_data), intent(in) :: ele
      integer (kind = kint), intent(in) :: ntot_comp_ele, numdir
      integer (kind = kint), intent(in) :: icomp_f
!
      real(kind = kreal), intent(inout)                                 &
     &                   :: ak_sgs(ele%numele,ntot_comp_ele)
!
      integer (kind = kint) :: ip, ist, ied, nst, ned
      integer (kind = kint) :: iele, nd
!
!
      nst = icomp_f
      ned = icomp_f + numdir - 1
!
!$omp parallel do private(ist,ied,iele)
      do ip = 1, np_smp
        ist = ele%istack_ele_smp(ip-1) + 1
        ied = ele%istack_ele_smp(ip  )
        do nd = nst, ned
!cdir nodep
          do iele = ist, ied
            ak_sgs(iele,nd) = zero
          end do
        end do
      end do
!$omp end parallel do
!
      end subroutine clear_model_coefs_2_ele
!
!  ---------------------------------------------------------------------
!  ---------------------------------------------------------------------
!
      subroutine set_model_coefs_2_ele(ele, itype_csim, numdir,         &
     &          ifield_d, icomp_f, n_layer_d, n_item_layer_d,           &
     &          layer_stack_smp, item_layer, num_kind_ele,              &
     &          ntot_comp_ele, sgs_f_clip, sgs_c_clip, ak_sgs)
!
      use m_machine_parameter
      use m_control_parameter
!
      type(element_data), intent(in) :: ele
      integer (kind = kint), intent(in) :: itype_csim
      integer (kind = kint), intent(in) :: num_kind_ele, ntot_comp_ele
      integer (kind = kint), intent(in) :: numdir
      integer (kind = kint), intent(in) :: ifield_d, icomp_f
!
      integer (kind = kint), intent(in) :: n_layer_d, n_item_layer_d
      integer (kind = kint), intent(in)                                 &
     &                      :: layer_stack_smp(0:n_layer_d*np_smp)
      integer (kind = kint), intent(in) :: item_layer(n_item_layer_d)
      real(kind = kreal), intent(in)                                    &
     &          :: sgs_f_clip(n_item_layer_d,num_kind_ele)
      real(kind = kreal), intent(in)                                    &
     &          :: sgs_c_clip(n_item_layer_d,ntot_comp_ele)
!
      real(kind = kreal), intent(inout)                                 &
     &                   :: ak_sgs(ele%numele,ntot_comp_ele)
!
      integer (kind = kint) :: ip, is, ist, ied, nst, ned
      integer (kind = kint) :: inum, iele0, iele, nd
!
!
      nst = icomp_f
      ned = icomp_f + numdir - 1
!
      if(itype_csim .eq. 1) then
!$omp parallel do private(is,ist,ied,inum,iele0,iele)
        do ip = 1, np_smp
          do nd = nst, ned
            do inum = 1, n_layer_d
              is = (inum-1)*np_smp + ip
              ist = layer_stack_smp(is-1) + 1
              ied = layer_stack_smp(is  )
!
!cdir nodep
              do iele0 = ist, ied
                iele = item_layer(iele0)
                ak_sgs(iele,nd) = sgs_c_clip(inum,nd)
              end do
            end do
          end do
        end do
!$omp end parallel do
!
      else
!$omp parallel do private(is,ist,ied,inum,iele0,iele)
        do ip = 1, np_smp
          do nd = nst, ned
            do inum = 1, n_layer_d
              is = (inum-1)*np_smp + ip
              ist = layer_stack_smp(is-1) + 1
              ied = layer_stack_smp(is  )
!
!cdir nodep
              do iele0 = ist, ied
                iele = item_layer(iele0)
                ak_sgs(iele,nd) = sgs_f_clip(inum,ifield_d)
              end do
            end do
          end do
        end do
!$omp end parallel do
      end if
!
      end subroutine set_model_coefs_2_ele
!
!  ---------------------------------------------------------------------
!
      subroutine set_diff_coefs_layer_ele(ele, ifield_d,                &
     &         n_layer_d, n_item_layer_d, layer_stack_smp, item_layer,  &
     &         ntot_fld_ele, diff_f_clip, ak_diff)
!
      use m_machine_parameter
!
      type(element_data), intent(in) :: ele
      integer (kind = kint), intent(in) :: ntot_fld_ele, ifield_d
      integer (kind = kint), intent(in) :: n_layer_d, n_item_layer_d
      integer (kind = kint), intent(in)                                 &
     &                      :: layer_stack_smp(0:n_layer_d*np_smp)
      integer (kind = kint), intent(in) :: item_layer(n_item_layer_d)
      real(kind = kreal), intent(in)                                    &
     &          :: diff_f_clip(n_item_layer_d,ntot_fld_ele)
!
      real(kind = kreal), intent(inout)                                 &
     &                   :: ak_diff(ele%numele,ntot_fld_ele)
!
      integer (kind = kint) :: ip, is, ist, ied, inum, iele0, iele
!
!
!$omp parallel do private(is,ist,ied,inum,iele0,iele)
      do ip = 1, np_smp
          ist = ele%istack_ele_smp(ip-1) + 1
          ied = ele%istack_ele_smp(ip  )
!cdir nodep
          do iele = ist, ied
            ak_diff(iele,ifield_d) = zero
          end do
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
              ak_diff(iele,ifield_d) = diff_f_clip(inum,ifield_d)
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
     &          ifield_d, ntot_fld_ele, diff_f_whole_clip, ak_diff)
!
      use m_machine_parameter
!
      type(element_data), intent(in) :: ele
      integer (kind = kint), intent(in) :: ntot_fld_ele, ifield_d
      integer(kind=kint), intent(in) :: iele_fsmp_stack(0:np_smp)
      real(kind = kreal), intent(in) :: diff_f_whole_clip(ntot_fld_ele)
!
      real(kind = kreal), intent(inout)                                 &
     &                   :: ak_diff(ele%numele,ntot_fld_ele)
!
      integer (kind = kint) :: ip, ist, ied, iele
!
!
!$omp parallel do private(ist,ied,iele)
      do ip = 1, np_smp
        ist = ele%istack_ele_smp(ip-1) + 1
        ied = ele%istack_ele_smp(ip  )
!cdir nodep
        do iele = ist, ied
          ak_diff(iele,ifield_d) = zero
        end do
      end do
!$omp end parallel do
!
!$omp parallel do private(ist,ied,iele)
      do ip = 1, np_smp
        ist = iele_fsmp_stack(ip-1) + 1
        ied = iele_fsmp_stack(ip  )
        do iele = ist, ied
          ak_diff(iele,ifield_d) = diff_f_whole_clip(ifield_d)
        end do
      end do
!$omp end parallel do
!
      end subroutine set_diff_coefs_whole_ele
!
!  ---------------------------------------------------------------------
!
      end module set_sgs_diff_model_coefs
