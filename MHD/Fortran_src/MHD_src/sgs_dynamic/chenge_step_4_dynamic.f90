!
!     module chenge_step_4_dynamic
!
!        Written by H. Matsui on Aug., 2007
!
!!      subroutine s_chenge_step_4_dynamic                              &
!!     &         (id_rank, i_step_MHD, SGS_par, SGS_MHD_wk)
!!        type(SGS_paremeters), intent(inout) :: SGS_par
!!        type(work_FEM_SGS_MHD), intent(inout) :: SGS_MHD_wk
!!      subroutine copy_model_coef_2_previous                           &
!!     &         (SGS_param, cmt_param, FEM_SGS_wk)
!!        type(SGS_model_control_params), intent(in) :: SGS_param
!!        type(commutation_control_params), intent(in) :: cmt_param
!!        type(work_FEM_dynamic_SGS), intent(inout) :: FEM_SGS_wk
!
      module chenge_step_4_dynamic
!
      use m_precision
!
      use t_SGS_control_parameter
!
      implicit none
!
      integer(kind=kint), parameter :: sgs_diff_max_code =   30
      character(len=kchara), parameter                                  &
     &           :: sgs_diff_max_name = "SGS_step_monitor.dat"
!
      private :: sgs_diff_max_code, sgs_diff_max_name
!
      private :: find_maximum_model_coefs, chenge_increment_4_dynamic
      private :: find_maximum_Csim, copy_Csim_2_previous
!
!-----------------------------------------------------------------------
!
      contains
!
!-----------------------------------------------------------------------
!
      subroutine open_sgs_diff_monitor(id_rank)
!
      integer, intent(in) :: id_rank
!
!
      if(id_rank .gt. 0) return
!
      open (sgs_diff_max_code,file = sgs_diff_max_name,                 &
     &        status='old', position='append', err = 99)
      return
!
  99  continue
      open (sgs_diff_max_code,file = sgs_diff_max_name,                 &
     &        status='replace')
!
      end subroutine open_sgs_diff_monitor
!
!-----------------------------------------------------------------------
!
      subroutine s_chenge_step_4_dynamic                                &
     &         (id_rank, i_step_MHD, SGS_par, SGS_MHD_wk)
!
      use t_work_FEM_SGS_MHD
!
      integer, intent(in) :: id_rank
      integer(kind=kint), intent(in) :: i_step_MHD
!
      type(SGS_paremeters), intent(inout) :: SGS_par
      type(work_FEM_SGS_MHD), intent(inout) :: SGS_MHD_wk
!
!
      if(SGS_par%model_p%iflag_dynamic .eq. id_SGS_DYNAMIC_OFF) return
      call chenge_increment_4_dynamic                                   &
     &   (id_rank, i_step_MHD, SGS_par%model_p, SGS_par%commute_p,      &
     &    SGS_par%i_step_sgs_coefs,  SGS_MHD_wk%FEM_SGS_wk)
!
      end subroutine s_chenge_step_4_dynamic
!
!-----------------------------------------------------------------------
!
      subroutine copy_model_coef_2_previous                             &
     &         (SGS_param, cmt_param, FEM_SGS_wk)
!
      use t_work_FEM_dynamic_SGS
!
      type(SGS_model_control_params), intent(in) :: SGS_param
      type(commutation_control_params), intent(in) :: cmt_param
      type(work_FEM_dynamic_SGS), intent(inout) :: FEM_SGS_wk
!
!
      if(SGS_param%iflag_dynamic .eq. id_SGS_DYNAMIC_OFF) return
      call copy_Csim_2_previous(cmt_param, FEM_SGS_wk%wk_sgs%nlayer,    &
     &    FEM_SGS_wk%wk_sgs%num_kinds, FEM_SGS_wk%wk_sgs%fld_coef,      &
     &    FEM_SGS_wk%wk_diff%nlayer, FEM_SGS_wk%wk_diff%num_kinds,      &
     &    FEM_SGS_wk%wk_diff%fld_coef, FEM_SGS_wk%wk_diff%fld_whole,    &
     &    FEM_SGS_wk%wk_sgs%coef_p, FEM_SGS_wk%wk_diff%coef_p,          &
     &    FEM_SGS_wk%wk_diff%coef_wp)
!
      end subroutine copy_model_coef_2_previous
!
!-----------------------------------------------------------------------
!
      subroutine find_maximum_model_coefs                               &
     &         (cmt_param, wk_sgs, wk_diff, diff_max)
!
      use t_ele_info_4_dynamic
!
      type(commutation_control_params), intent(in) :: cmt_param
      type(dynamic_model_data), intent(inout) :: wk_sgs
      type(dynamic_model_data), intent(inout) :: wk_diff
!
      real(kind = kreal), intent(inout) :: diff_max
!
      call find_maximum_Csim(cmt_param,                                 &
     &      wk_sgs%nlayer, wk_sgs%num_kinds,                            &
     &      wk_sgs%fld_coef, wk_sgs%coef_p,                             &
     &      wk_diff%nlayer, wk_diff%num_kinds,                          &
     &      wk_diff%fld_coef, wk_diff%fld_whole,                        &
     &      wk_diff%coef_p, wk_diff%coef_wp, diff_max)
!
      end subroutine find_maximum_model_coefs
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!
      subroutine chenge_increment_4_dynamic                             &
     &         (id_rank, i_step_MHD, SGS_param, cmt_param,              &
     &          i_step_sgs_coefs, FEM_SGS_wk)
!
      use t_work_FEM_dynamic_SGS
!
      type(SGS_model_control_params), intent(in) :: SGS_param
      type(commutation_control_params), intent(in) :: cmt_param
      integer, intent(in) :: id_rank
      integer(kind=kint), intent(in) :: i_step_MHD
!
      integer(kind = kint), intent(inout) :: i_step_sgs_coefs
      type(work_FEM_dynamic_SGS), intent(inout) :: FEM_SGS_wk
!
      real(kind = kreal) ::  diff_max
!
!
      if(mod(i_step_MHD, i_step_sgs_coefs) .eq. 0) then
        call open_sgs_diff_monitor(id_rank)
!
        call find_maximum_model_coefs                                   &
     &     (cmt_param, FEM_SGS_wk%wk_sgs, FEM_SGS_wk%wk_diff, diff_max)
!
        call copy_model_coef_2_previous                                 &
     &     (SGS_param, cmt_param, FEM_SGS_wk)
!
        if (id_rank .eq. 0) write(sgs_diff_max_code,*)                  &
     &    'difference from previous step: ', i_step_MHD, diff_max
!
        if (diff_max .gt. SGS_param%extend_SGS_dt) then
          if      (i_step_sgs_coefs .eq. 1) then
            i_step_sgs_coefs = 1
          else if (i_step_sgs_coefs .eq. 2) then
            i_step_sgs_coefs = 1
          else if (i_step_sgs_coefs .eq. 5) then
            i_step_sgs_coefs = 2
          else if (i_step_sgs_coefs .eq. 10) then
            i_step_sgs_coefs = 5
          else if (i_step_sgs_coefs .eq. 20) then
            i_step_sgs_coefs = 10
          else if (i_step_sgs_coefs .eq. 50) then
           i_step_sgs_coefs = 20
          else
            i_step_sgs_coefs = i_step_sgs_coefs / 2
          end if
!
          if (i_step_sgs_coefs .gt. SGS_param%max_step_dynamic) then
            i_step_sgs_coefs = SGS_param%max_step_dynamic
          else if(i_step_sgs_coefs .lt. SGS_param%min_step_dynamic)     &
     &     then
            i_step_sgs_coefs = SGS_param%min_step_dynamic
          end if
!
          if (id_rank .eq. 0) write(sgs_diff_max_code,*)                &
     &    'change step interbal for dynamic to ', i_step_sgs_coefs
        end if
!
!
        if (diff_max .lt. SGS_param%extend_SGS_dt) then
          if      (i_step_sgs_coefs .eq. 1) then
            i_step_sgs_coefs = 2
          else if (i_step_sgs_coefs .eq. 2) then
            i_step_sgs_coefs = 5
          else if (i_step_sgs_coefs .eq. 5) then
            i_step_sgs_coefs = 10
          else if (i_step_sgs_coefs .eq. 10) then
            i_step_sgs_coefs = 20
          else if (i_step_sgs_coefs .eq. 20) then
            i_step_sgs_coefs = 50
          else
            i_step_sgs_coefs = 2*i_step_sgs_coefs
          end if
!
          if (i_step_sgs_coefs .gt. SGS_param%max_step_dynamic) then
            i_step_sgs_coefs = SGS_param%max_step_dynamic
          else if(i_step_sgs_coefs .lt. SGS_param%min_step_dynamic)     &
     &     then
            i_step_sgs_coefs = SGS_param%min_step_dynamic
          end if
!
          if (id_rank .eq. 0) write(sgs_diff_max_code,*)                &
     &    'change step interbal for dynamic to ', i_step_sgs_coefs
        end if
!
        if(id_rank .eq. 0) close(sgs_diff_max_code)
      end if
!
      end subroutine chenge_increment_4_dynamic
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!
      subroutine find_maximum_Csim(cmt_param,                           &
     &          nlayer_sgs, num_sgs_kind, sgs_f_coef, coef_sgs_p,       &
     &          nlayer_diff, num_diff_kind, diff_f_coef, diff_f_whole,  &
     &          coef_diff_p, coef_diff_wp, diff_max)
!
      type(commutation_control_params), intent(in) :: cmt_param
      integer(kind = kint), intent(in) :: nlayer_sgs, num_sgs_kind
      real(kind = kreal), intent(in)                                    &
     &                    :: sgs_f_coef(nlayer_SGS,num_sgs_kind)
      real(kind = kreal), intent(in)                                    &
     &                    :: coef_sgs_p(nlayer_diff,num_sgs_kind)
!
      integer(kind = kint), intent(in) :: nlayer_diff, num_diff_kind
      real(kind = kreal), intent(in)                                    &
     &                    :: diff_f_coef(nlayer_diff,num_diff_kind)
      real(kind = kreal), intent(in) :: diff_f_whole(num_diff_kind)
!
      real(kind = kreal), intent(in)                                    &
     &                    :: coef_diff_p(nlayer_diff,num_diff_kind)
      real(kind = kreal), intent(in) :: coef_diff_wp(num_diff_kind)
!
      real(kind = kreal), intent(inout) :: diff_max
!
      integer(kind = kint) :: i, j
      real(kind = kreal) :: diff_r
!
!
      diff_r = 0.0d0
      diff_max = 0.0d0
      do j = 1, num_sgs_kind
        do i = 1, nlayer_SGS
          diff_r = abs(sgs_f_coef(i,j) - coef_sgs_p(i,j))
          diff_max = max(diff_max, diff_r)
        end do
      end do
!
      if (cmt_param%iflag_c_linear .gt. id_SGS_commute_OFF) then
        if (cmt_param%iset_DIFF_coefs .eq. 1) then
          do j = 1, num_diff_kind
            do i = 1, nlayer_diff
              diff_r = abs(diff_f_coef(i,j) - coef_diff_p(i,j))
              diff_max = max(diff_max, diff_r)
            end do
          end do
       else
          do j = 1, num_diff_kind
            diff_r = abs(diff_f_whole(j) - coef_diff_wp(j))
            diff_max = max(diff_max, diff_r)
          end do
        end if
      end if
!
      end subroutine find_maximum_Csim
!
!-----------------------------------------------------------------------
!
      subroutine copy_Csim_2_previous(cmt_param,                        &
     &          nlayer_SGS, num_sgs_kind,  sgs_f_coef,                  &
     &          nlayer_diff, num_diff_kind, diff_f_coef, diff_f_whole,  &
     &          coef_sgs_p, coef_diff_p, coef_diff_wp)
!
      type(commutation_control_params), intent(in) :: cmt_param
      integer(kind = kint), intent(in) :: nlayer_sgs, num_sgs_kind
      real(kind = kreal), intent(in)                                    &
     &                    :: sgs_f_coef(nlayer_SGS,num_sgs_kind)
!
      integer(kind = kint), intent(in) :: nlayer_diff, num_diff_kind
      real(kind = kreal), intent(in)                                    &
     &                     :: diff_f_coef(nlayer_diff,num_diff_kind)
      real(kind = kreal), intent(in) :: diff_f_whole(num_diff_kind)
!
      real(kind = kreal), intent(inout)                                 &
     &                    :: coef_sgs_p(nlayer_diff,num_sgs_kind)
      real(kind = kreal), intent(inout)                                 &
     &                    :: coef_diff_p(nlayer_diff,num_diff_kind)
      real(kind = kreal), intent(inout) :: coef_diff_wp(num_diff_kind)
!
      integer(kind = kint) :: i, j
!
!
      do j = 1, num_sgs_kind
        do i = 1, nlayer_sgs
          coef_sgs_p(i,j) = sgs_f_coef(i,j)
        end do
      end do
!
      if (cmt_param%iflag_c_linear .gt. id_SGS_commute_OFF) then
        if (cmt_param%iset_DIFF_coefs .eq. 1) then
          do j = 1, num_diff_kind
            do i = 1, nlayer_diff
              coef_diff_p(i,j) = diff_f_coef(i,j)
            end do
          end do
       else
          do j = 1, num_diff_kind
            coef_diff_wp(j) = diff_f_whole(j)
          end do
        end if
      end if
!
      end subroutine copy_Csim_2_previous
!
!-----------------------------------------------------------------------
!
      end module chenge_step_4_dynamic
