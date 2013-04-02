!
!     module sgs_model_coefs_IO
!
!     programmed by H.Matsui in 2005
!     modified by H. Matsui on Aug., 2007
!
!      subroutine s_output_sgs_model_coefs
!
!      subroutine s_read_sgs_model_coefs
!
!      subroutine read_sgs_layerd_data(file_id, iflag, n_layer,         &
!     &          n_comp, coef)
!      subroutine read_sgs_whole_data(file_id, iflag, n_comp, coef)
!
      module sgs_model_coefs_IO
!
      use m_precision
!
      use m_parallel_var_dof
      use m_file_control_parameter
      use m_control_parameter
      use m_SGS_model_coefs
      use m_t_step_parameter
      use m_ele_info_4_dynamical
!
      implicit none
!
      private :: read_sgs_layerd_2data
      private :: read_sgs_whole_2data
!
!-----------------------------------------------------------------------
!
      contains
!
!-----------------------------------------------------------------------
!
      subroutine s_output_sgs_model_coefs
!
      use m_layering_ele_list
      use set_exit_flag_4_visualizer
!
      integer (kind = kint) :: i_coef, iflag
      integer (kind = kint) :: inum, nd
!
!
      if (iflag_dynamic_SGS .ne. id_SGS_DYNAMIC_OFF) then
!
        iflag = mod(istep_max_dt, i_step_sgs_output)
        if (iflag.eq.0) then
!
          call set_output_flag(i_coef, istep_max_dt, i_step_sgs_output)
!
          if (i_coef.eq.0 .and. my_rank.eq.0) then
            do inum = 1, n_layer_d
              write(sgs_fld_coef_file_code,1000) i_step_MHD,            &
     &           time, inum, (sgs_f_clip(inum,nd),nd=1,num_sgs_kinds)
              write(sgs_comp_coef_file_code,1000) i_step_MHD,           &
     &           time, inum, (sgs_c_clip(inum,nd),nd=1,num_sgs_coefs)
!
!
              write(sgs_cor_file_code,1000) i_step_MHD, time, inum,     &
     &         (cor_sgs(inum,nd),nd=1,num_sgs_coefs)
              write(sgs_ratio_file_code,1000) i_step_MHD, time, inum,   &
     &         (ratio_sgs(inum,nd),nd=1,num_sgs_coefs)
!
              write(sgs_rms_file_code,1000) i_step_MHD, time, inum,     &
     &         (rms_sgs_simi(inum,nd),nd=1,num_sgs_coefs),              &
     &         (rms_sgs_grad(inum,nd),nd=1,num_sgs_coefs)
!
            end do
!
            write(sgs_fld_whole_file_code,1001)  i_step_MHD, time,      &
     &        (sgs_f_whole_clip(nd),nd=1,num_sgs_kinds)
            write(sgs_comp_whole_file_code,1001)  i_step_MHD, time,     &
     &        (sgs_c_whole_clip(nd),nd=1,num_sgs_coefs)
!
            write(sgs_w_cor_file_code,1001)  i_step_MHD, time,          &
     &        (cor_sgs_w(nd),nd=1,num_sgs_coefs)
            write(sgs_w_ratio_file_code,1001) i_step_MHD, time,         &
     &        (ratio_sgs_w(nd),nd=1, num_sgs_coefs)
            write(sgs_w_rms_file_code,1001) i_step_MHD, time,           &
     &        (rms_sgs_simi_w(nd),nd=1,num_sgs_coefs),                  &
     &        (rms_sgs_grad_w(nd),nd=1,num_sgs_coefs)
!
            if (iflag_commute_correction .gt. id_SGS_commute_OFF) then
              write(diff_fld_whole_file_code,1001) i_step_MHD, time,    &
     &          (diff_f_whole_clip(nd),nd=1, num_diff_kinds)
              write(diff_comp_whole_file_code,1001) i_step_MHD, time,   &
     &          (diff_c_whole_clip(nd),nd=1, num_diff_coefs)
!
              write(diff_w_cor_file_code,1001) i_step_MHD, time,        &
     &          (cor_diff_w(nd),nd=1, num_diff_coefs)
              write(diff_w_ratio_file_code,1001) i_step_MHD, time,      &
     &          (ratio_diff_w(nd),nd=1, num_diff_coefs)
              write(diff_w_rms_file_code,1001) i_step_MHD, time,        &
     &          (rms_diff_simi_w(nd),nd=1, num_diff_coefs),             &
     &          (rms_diff_grad_w(nd),nd=1, num_diff_coefs)
!
              if (iset_DIFF_model_coefs .eq. 1 ) then
                do inum = 1, n_layer_d
                  write(diff_coef_file_code,1000)                       &
     &                 i_step_MHD, time, inum,                          &
     &              (diff_f_clip(inum,nd),nd=1,num_diff_kinds)
!
                  write(diff_cor_file_code,1000)                        &
     &                 i_step_MHD, time, inum,                          &
     &              (cor_diff(inum,nd),nd=1, num_diff_coefs)
                  write(diff_ratio_file_code,1000)                      &
     &                 i_step_MHD, time, inum,                          &
     &              (ratio_diff(inum,nd),nd=1, num_diff_coefs)
                  write(diff_rms_file_code,1000)                        &
     &                i_step_MHD, time, inum,                           &
     &              (rms_diff_simi(inum,nd),nd=1, num_diff_coefs),      &
     &              (rms_diff_grad(inum,nd),nd=1, num_diff_coefs)
                end do
              end if
            end if
!
          end if
!
        end if
      end if
!
 1000 format(i10,1pE25.15e3,i10,1p200E25.15e3)
 1001 format(i10,1pE25.15e3,1p200E25.15e3)
!
      end subroutine s_output_sgs_model_coefs
!
!-----------------------------------------------------------------------
!
      subroutine s_read_sgs_model_coefs
!
      use m_layering_ele_list
!
      integer (kind = kint) :: iflag
!
!
      if (iflag_dynamic_SGS .ne. id_SGS_DYNAMIC_OFF) then
!
        iflag = i_step_init - mod(istep_max_dt, i_step_sgs_output)
!
        call read_sgs_layerd_data(sgs_fld_coef_file_code, iflag,        &
     &          n_layer_d, num_sgs_kinds, sgs_f_clip)
        call read_sgs_layerd_data(sgs_comp_coef_file_code, iflag,       &
     &          n_layer_d, num_sgs_coefs, sgs_c_clip)
!
        call read_sgs_layerd_data(sgs_cor_file_code, iflag,             &
     &          n_layer_d, num_sgs_coefs, cor_sgs)
!
        call read_sgs_layerd_data(sgs_ratio_file_code, iflag,           &
     &          n_layer_d, num_sgs_coefs, ratio_sgs)
!
        call read_sgs_layerd_2data(sgs_rms_file_code, iflag,            &
     &         n_layer_d, num_sgs_coefs, rms_sgs_simi, rms_sgs_grad)
!
!
        call read_sgs_whole_data(sgs_fld_whole_file_code, iflag,        &
     &      num_sgs_kinds, sgs_f_whole_clip)
        call read_sgs_whole_data(sgs_comp_whole_file_code, iflag,       &
     &      num_sgs_coefs, sgs_c_whole_clip)
        call read_sgs_whole_data(sgs_w_cor_file_code, iflag,            &
     &      num_sgs_coefs, cor_sgs_w)
        call read_sgs_whole_data(sgs_w_ratio_file_code, iflag,          &
     &      num_sgs_coefs, ratio_sgs_w)
!
        call read_sgs_whole_2data(sgs_w_rms_file_code, iflag,           &
     &      num_sgs_coefs, rms_sgs_simi_w, rms_sgs_grad_w)
!
!
        if (iflag_commute_correction .gt. id_SGS_commute_OFF) then
!
          call read_sgs_whole_data(diff_fld_whole_file_code, iflag,     &
     &        num_diff_kinds, diff_f_whole_clip)
          call read_sgs_whole_data(diff_comp_whole_file_code, iflag,    &
     &        num_diff_coefs, diff_c_whole_clip)
!
          call read_sgs_whole_data(diff_w_cor_file_code, iflag,         &
     &        num_diff_coefs, cor_diff_w)
          call read_sgs_whole_data(diff_w_ratio_file_code, iflag,       &
     &        num_diff_coefs, ratio_diff_w)
!
          call read_sgs_whole_2data(diff_w_rms_file_code, iflag,        &
     &        num_diff_coefs, rms_diff_simi_w, rms_diff_grad_w)
!
          if (iset_DIFF_model_coefs .eq. 1 ) then
!
            call read_sgs_layerd_data(diff_coef_file_code, iflag,       &
     &          n_layer_d, num_diff_kinds, diff_f_clip)
!
            call read_sgs_layerd_data(diff_cor_file_code, iflag,        &
     &          n_layer_d, num_diff_coefs, cor_diff)
!
            call read_sgs_layerd_data(diff_ratio_file_code, iflag,      &
     &          n_layer_d, num_diff_coefs, ratio_diff)
!
            call read_sgs_layerd_2data(diff_rms_file_code, iflag,       &
     &         n_layer_d, num_diff_coefs, rms_diff_simi, rms_diff_grad)
!
          end if
        end if
      end if
!
!
      end subroutine s_read_sgs_model_coefs
!
!-----------------------------------------------------------------------
!
      subroutine read_sgs_layerd_data(file_id, iflag, n_layer,          &
     &          n_comp, coef)
!
      integer(kind = kint), intent(in) :: file_id, iflag
      integer(kind = kint), intent(in) :: n_layer, n_comp
      real(kind = kreal), intent(inout) :: coef(n_layer, n_comp)
!
      integer(kind = kint) :: inum, nd, i_read_step, itmp
      real(kind = kreal) :: rtmp
!
!
      do
        do inum = 1, n_layer
          read(file_id,*,err=99,end=99)                                 &
     &        i_read_step, rtmp, itmp, (coef(inum,nd),nd=1, n_comp)
        end do
        if (i_read_step .ge. iflag) exit
      end do
 99   continue
!
      end subroutine read_sgs_layerd_data
!
!-----------------------------------------------------------------------
!
      subroutine read_sgs_layerd_2data(file_id, iflag, n_layer,         &
     &          n_comp, coef1, coef2)
!
      integer(kind = kint), intent(in) :: file_id, iflag
      integer(kind = kint), intent(in) :: n_layer, n_comp
      real(kind = kreal), intent(inout) :: coef1(n_layer, n_comp)
      real(kind = kreal), intent(inout) :: coef2(n_layer, n_comp)
!
      integer(kind = kint) :: inum, nd, i_read_step, itmp
      real(kind = kreal) :: rtmp
!
!
      do
        do inum = 1, n_layer
          read(file_id,*,err=99,end=99) i_read_step, rtmp, itmp,        &
     &     (coef1(inum,nd),nd=1, n_comp), (coef2(inum,nd),nd=1, n_comp)
        end do
        if (i_read_step .ge. iflag) exit
      end do
 99   continue
!
      end subroutine read_sgs_layerd_2data
!
!-----------------------------------------------------------------------
!
      subroutine read_sgs_whole_data(file_id, iflag, n_comp, coef)
!
      integer(kind = kint), intent(in) :: file_id, iflag, n_comp
      real(kind = kreal), intent(inout) :: coef(n_comp)
!
      integer(kind = kint) :: inum, nd, i_read_step
      real(kind = kreal) :: rtmp
!
!
      do
          read(file_id,*,err=99,end=99)                                 &
     &        i_read_step, rtmp, (coef(nd),nd=1, n_comp)
        if (i_read_step .ge. iflag) exit
      end do
 99   continue
!
      end subroutine read_sgs_whole_data
!
!-----------------------------------------------------------------------
!
      subroutine read_sgs_whole_2data(file_id, iflag, n_comp,           &
     &          coef1, coef2)
!
      integer(kind = kint), intent(in) :: file_id, iflag, n_comp
      real(kind = kreal), intent(inout) :: coef1(n_comp), coef2(n_comp)
!
      integer(kind = kint) :: nd, i_read_step
      real(kind = kreal) :: rtmp
!
!
      do
          read(file_id,*,err=99,end=99) i_read_step, rtmp,              &
     &     (coef1(nd),nd=1, n_comp), (coef2(nd),nd=1, n_comp)
        if (i_read_step .ge. iflag) exit
      end do
 99   continue
!
      end subroutine read_sgs_whole_2data
!
!-----------------------------------------------------------------------
!
      end module sgs_model_coefs_IO
