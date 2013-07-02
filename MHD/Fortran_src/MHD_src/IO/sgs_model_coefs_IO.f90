!
!     module sgs_model_coefs_IO
!
!     programmed by H.Matsui in 2005
!     modified by H. Matsui on Aug., 2007
!
!      subroutine s_output_sgs_model_coefs
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
      use m_control_parameter
      use m_SGS_model_coefs
      use m_t_step_parameter
      use m_layering_ele_list
      use m_ele_info_4_dynamical
      use open_sgs_model_coefs
!
      implicit none
!
      integer(kind=kint), parameter :: sgs_fld_coef_file_code =  21
      integer(kind=kint), parameter :: sgs_comp_coef_file_code = 22
      integer(kind=kint), parameter :: diff_coef_file_code =  31
      integer(kind=kint), parameter :: diff_comp_file_code =  32
!
      integer(kind=kint), parameter :: sgs_cor_file_code =    24
      integer(kind=kint), parameter :: sgs_ratio_file_code =  25
      integer(kind=kint), parameter :: sgs_rms_file_code =    26
      integer(kind=kint), parameter :: sgs_cov_file_code =    36
      integer(kind=kint), parameter :: diff_cor_file_code =   27
      integer(kind=kint), parameter :: diff_cov_file_code =   37
      integer(kind=kint), parameter :: diff_ratio_file_code = 28
      integer(kind=kint), parameter :: diff_rms_file_code =   29
!
      character(len=kchara), parameter                                  &
     &       :: sgs_fld_coef_file_name =  'sgs_model_coefs.dat'
      character(len=kchara), parameter                                  &
     &       :: sgs_comp_coef_file_name = 'sgs_model_coefs_comp.dat'
      character(len=kchara), parameter                                  &
     &       :: sgs_fld_whole_file_name =  'sgs_m_coefs_whole.dat'
      character(len=kchara), parameter                                  &
     &       :: sgs_comp_whole_file_name = 'sgs_m_coefs_whole_comp.dat'
      character(len=kchara), parameter                                  &
     &       :: diff_coef_file_name =    'diff_model_coefs.dat'
      character(len=kchara), parameter                                  &
     &       :: diff_fld_whole_file_name = 'diff_m_coefs_whole.dat'
      character(len=kchara), parameter  :: diff_comp_whole_file_name    &
     &        = 'diff_m_comp_coefs_whole.dat'
!
      character(len=kchara), parameter                                  &
     &       :: sgs_cor_file_name =      'sgs_correlate.dat'
      character(len=kchara), parameter                                  &
     &       :: sgs_cov_file_name =      'sgs_covariance.dat'
      character(len=kchara), parameter                                  &
     &       :: sgs_ratio_file_name =    'sgs_rms_ratio.dat'
      character(len=kchara), parameter                                  &
     &       :: sgs_rms_file_name =      'sgs_rms.dat'
      character(len=kchara), parameter                                  &
     &       :: diff_cor_file_name =     'diff_correlate.dat'
      character(len=kchara), parameter                                  &
     &       :: diff_cov_file_name =     'diff_covariance.dat'
      character(len=kchara), parameter                                  &
     &       :: diff_ratio_file_name =   'diff_rms_ratio.dat'
      character(len=kchara), parameter                                  &
     &       :: diff_rms_file_name =     'diff_rms.dat'
!
      character(len=kchara), parameter                                  &
     &       :: sgs_w_cor_file_name =    'sgs_correlate_whole.dat'
      character(len=kchara), parameter                                  &
     &       :: sgs_w_cov_file_name =    'sgs_covariance_whole.dat'
      character(len=kchara), parameter                                  &
     &       :: sgs_w_ratio_file_name =  'sgs_rms_ratio_whole.dat'
      character(len=kchara), parameter                                  &
     &       :: sgs_w_rms_file_name =    'sgs_rms_whole.dat'
      character(len=kchara), parameter                                  &
     &       :: diff_w_cor_file_name =   'diff_correlate_whole.dat'
      character(len=kchara), parameter                                  &
     &       :: diff_w_cov_file_name =   'diff_covariance_whole.dat'
      character(len=kchara), parameter                                  &
     &       :: diff_w_ratio_file_name = 'diff_rms_ratio_whole.dat'
      character(len=kchara), parameter                                  &
     &       :: diff_w_rms_file_name =   'diff_rms_whole.dat'
!
      private :: sgs_fld_coef_file_code, sgs_comp_coef_file_code
      private :: diff_coef_file_code, diff_comp_file_code
      private :: sgs_cor_file_code, sgs_ratio_file_code
      private :: sgs_rms_file_code, sgs_cov_file_code
      private :: diff_cor_file_code, diff_cov_file_code
      private :: diff_ratio_file_code, diff_rms_file_code
!
      private :: sgs_fld_coef_file_name, sgs_comp_coef_file_name
      private :: sgs_fld_whole_file_name, sgs_comp_whole_file_name
      private :: diff_coef_file_name, diff_fld_whole_file_name
      private :: diff_comp_whole_file_name
      private :: sgs_cor_file_name, sgs_cov_file_name
      private :: sgs_ratio_file_name, sgs_rms_file_name
      private :: diff_cor_file_name, diff_cov_file_name
      private :: diff_ratio_file_name, diff_rms_file_name
      private :: sgs_w_cor_file_name, sgs_w_cov_file_name
      private :: sgs_w_ratio_file_name, sgs_w_rms_file_name
      private :: diff_w_cor_file_name, diff_w_cov_file_name
      private :: diff_w_ratio_file_name, diff_w_rms_file_name
!
      private :: output_layered_model_coefs_file
      private :: output_whole_model_coefs_file
      private :: output_whole_diff_coefs_file
      private :: output_layered_diff_coefs_file
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
      integer (kind = kint) :: i_coef
!
!
      if (iflag_dynamic_SGS .eq. id_SGS_DYNAMIC_OFF) return
!
      if (mod(istep_max_dt,i_step_sgs_output) .ne. 0) return
!
      call set_output_flag(i_coef, istep_max_dt, i_step_sgs_output)
      if (i_coef.ne.0 .or. my_rank.ne.0) return
!
      call output_layered_model_coefs_file
      call output_whole_model_coefs_file
!
      if (iflag_commute_correction .gt. id_SGS_commute_OFF) then
        call output_whole_diff_coefs_file
!
        if (iset_DIFF_model_coefs .eq. 1 ) then
          call output_layered_diff_coefs_file
        end if
      end if
!
      end subroutine s_output_sgs_model_coefs
!
!-----------------------------------------------------------------------
!
      subroutine output_layered_model_coefs_file
!
      integer (kind = kint) :: inum
!
!
      call open_SGS_model_coef_file(iflag_layered,                      &
     &    sgs_fld_coef_file_code, sgs_fld_coef_file_name)
      call open_SGS_correlation_file(iflag_layered,                     &
     &    sgs_comp_coef_file_code, sgs_comp_coef_file_name)
!
      call open_SGS_correlation_file(iflag_layered,                     &
     &    sgs_cor_file_code, sgs_cor_file_name)
      call open_SGS_correlation_file(iflag_layered,                     &
     &    sgs_cov_file_code, sgs_cov_file_name)
      call open_SGS_correlation_file(iflag_layered,                     &
     &    sgs_ratio_file_code, sgs_ratio_file_name)
!
      call open_SGS_rms_ratio_file(iflag_layered,                       &
     &    sgs_rms_file_code, sgs_rms_file_name)
!
!
      do inum = 1, n_layer_d
        write(sgs_fld_coef_file_code,1000) i_step_MHD,                  &
     &           time, inum, sgs_f_clip(inum,1:num_sgs_kinds)
        write(sgs_comp_coef_file_code,1000) i_step_MHD,                 &
     &           time, inum, sgs_c_clip(inum,1:num_sgs_coefs)
!
        write(sgs_cor_file_code,1000) i_step_MHD, time, inum,           &
     &         cor_sgs(inum,1:num_sgs_coefs)
        write(sgs_cov_file_code,1000) i_step_MHD, time, inum,           &
     &         cov_sgs(inum,1:num_sgs_coefs)
        write(sgs_ratio_file_code,1000) i_step_MHD, time, inum,         &
     &         ratio_sgs(inum,1:num_sgs_coefs)
!
        write(sgs_rms_file_code,1000) i_step_MHD, time, inum,           &
     &         rms_sgs_simi(inum,1:num_sgs_coefs),                      &
     &         rms_sgs_grad(inum,1:num_sgs_coefs)
      end do
!
      close (sgs_fld_coef_file_code)
      close (sgs_comp_coef_file_code)
!
      close (sgs_cov_file_code)
      close (sgs_cor_file_code)
      close (sgs_ratio_file_code)
      close (sgs_rms_file_code)
!
 1000 format(i10,1pE25.15e3,i10,1p200E25.15e3)
!
      end subroutine output_layered_model_coefs_file
!
!-----------------------------------------------------------------------
!
      subroutine output_whole_model_coefs_file
!
!
      call open_SGS_model_coef_file(iflag_whole,                        &
     &    sgs_fld_coef_file_code, sgs_fld_whole_file_name)
      call open_SGS_correlation_file(iflag_whole,                       &
     &    sgs_comp_coef_file_code, sgs_comp_whole_file_name)
!
      call open_SGS_correlation_file(iflag_whole,                       &
     &    sgs_cor_file_code, sgs_w_cor_file_name)
      call open_SGS_correlation_file(iflag_whole,                       &
     &    sgs_cov_file_code, sgs_w_cov_file_name)
      call open_SGS_correlation_file(iflag_whole,                       &
     &    sgs_ratio_file_code, sgs_w_ratio_file_name)
!
      call open_SGS_rms_ratio_file(iflag_whole,                         &
     &    sgs_rms_file_code, sgs_w_rms_file_name)
!
      write(sgs_fld_coef_file_code,1001)  i_step_MHD, time,             &
     &        sgs_f_whole_clip(1:num_sgs_kinds)
      write(sgs_comp_coef_file_code,1001)  i_step_MHD, time,            &
     &        sgs_c_whole_clip(1:num_sgs_coefs)
!
      write(sgs_cor_file_code,1001)  i_step_MHD, time,                  &
     &        cor_sgs_w(1:num_sgs_coefs)
      write(sgs_cov_file_code,1001)  i_step_MHD, time,                  &
     &        cov_sgs_w(1:num_sgs_coefs)
      write(sgs_ratio_file_code,1001) i_step_MHD, time,                 &
     &        ratio_sgs_w(1:num_sgs_coefs)
      write(sgs_rms_file_code,1001) i_step_MHD, time,                   &
     &        rms_sgs_simi_w(1:num_sgs_coefs),                          &
     &        rms_sgs_grad_w(1:num_sgs_coefs)
!
      close (sgs_fld_coef_file_code)
      close (sgs_comp_coef_file_code)
      close (sgs_cor_file_code)
      close (sgs_cov_file_code)
      close (sgs_ratio_file_code)
      close (sgs_rms_file_code)
!
 1001 format(i10,1pE25.15e3,1p200E25.15e3)
!
      end subroutine output_whole_model_coefs_file
!
!-----------------------------------------------------------------------
!
      subroutine output_whole_diff_coefs_file
!
!
      call open_SGS_diff_coef_file(iflag_whole,                         &
     &      diff_coef_file_code, diff_fld_whole_file_name)
      call open_SGS_diff_coef_file(iflag_whole,                         &
     &      diff_comp_file_code, diff_comp_whole_file_name)
!
      call open_diff_correlation_file(iflag_whole,                      &
     &      diff_cor_file_code, diff_w_cor_file_name)
      call open_diff_correlation_file(iflag_whole,                      &
     &      diff_cov_file_code, diff_w_cov_file_name)
      call open_diff_correlation_file(iflag_whole,                      &
     &      diff_ratio_file_code, diff_w_ratio_file_name)
!
      call open_diff_rms_ratio_file(iflag_whole,                        &
     &      diff_rms_file_code, diff_w_rms_file_name)
!
      write(diff_coef_file_code,1001) i_step_MHD, time,                 &
     &          diff_f_whole_clip(1:num_diff_kinds)
      write(diff_comp_file_code,1001) i_step_MHD, time,                 &
     &          diff_c_whole_clip(1:num_diff_coefs)
!
      write(diff_cor_file_code,1001) i_step_MHD, time,                  &
     &          cor_diff_w(1:num_diff_coefs)
      write(diff_cov_file_code,1001) i_step_MHD, time,                  &
     &          cov_diff_w(1:num_diff_coefs)
      write(diff_ratio_file_code,1001) i_step_MHD, time,                &
     &          ratio_diff_w(1:num_diff_coefs)
      write(diff_rms_file_code,1001) i_step_MHD, time,                  &
     &          rms_diff_simi_w(1:num_diff_coefs),                      &
     &          rms_diff_grad_w(1:num_diff_coefs)
!
      close (diff_coef_file_code)
      close (diff_comp_file_code)
      close (diff_cor_file_code)
      close (diff_cov_file_code)
      close (diff_ratio_file_code)
      close (diff_rms_file_code)
!
 1001 format(i10,1pE25.15e3,1p200E25.15e3)
!
      end subroutine output_whole_diff_coefs_file
!
!-----------------------------------------------------------------------
!
      subroutine output_layered_diff_coefs_file
!
      integer (kind = kint) :: inum
!
!
      call open_SGS_diff_coef_file(iflag_layered,                       &
     &              diff_coef_file_code, diff_coef_file_name)
!
      call open_diff_correlation_file(iflag_layered,                    &
     &              diff_cor_file_code, diff_cor_file_name)
      call open_diff_correlation_file(iflag_layered,                    &
     &              diff_cov_file_code, diff_cov_file_name)
      call open_diff_correlation_file(iflag_layered,                    &
     &              diff_ratio_file_code, diff_ratio_file_name)
!
      call open_diff_rms_ratio_file(iflag_layered,                      &
     &              diff_rms_file_code, diff_rms_file_name)
!
      do inum = 1, n_layer_d
        write(diff_coef_file_code,1000)                                 &
     &       i_step_MHD, time, inum,                                    &
     &              diff_f_clip(inum,1:num_diff_kinds)
!
        write(diff_cor_file_code,1000)                                  &
     &       i_step_MHD, time, inum,                                    &
     &              cor_diff(inum,1:num_diff_coefs)
        write(diff_cov_file_code,1000)                                  &
     &       i_step_MHD, time, inum,                                    &
     &              cov_diff(inum,1:num_diff_coefs)
!
        write(diff_ratio_file_code,1000)                                &
     &       i_step_MHD, time, inum,                                    &
     &              ratio_diff(inum,1:num_diff_coefs)
        write(diff_rms_file_code,1000)                                  &
     &      i_step_MHD, time, inum,                                     &
     &              rms_diff_simi(inum,1:num_diff_coefs),               &
     &              rms_diff_grad(inum,1:num_diff_coefs)
      end do
!
      close(diff_coef_file_code)
      close(diff_cor_file_code)
      close(diff_cov_file_code)
      close(diff_ratio_file_code)
      close(diff_rms_file_code)
!
 1000 format(i10,1pE25.15e3,i10,1p200E25.15e3)
!
      end subroutine output_layered_diff_coefs_file
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
      integer(kind = kint) :: nd, i_read_step
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
