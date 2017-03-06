!
!     module sgs_model_coefs_IO
!
!     programmed by H.Matsui in 2005
!     modified by H. Matsui on Aug., 2007
!
!!      subroutine s_output_sgs_model_coefs                             &
!!     &         (i_step, SGS_par, wk_sgs, wk_diff)
!!        type(SGS_paremeters), intent(in) :: SGS_par
!!        type(dynamic_model_data), intent(in) :: wk_sgs, wk_diff
!!
!!      subroutine read_sgs_layerd_data(file_id, iflag, n_layer,        &
!!     &          n_comp, coef)
!!      subroutine read_sgs_whole_data(file_id, iflag, n_comp, coef)
!
      module sgs_model_coefs_IO
!
      use m_precision
!
      use calypso_mpi
      use m_t_step_parameter
      use t_physical_property
      use t_SGS_control_parameter
      use t_ele_info_4_dynamic
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
      subroutine s_output_sgs_model_coefs                               &
     &         (i_step, SGS_par, wk_sgs, wk_diff)
!
      use m_physical_property
      use t_IO_step_parameter
!
      integer(kind = kint), intent(in) :: i_step
      type(SGS_paremeters), intent(in) :: SGS_par
      type(dynamic_model_data), intent(in) :: wk_sgs, wk_diff
!
!
      if(SGS_par%model_p%iflag_dynamic .eq. id_SGS_DYNAMIC_OFF) return
      if(output_flag(i_step,SGS_par%sgs_step%increment) .ne. 0) return
      if(my_rank .ne. 0) return
!
      call output_layered_model_coefs_file                              &
     &   (SGS_par%model_p, cd_prop1, wk_sgs)
      call output_whole_model_coefs_file                                &
     &   (SGS_par%model_p, cd_prop1, wk_sgs)
!
      if (SGS_par%commute_p%iflag_commute .gt. id_SGS_commute_OFF) then
        call output_whole_diff_coefs_file(cd_prop1, wk_diff)
!
        if (SGS_par%commute_p%iset_DIFF_coefs .eq. 1 ) then
          call output_layered_diff_coefs_file(cd_prop1, wk_diff)
        end if
      end if
!
      end subroutine s_output_sgs_model_coefs
!
!-----------------------------------------------------------------------
!
      subroutine output_layered_model_coefs_file                        &
     &         (SGS_param, cd_prop, wk_sgs)
!
      type(SGS_model_control_params), intent(in) :: SGS_param
      type(conductive_property), intent(in) :: cd_prop
      type(dynamic_model_data), intent(in) :: wk_sgs
!
      integer (kind = kint) :: inum
!
!
      call open_SGS_model_coef_file(iflag_layered,                      &
     &    sgs_fld_coef_file_code, sgs_fld_coef_file_name, wk_sgs)
      call open_SGS_correlation_file(iflag_layered,                     &
     &    sgs_comp_coef_file_code, sgs_comp_coef_file_name,             &
     &    cd_prop, SGS_param, wk_sgs)
!
      call open_SGS_correlation_file(iflag_layered,                     &
     &    sgs_cor_file_code, sgs_cor_file_name,                         &
     &    cd_prop, SGS_param, wk_sgs)
      call open_SGS_correlation_file(iflag_layered,                     &
     &    sgs_cov_file_code, sgs_cov_file_name,                         &
     &    cd_prop, SGS_param, wk_sgs)
      call open_SGS_correlation_file(iflag_layered,                     &
     &    sgs_ratio_file_code, sgs_ratio_file_name,                     &
     &    cd_prop, SGS_param, wk_sgs)
!
      call open_SGS_rms_ratio_file(iflag_layered,                       &
     &    sgs_rms_file_code, sgs_rms_file_name,                         &
     &    cd_prop, SGS_param, wk_sgs)
!
!
      do inum = 1, wk_sgs%nlayer
        write(sgs_fld_coef_file_code,1000) i_step_MHD,                  &
     &      time, inum, wk_sgs%fld_clip(inum,1:wk_sgs%num_kinds)
        write(sgs_comp_coef_file_code,1000) i_step_MHD,                 &
     &      time, inum, wk_sgs%comp_clip(inum,1:wk_sgs%ntot_comp)
!
        write(sgs_cor_file_code,1000) i_step_MHD, time, inum,           &
     &         wk_sgs%corrilate(inum,1:wk_sgs%ntot_comp)
        write(sgs_cov_file_code,1000) i_step_MHD, time, inum,           &
     &         wk_sgs%covariant(inum,1:wk_sgs%ntot_comp)
        write(sgs_ratio_file_code,1000) i_step_MHD, time, inum,         &
     &         wk_sgs%ratio(inum,1:wk_sgs%ntot_comp)
!
        write(sgs_rms_file_code,1000) i_step_MHD, time, inum,           &
     &         wk_sgs%rms_simi(inum,1:wk_sgs%ntot_comp),                &
     &         wk_sgs%rms_grad(inum,1:wk_sgs%ntot_comp)
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
 1000 format(i16,1pE25.15e3,i16,1p200E25.15e3)
!
      end subroutine output_layered_model_coefs_file
!
!-----------------------------------------------------------------------
!
      subroutine output_whole_model_coefs_file                          &
     &         (SGS_param, cd_prop, wk_sgs)
!
      type(SGS_model_control_params), intent(in) :: SGS_param
      type(conductive_property), intent(in) :: cd_prop
      type(dynamic_model_data), intent(in) :: wk_sgs
!
!
      call open_SGS_model_coef_file(iflag_whole,                        &
     &    sgs_fld_coef_file_code, sgs_fld_whole_file_name, wk_sgs)
      call open_SGS_correlation_file(iflag_whole,                       &
     &    sgs_comp_coef_file_code, sgs_comp_whole_file_name,            &
     &    cd_prop, SGS_param, wk_sgs)
!
      call open_SGS_correlation_file(iflag_whole,                       &
     &    sgs_cor_file_code, sgs_w_cor_file_name,                       &
     &    cd_prop, SGS_param, wk_sgs)
      call open_SGS_correlation_file(iflag_whole,                       &
     &    sgs_cov_file_code, sgs_w_cov_file_name,                       &
     &    cd_prop, SGS_param, wk_sgs)
      call open_SGS_correlation_file(iflag_whole,                       &
     &    sgs_ratio_file_code, sgs_w_ratio_file_name,                   &
     &    cd_prop, SGS_param, wk_sgs)
!
      call open_SGS_rms_ratio_file(iflag_whole,                         &
     &    sgs_rms_file_code, sgs_w_rms_file_name,                       &
     &    cd_prop, SGS_param, wk_sgs)
!
      write(sgs_fld_coef_file_code,1001)  i_step_MHD, time,             &
     &        wk_sgs%fld_whole_clip(1:wk_sgs%num_kinds)
      write(sgs_comp_coef_file_code,1001)  i_step_MHD, time,            &
     &        wk_sgs%comp_whole_clip(1:wk_sgs%ntot_comp)
!
      write(sgs_cor_file_code,1001)  i_step_MHD, time,                  &
     &        wk_sgs%corrilate_w(1:wk_sgs%ntot_comp)
      write(sgs_cov_file_code,1001)  i_step_MHD, time,                  &
     &        wk_sgs%covariant_w(1:wk_sgs%ntot_comp)
      write(sgs_ratio_file_code,1001) i_step_MHD, time,                 &
     &        wk_sgs%ratio_w(1:wk_sgs%ntot_comp)
      write(sgs_rms_file_code,1001) i_step_MHD, time,                   &
     &        wk_sgs%rms_simi_w(1:wk_sgs%ntot_comp),                    &
     &        wk_sgs%rms_grad_w(1:wk_sgs%ntot_comp)
!
      close (sgs_fld_coef_file_code)
      close (sgs_comp_coef_file_code)
      close (sgs_cor_file_code)
      close (sgs_cov_file_code)
      close (sgs_ratio_file_code)
      close (sgs_rms_file_code)
!
 1001 format(i16,1pE25.15e3,1p200E25.15e3)
!
      end subroutine output_whole_model_coefs_file
!
!-----------------------------------------------------------------------
!
      subroutine output_whole_diff_coefs_file(cd_prop, wk_diff)
!
      type(conductive_property), intent(in) :: cd_prop
      type(dynamic_model_data), intent(in) :: wk_diff
!
!
      call open_SGS_diff_coef_file(iflag_whole,                         &
     &      diff_coef_file_code, diff_fld_whole_file_name, wk_diff)
      call open_SGS_diff_coef_file(iflag_whole,                         &
     &      diff_comp_file_code, diff_comp_whole_file_name, wk_diff)
!
      call open_diff_correlation_file(iflag_whole,                      &
     &   diff_cor_file_code, diff_w_cor_file_name, cd_prop, wk_diff)
      call open_diff_correlation_file(iflag_whole,                      &
     &   diff_cov_file_code, diff_w_cov_file_name, cd_prop, wk_diff)
      call open_diff_correlation_file(iflag_whole,                      &
     &   diff_ratio_file_code, diff_w_ratio_file_name,                  &
     &   cd_prop, wk_diff)
!
      call open_diff_rms_ratio_file(iflag_whole,                        &
     &   diff_rms_file_code, diff_w_rms_file_name, cd_prop, wk_diff)
!
      write(diff_coef_file_code,1001) i_step_MHD, time,                 &
     &          wk_diff%fld_whole_clip(1:wk_diff%num_kinds)
      write(diff_comp_file_code,1001) i_step_MHD, time,                 &
     &          wk_diff%comp_whole_clip(1:wk_diff%ntot_comp)
!
      write(diff_cor_file_code,1001) i_step_MHD, time,                  &
     &          wk_diff%corrilate_w(1:wk_diff%ntot_comp)
      write(diff_cov_file_code,1001) i_step_MHD, time,                  &
     &          wk_diff%covariant_w(1:wk_diff%ntot_comp)
      write(diff_ratio_file_code,1001) i_step_MHD, time,                &
     &          wk_diff%ratio_w(1:wk_diff%ntot_comp)
      write(diff_rms_file_code,1001) i_step_MHD, time,                  &
     &          wk_diff%rms_simi_w(1:wk_diff%ntot_comp),                &
     &          wk_diff%rms_grad_w(1:wk_diff%ntot_comp)
!
      close (diff_coef_file_code)
      close (diff_comp_file_code)
      close (diff_cor_file_code)
      close (diff_cov_file_code)
      close (diff_ratio_file_code)
      close (diff_rms_file_code)
!
 1001 format(i16,1pE25.15e3,1p200E25.15e3)
!
      end subroutine output_whole_diff_coefs_file
!
!-----------------------------------------------------------------------
!
      subroutine output_layered_diff_coefs_file(cd_prop, wk_diff)
!
      type(conductive_property), intent(in) :: cd_prop
      type(dynamic_model_data), intent(in) :: wk_diff
!
      integer (kind = kint) :: inum
!
!
      call open_SGS_diff_coef_file(iflag_layered,                       &
     &             diff_coef_file_code, diff_coef_file_name, wk_diff)
!
      call open_diff_correlation_file(iflag_layered,                    &
     &    diff_cor_file_code, diff_cor_file_name, cd_prop, wk_diff)
      call open_diff_correlation_file(iflag_layered,                    &
     &    diff_cov_file_code, diff_cov_file_name, cd_prop, wk_diff)
      call open_diff_correlation_file(iflag_layered,                    &
     &    diff_ratio_file_code, diff_ratio_file_name,                   &
     &    cd_prop, wk_diff)
!
      call open_diff_rms_ratio_file(iflag_layered,                      &
     &    diff_rms_file_code, diff_rms_file_name, cd_prop, wk_diff)
!
      do inum = 1, wk_diff%nlayer
        write(diff_coef_file_code,1000)                                 &
     &       i_step_MHD, time, inum,                                    &
     &              wk_diff%fld_clip(inum,1:wk_diff%num_kinds)
!
        write(diff_cor_file_code,1000)                                  &
     &       i_step_MHD, time, inum,                                    &
     &              wk_diff%corrilate(inum,1:wk_diff%ntot_comp)
        write(diff_cov_file_code,1000)                                  &
     &       i_step_MHD, time, inum,                                    &
     &              wk_diff%covariant(inum,1:wk_diff%ntot_comp)
!
        write(diff_ratio_file_code,1000)                                &
     &       i_step_MHD, time, inum,                                    &
     &              wk_diff%ratio(inum,1:wk_diff%ntot_comp)
        write(diff_rms_file_code,1000)                                  &
     &      i_step_MHD, time, inum,                                     &
     &              wk_diff%rms_simi(inum,1:wk_diff%ntot_comp),         &
     &              wk_diff%rms_grad(inum,1:wk_diff%ntot_comp)
      end do
!
      close(diff_coef_file_code)
      close(diff_cor_file_code)
      close(diff_cov_file_code)
      close(diff_ratio_file_code)
      close(diff_rms_file_code)
!
 1000 format(i16,1pE25.15e3,i16,1p200E25.15e3)
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
